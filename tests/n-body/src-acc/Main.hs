{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import Common.Body
import Common.World
import Random.Position
import Solver

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Data                             as A
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.System.Random.SFMT

import Data.Array.Accelerate.Interpreter                            as I
import Data.Array.Accelerate.CUDA                                   as CUDA
import Data.Array.Accelerate.LLVM.Multi                             as Multi
import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.LLVM.PTX                               as PTX
import Data.Array.Accelerate.Debug                                  ( accInit )

import Foreign.CUDA.Driver                                          as F

import Criterion.Main

import Control.Monad
import System.Environment
import System.IO
import System.IO.Unsafe
import System.CPUTime
import Text.Printf
import Prelude                                                      as P

import GHC.Conc
import GHC.Base                                                     ( quotInt, remInt )


maybeEnv :: Read a => String -> a -> IO a
maybeEnv var def = do
  ms <- lookupEnv var
  case ms of
    Just s | [(v,[])] <- reads s -> return v
    _                            -> return def

main :: IO ()
main = do
  accInit

  -- Create the contexts for the CUDA and PTX backends. Sadly, the underlying
  -- (raw) contexts can not be shared with the current API.
  --
  F.initialise []
  ngpu    <- F.count
  devs    <- mapM F.device [0 .. ngpu - 1]
  prps    <- mapM F.props devs
  cc    <- mapM     (\dev     -> CUDA.create      dev     []) devs
  pc    <- zipWithM (\dev prp -> PTX.createTarget dev prp []) devs prps

  n       <- maybeEnv "N" 10000
  pin     <- maybeEnv "PINNED" False

  printf "N-body simulation:\n"
  printf "  bodies:              %d\n" n
  printf "  number of threads:   %d\n" =<< getNumCapabilities
  printf "  number of GPUs:      %d\n" ngpu
  printf "  using pinned memory: %s\n" (show pin)
  printf "\n"

  when pin $ do
    registerForeignPtrAllocator (F.mallocHostForeignPtr [])

  let mMax    = 40
      rMax    = 500
      epsilon = 50
      extent  = 1000
      speed   = 1

  printf "generating data... "
  hFlush stdout

  t1          <- getCPUTime
  !positions  <- randomArray (cloud (extent,extent) rMax) (Z :. n)
  !masses     <- randomArray (uniformR (1,mMax)) (Z :. n)

  let !bodies       = I.run
                    $ A.map (setStartVelOfBody speed)
                    $ A.zipWith setMassOfBody (A.use masses)
                    $ A.map unitBody (A.use positions)

      dt            = fromList Z [0.1]
      advance       = advanceBodies (calcAccels epsilon) (use dt)
      gpugpu_bodies = split ngpu       bodies
      cpugpu_bodies = split (ngpu + 1) bodies

      p0            = head pc
      c0            = head cc

  t2          <- bodies `seq` getCPUTime

  printf "done! (%.2fs)\n" (P.fromIntegral (t2-t1) * 1.0e-12 :: Double)
  printf "\n"

  defaultMain
    [ bench "cuda"       $ whnf (CUDA.run1With c0 advance) bodies
    , bench "llvm-ptx"   $ whnf (PTX.run1With  p0 advance) bodies
    , bench "llvm-cpu"   $ whnf (CPU.run1   advance) bodies
    , bench "llvm-multi" $ whnf (Multi.run1 advance) bodies
    , bgroup "manual-split"
      $ bench "llvm-cpu-ptx" (whnf (async ( CPU.run1Async advance : [ PTX.run1AsyncWith ptx advance | ptx <- pc ])) cpugpu_bodies)
      : if ngpu > 1
          then [ bench "cuda-cuda"    $ whnf (async [ CUDA.run1AsyncWith ctx advance | ctx <- cc ]) gpugpu_bodies
               , bench "llvm-ptx-ptx" $ whnf (async [ PTX.run1AsyncWith  ptx advance | ptx <- pc ]) gpugpu_bodies
               ]
          else []
    ]


async :: [a -> IO (Async b)] -> [a] -> ()
async fs xs = unsafePerformIO $! do
  as <- sequence $ P.zipWith ($) fs xs
  () <- mapM_ wait as
  return ()

split :: Elt e => Int -> Vector e -> [Vector e]
split 1      arr = [ arr ]
split pieces arr = [ range from to | from <- splitPts | to <- P.tail splitPts ]
  where
    Z :. n        = arrayShape arr
    chunk         = n `quotInt` pieces
    leftover      = n `remInt`  pieces

    splitPts      = P.map splitIx [0 .. pieces]
    splitIx i
      | i < leftover  = i * (chunk + 1)
      | otherwise     = i * chunk + leftover

    range from to = A.fromFunction
                        (Z :. to - from)
                        (\(Z :. i) -> arr S.! (Z :. i+from))

