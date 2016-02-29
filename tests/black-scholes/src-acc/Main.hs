{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import BlackScholes

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Data                             as A
import Data.Array.Accelerate.Array.Sugar                            as S
import Data.Array.Accelerate.CUDA                                   as CUDA
import Data.Array.Accelerate.LLVM.Multi                             as Multi
import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.LLVM.PTX                               as PTX
import Data.Array.Accelerate.Debug                                  ( accInit )

import Foreign.CUDA.Driver                                          as F

import Criterion.Main

import Control.Monad
import System.CPUTime
import System.Environment
import System.IO
import System.IO.Unsafe
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

  -- Create a couple CUDA contexts for the CUDA and PTX backends. Sadly these
  -- can not be shared.
  F.initialise []
  ngpu  <- F.count
  devs  <- mapM F.device [0 .. ngpu-1]
  prps  <- mapM F.props devs
  cc    <- mapM     (\dev     -> CUDA.create               dev     []) devs
  pc    <- zipWithM (\dev prp -> PTX.createTargetForDevice dev prp []) devs prps

  n     <- maybeEnv "N" 20000000
  pin   <- maybeEnv "PINNED" False

  printf "BlackScholes:\n"
  printf "  number of options:   %d\n" n
  printf "  number of threads:   %d\n" =<< getNumCapabilities
  printf "  number of GPUs:      %d\n" ngpu
  printf "  using pinned memory: %s\n" (show pin)
  printf "\n"

  -- Requires a CUDA _context_ to be initialised, which is a little odd...
  when pin $ do
    registerForeignPtrAllocator (F.mallocHostForeignPtr [])

  -- Generate random numbers. This can take a while...
  printf "generating data... "
  hFlush stdout

  t1        <- getCPUTime
  -- !opts_f32 <- mkData n :: IO (Vector (Float,Float,Float))
  !opts_f64 <- mkData n :: IO (Vector (Double,Double,Double))
  t2        <- getCPUTime

  printf "done! (%.2fs)\n" (P.fromIntegral (t2-t1) * 1.0e-12 :: Double)
  printf "\n"

  -- Grab the default context for each GPU backend. The list will not be empty
  -- (c.f. head) because if there are no devices, initialising CUDA would
  -- already have failed. Assume that device 0 is the "best" device.
  --
  let c0              = head cc
      p0              = head pc
      -- gpugpu_opts_f32 = split ngpu opts_f32
      -- cpugpu_opts_f32 = split (ngpu + 1) opts_f32
      gpugpu_opts_f64 = split ngpu opts_f64
      cpugpu_opts_f64 = split (ngpu + 1) opts_f64

  defaultMain
    -- [ bgroup "float"
    --   [ bench "cuda"       $ whnf (CUDA.run1With c0 blackscholes) opts_f32
    --   , bench "llvm-ptx"   $ whnf (PTX.run1With p0 blackscholes)  opts_f32
    --   , bench "llvm-cpu"   $ whnf (CPU.run1 blackscholes)         opts_f32
    --   , bench "llvm-multi" $ whnf (Multi.run1 blackscholes)       opts_f32
    --   , bgroup "manual-split"
    --     $ bench "llvm-cpu-ptx" (whnf (async ( CPU.run1Async blackscholes : [ PTX.run1AsyncWith ptx blackscholes | ptx <- pc ])) cpugpu_opts_f32)
    --     : if ngpu > 1
    --         then [ bench "cuda-cuda"    $ whnf (async [ CUDA.run1AsyncWith ctx blackscholes | ctx <- cc ]) gpugpu_opts_f32
    --              , bench "llvm-ptx-ptx" $ whnf (async [ PTX.run1AsyncWith  ptx blackscholes | ptx <- pc ]) gpugpu_opts_f32
    --              ]
    --         else []
    --   ]
    -- , bgroup "double"
      [ bench "cuda"       $ whnf (CUDA.run1With c0 blackscholes) opts_f64
      , bench "llvm-ptx"   $ whnf (PTX.run1With p0 blackscholes)  opts_f64
      , bench "llvm-cpu"   $ whnf (CPU.run1 blackscholes)         opts_f64
      , bench "llvm-multi" $ whnf (Multi.run1 blackscholes)       opts_f64
      , bgroup "manual-split"
        $ bench "llvm-cpu-ptx" (whnf (async ( CPU.run1Async blackscholes : [ PTX.run1AsyncWith ptx blackscholes | ptx <- pc ])) cpugpu_opts_f64)
        : if ngpu > 1
            then [ bench "cuda-cuda"    $ whnf (async [ CUDA.run1AsyncWith ctx blackscholes | ctx <- cc ]) gpugpu_opts_f64
                 , bench "llvm-ptx-ptx" $ whnf (async [ PTX.run1AsyncWith  ptx blackscholes | ptx <- pc ]) gpugpu_opts_f64
                 ]
            else []
      ]
    -- ]


async :: [a -> IO (Async b)] -> [a] -> ()
async fs xs = unsafePerformIO $! do
  as <- sequence $ P.zipWith ($) fs xs
  () <- mapM_ wait as
  return ()


split :: Elt e => Int -> Vector e -> [Vector e]
split pieces arr =
  [ range from to | from <- splitPts
                  | to   <- P.tail splitPts
  ]
  where
    Z :. n        = arrayShape arr
    chunk         = n `quotInt` pieces
    leftover      = n `remInt`  pieces

    splitPts      = P.map splitIx [0 .. pieces]
    splitIx i
      | i < leftover  = i * (chunk + 1)
      | otherwise     = i * chunk + leftover

    range from to =
      A.fromFunction
          (Z :. to - from)
          (\(Z :. i) -> arr S.! (Z :. i+from))

