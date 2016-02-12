
module Main where

import Common.Body
import Common.World
import Random.Position
import Solver

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Data                             as A
import Data.Array.Accelerate.System.Random.SFMT

import Data.Array.Accelerate.Interpreter                            as I
import Data.Array.Accelerate.CUDA                                   as CUDA ( run1 )
import Data.Array.Accelerate.LLVM.Multi                             as Multi
import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.LLVM.PTX                               as PTX

import Foreign.CUDA.Driver                                          as CUDA
import Foreign.CUDA.Driver.Profiler                                 as Prof

import Criterion.Main

import Control.Monad
import System.Environment
import Text.Printf
import Prelude                                                      as P

import GHC.Conc

maybeEnv :: Read a => String -> a -> IO a
maybeEnv var def = do
  ms <- lookupEnv var
  case ms of
    Just s | [(v,[])] <- reads s -> return v
    _                            -> return def

main :: IO ()
main = do
  n       <- maybeEnv "N" 1000
  pin     <- maybeEnv "PINNED" False

  printf "N-body simulation:\n"
  printf "  bodies:              %d\n" n
  printf "  number of threads:   %d\n" =<< getNumCapabilities
  printf "  using pinned memory: %s\n" (show pin)
  printf "\n"

  when pin $ do
    CUDA.initialise []
    dev <- CUDA.device 0
    ctx <- CUDA.create dev []
    registerForeignPtrAllocator (CUDA.mallocHostForeignPtr [])

  let mMax    = 40
      rMax    = 500
      epsilon = 50
      extent  = 1000
      speed   = 1

  positions <- randomArray (cloud (extent,extent) rMax) (Z :. n)
  masses    <- randomArray (uniformR (1,mMax)) (Z :. n)

  let bodies  = I.run
              $ A.map (setStartVelOfBody speed)
              $ A.zipWith setMassOfBody (A.use masses)
              $ A.map unitBody (A.use positions)

      dt      = fromList Z [0.1]
      advance = advanceBodies (calcAccels epsilon) (use dt)

  defaultMain
    [ bench "cuda"       $ whnf (CUDA.run1 advance) bodies
    , bench "llvm-cpu"   $ whnf (CPU.run1 advance) bodies
    , bench "llvm-ptx"   $ whnf (PTX.run1 advance) bodies
    , bench "llvm-multi" $ whnf (Multi.run1 advance) bodies
    ]

