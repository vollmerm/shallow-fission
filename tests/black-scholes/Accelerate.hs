{-# LANGUAGE LambdaCase #-}

module Main where

import BlackScholes
import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Data                             as A
import Data.Array.Accelerate.CUDA                                   as CUDA
import Data.Array.Accelerate.LLVM.Multi                             as Multi
import Data.Array.Accelerate.LLVM.Native                            as CPU
import Data.Array.Accelerate.LLVM.PTX                               as PTX

import Foreign.CUDA.Driver                                          as CUDA

import Criterion.Main

import Control.Monad
import System.Environment
import Text.Printf
import Prelude                                                      as P

import GHC.Conc


main :: IO ()
main = do
  n   <- flip fmap (lookupEnv "N") $ \case
           Just s | [(v,[])] <- reads s -> v
           _                            -> 20000000

  pin <- flip fmap (lookupEnv "PINNED") $ \case
           Just []                      -> True
           Just s | [(v,[])] <- reads s -> v
           _                            -> False

  printf "BlackScholes:\n"
  printf "  number of options:   %d\n" n
  printf "  number of threads:   %d\n" =<< getNumCapabilities
  printf "  using pinned memory: %s\n" (show pin)
  printf "\n"

  when pin $
    registerForeignPtrAllocator (CUDA.mallocHostForeignPtr [])

  opts_f32 <- mkData n :: IO (Vector (Float,Float,Float))
  opts_f64 <- mkData n :: IO (Vector (Double,Double,Double))

  defaultMain
    [ bgroup "float"
      [ bench "cuda"       $ whnf (CUDA.run1 blackscholes)  opts_f32
      , bench "llvm-cpu"   $ whnf (CPU.run1 blackscholes)   opts_f32
      , bench "llvm-ptx"   $ whnf (PTX.run1 blackscholes)   opts_f32
      , bench "llvm-multi" $ whnf (Multi.run1 blackscholes) opts_f32
      ]
    , bgroup "double"
      [ bench "cuda"       $ whnf (CUDA.run1 blackscholes)  opts_f64
      , bench "llvm-cpu"   $ whnf (CPU.run1 blackscholes)   opts_f64
      , bench "llvm-ptx"   $ whnf (PTX.run1 blackscholes)   opts_f64
      , bench "llvm-multi" $ whnf (Multi.run1 blackscholes) opts_f64
      ]
    ]

