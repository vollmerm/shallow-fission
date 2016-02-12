{-# LANGUAGE BangPatterns #-}

module Main where

import BlackScholes

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Array.Data                             as A
import Data.Array.Accelerate.CUDA                                   as CUDA  ( run1 )
import Data.Array.Accelerate.LLVM.Multi                             as Multi ( run1 )
import Data.Array.Accelerate.LLVM.Native                            as CPU   ( run1 )
import Data.Array.Accelerate.LLVM.PTX                               as PTX   ( run1 )

import Foreign.CUDA.Driver                                          as CUDA

import Criterion.Main

import Control.Monad
import System.Environment
import System.IO
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
  n   <- maybeEnv "N" 20000000
  pin <- maybeEnv "PINNED" False

  printf "BlackScholes:\n"
  printf "  number of options:   %d\n" n
  printf "  number of threads:   %d\n" =<< getNumCapabilities
  printf "  using pinned memory: %s\n" (show pin)
  printf "\n"

  when pin $ do
    CUDA.initialise []
    dev <- CUDA.device 0
    ctx <- CUDA.create dev []
    registerForeignPtrAllocator (CUDA.mallocHostForeignPtr [])

  printf "generating data... "
  hFlush stdout

  !opts_f32 <- mkData n :: IO (Vector (Float,Float,Float))
  !opts_f64 <- mkData n :: IO (Vector (Double,Double,Double))

  printf "done!\n"

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

