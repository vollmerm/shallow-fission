{-# LANGUAGE ParallelListComp #-}

module Main where

import Mandel

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Debug                                  ( accInit )
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

maybeEnv :: Read a => String -> a -> IO a
maybeEnv var def = do
  ms <- lookupEnv var
  case ms of
    Just s | [(v,[])] <- reads s -> return v
    _                            -> return def

main :: IO ()
main = do
  accInit

  width   <- maybeEnv "WIDTH"  2048
  height  <- maybeEnv "HEIGHT" 1536
  limit   <- maybeEnv "LIMIT"  512
  pin     <- maybeEnv "PINNED" False

  printf "Mandelbrot:\n"
  printf "  width:               %d\n" width
  printf "  height:              %d\n" height
  printf "  number of threads:   %d\n" =<< getNumCapabilities
  printf "  using pinned memory: %s\n" (show pin)
  printf "\n"

  let render :: Double -> Acc (Scalar (View Double)) -> Acc Bitmap
      render radius = A.map (prettyRGBA (P.fromIntegral limit))
                    . mandelbrot width height limit radius

  when pin $
    registerForeignPtrAllocator (CUDA.mallocHostForeignPtr [])

  defaultMain
    [ bgroup "cuda"       [ bench (printf "%02d" n) $ whnf (CUDA.run1  (render radius)) (fromList Z [view]) | (view,radius) <- table | n <- [(1::Int)..] ]
    , bgroup "llvm-cpu"   [ bench (printf "%02d" n) $ whnf (CPU.run1   (render radius)) (fromList Z [view]) | (view,radius) <- table | n <- [(1::Int)..] ]
    , bgroup "llvm-ptx"   [ bench (printf "%02d" n) $ whnf (PTX.run1   (render radius)) (fromList Z [view]) | (view,radius) <- table | n <- [(1::Int)..] ]
    , bgroup "llvm-multi" [ bench (printf "%02d" n) $ whnf (Multi.run1 (render radius)) (fromList Z [view]) | (view,radius) <- table | n <- [(1::Int)..] ]
    ]


table :: [ ((Double, Double, Double, Double), Double) ]
table =
    [ ((-2.23,                  -1.15,                           0.83,                   1.15),                2.0)
    , ((-0.7,                   0,                               3.067,                  100.0),               2.0)
    , ((0.20508818500545423,    0.9014915666351141   * 900/1440, 6.375321937544527e-6,   629.3354966759534),   16.0)
    , ((0.4510757067879078,     0.6144133202705898   * 900/1440, 7.632248223018773e-5,   253.61352386150395),  2.0)
    , ((0.3469337523117071,     0.6866350870407725   * 900/1440, 3.508380713647269e-5,   168.61054759193718),  1024.0)
    , ((-0.7902001921590814,    0.24910667566731381  * 900/1440, 5.071115028132377e-4,   1176.757810813391),   3.4359738368e10)
    , ((2.3127178455019423e-2, -1.301205470975472    * 900/1440, 3.6349313304610088e-9,  343.0390372557315),   2.0)
    , ((2.3127176148480418e-2, -1.3012054707668765   * 900/1440, 2.71444790387451e-10,   604.1620768089155),   2.0)
    , ((2.3127176156746785e-2, -1.301205470242045    * 900/1440, 4.49615119202067e-12,   1731.8575629678642),  2.0)
    , ((0.2550376327692795,     8.962363618058007e-4 * 900/1440, 7.351698819132829e-5,   1412.1093729760698),  16.0)
    , ((0.25498593633806477,    8.726424280526077e-4 * 900/1440, 1.6858526052251987e-10, 10492.090844482025),  2.0)
    ]

