{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE ParallelListComp          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

import Data.Array.Accelerate                        as A hiding ( Split )
import qualified Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native            as A
import qualified Data.Array.Accelerate.LLVM.PTX     as PTX
import Prelude                                      as P
import Data.Array.Accelerate.IO                     as A
import Data.Array.Accelerate.Data.Complex
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Parallel
import Data.Monoid
import Data.Typeable
import System.Random.MWC                            as MWC
import Text.Printf
import System.Environment
import Data.Array.Accelerate.Fission
import Criterion.Main
import Data.Array.Accelerate.System.Random.SFMT     as A

mandel1 width height limit n =  
  let view = (-2.23, -1.15, 0.83, 1.15) :: View Float
  --     comp = \_ -> mandelbrot n height width limit (A.use $ A.fromList Z [view])
  -- in stune $ schedulen comp (fromList Z [()]) n
  in mandelbrot n height width limit $ A.use $ A.fromList Z [view]

main = do
  sizeStr <- getEnv "SIZE"
  splitStr <- getEnv "SPLIT"
  let size  = read sizeStr :: Int
      limit = 256
      split = read splitStr :: Double
      m1    = stune $ schedule $ mandel1 size size limit split
      m1'   = exec1 m1 True
      view  = (-2.23, -1.15, 0.83, 1.15) :: View Float
      -- m0'   = A.run1 $ \_ -> mandelbrot' size size limit $ A.use $ A.fromList Z [view]
      -- m2'   = PTX.run1 $ \_ -> mandelbrot' size size limit $ A.use $ A.fromList Z [view]
  defaultMain
    [
    bench ("CPU+GPU " P.++ (show size) P.++ " " P.++ (show limit) P.++ " " P.++ (show split)) $ whnf eval m1'
  --  ,
    -- bench ("CPU " P.++ (show size) P.++ " " P.++ (show limit) P.++ " " P.++ (show split)) $ whnf m0' $ A.fromList Z [()]
    -- , bench ("GPU " P.++ (show size) P.++ " " P.++ (show limit) P.++ " " P.++ (show split)) $ whnf m2' $ A.fromList Z [()]      
    ]

-- Types -----------------------------------------------------------------------

-- Current view into the complex plane
type View a             = (a, a, a, a)

-- Image data
type Bitmap             = Array DIM2 RGBA32

-- Action to render a frame
type Render a           = Scalar (View a) -> Bitmap


-- Mandelbrot Set --------------------------------------------------------------

-- Compute the mandelbrot as repeated application of the recurrence relation:
--
--   Z_{n+1} = c + Z_n^2
--
-- This returns the iteration depth 'i' at divergence.
--
mandelbrot
    :: forall a. (Elt a, IsFloating a)
    => Double
    -> Int
    -> Int
    -> Int
    -> Acc (Scalar (View a))
    -> Arr (Acc (Array DIM2 Int32))
mandelbrot n screenX screenY depth view =
  pgeneraten n (constant (Z:.screenY:.screenX))
     (\ix -> let c = initial ix
             in  A.snd $ A.while (\zi -> A.snd zi A.<* lIMIT &&* dot (A.fst zi) A.<* 4)
                     (\zi -> lift1 (next c) zi)
                     (lift (c, constant 0)))
  where
    -- The view plane
    (xmin,ymin,xmax,ymax)     = unlift (the view)
    sizex                     = xmax - xmin
    sizey                     = ymax - ymin

    viewx                     = constant (P.fromIntegral screenX)
    viewy                     = constant (P.fromIntegral screenY)

    -- initial conditions for a given pixel in the window, translated to the
    -- corresponding point in the complex plane
    initial :: Exp DIM2 -> Exp (Complex a)
    initial ix = lift ( (xmin + (x * sizex) / viewx) :+ (ymin + (y * sizey) / viewy) )
      where
        pr = unindex2 ix
        x  = A.fromIntegral (A.snd pr :: Exp Int)
        y  = A.fromIntegral (A.fst pr :: Exp Int)

    -- take a single step of the iteration
    next :: Exp (Complex a) -> (Exp (Complex a), Exp Int32) -> (Exp (Complex a), Exp Int32)
    next c (z, i) = (c + (z * z), i+1)

    dot c = let r :+ i = unlift c
            in  r*r + i*i

    lIMIT = P.fromIntegral depth


mandelbrot'
    :: forall a. (Elt a, IsFloating a)
    => Int
    -> Int
    -> Int
    -> Acc (Scalar (View a))
    -> Acc (Array DIM2 Int32)
mandelbrot' screenX screenY depth view =
  A.generate (constant (Z:.screenY:.screenX))
     (\ix -> let c = initial ix
             in  A.snd $ A.while (\zi -> A.snd zi A.<* lIMIT &&* dot (A.fst zi) A.<* 4)
                     (\zi -> lift1 (next c) zi)
                     (lift (c, constant 0)))
  where
    -- The view plane
    (xmin,ymin,xmax,ymax)     = unlift (the view)
    sizex                     = xmax - xmin
    sizey                     = ymax - ymin

    viewx                     = constant (P.fromIntegral screenX)
    viewy                     = constant (P.fromIntegral screenY)

    -- initial conditions for a given pixel in the window, translated to the
    -- corresponding point in the complex plane
    initial :: Exp DIM2 -> Exp (Complex a)
    initial ix = lift ( (xmin + (x * sizex) / viewx) :+ (ymin + (y * sizey) / viewy) )
      where
        pr = unindex2 ix
        x  = A.fromIntegral (A.snd pr :: Exp Int)
        y  = A.fromIntegral (A.fst pr :: Exp Int)

    -- take a single step of the iteration
    next :: Exp (Complex a) -> (Exp (Complex a), Exp Int32) -> (Exp (Complex a), Exp Int32)
    next c (z, i) = (c + (z * z), i+1)

    dot c = let r :+ i = unlift c
            in  r*r + i*i

    lIMIT = P.fromIntegral depth

