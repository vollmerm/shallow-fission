{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.CUDA         as C
import           Data.Array.Accelerate.Data.Complex
import qualified Fission1                           as F
import           Prelude                            as P
import           System.Environment

test =
  let size      = 800
      limit     = 255
      view      = (-2.23, -1.15, 0.83, 1.15) :: View Float
      force arr = indexArray arr (Z:.0:.0) `seq` arr
      compute (size,limit) = do
        arr <- mandelbrot size size limit $ A.use $ A.fromList Z [view]
        return arr
  in compute (size,limit)

main = do
  n' <- getEnv "N"
  b' <- getEnv "BACKEND"
  let size      = read n' :: Int
      limit     = 255
      view      = (-2.23, -1.15, 0.83, 1.15) :: View Float
      force arr = indexArray arr (Z:.0:.0) `seq` arr
      compute (size,limit) = do
        arr <- mandelbrot size size limit $ A.use $ A.fromList Z [view]
        return $ force $ C.runMulti $ F.combine arr
      compute' (size,limit) =
          let arr = mandelbrot' size size limit $ A.use $ A.fromList Z [view]
          in force $ C.run arr
  if b' == "multi"
  then defaultMain
           [bgroup "Mandel" [ bench ("multi: n = " P.++ (show size)) $ whnf compute (size,limit)]]
  else defaultMain
           [bgroup "Mandel" [ bench ("normal: n = " P.++ (show size)) $ whnf compute' (size,limit)]]



-- Types -----------------------------------------------------------------------

type RGBA32             = Word32

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
    => Int
    -> Int
    -> Int
    -> Acc (Scalar (View a))
    -> F.TuneM (F.Acc (Array DIM2 Int32))
mandelbrot screenX screenY depth view =
  F.generateSplit (constant (Z:.screenY:.screenX))
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
  generate (constant (Z:.screenY:.screenX))
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

