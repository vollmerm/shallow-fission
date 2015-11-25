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
        arr <- mandelbrot size size limit F.Even $ A.use $ A.fromList Z [view]
        return arr
  in compute (size,limit)

main = do
  n' <- getEnv "N"
  l' <- getEnv "LIMIT"
  b' <- getEnv "BACKEND"
  let size      = read n' :: Int
      limit     = read l' :: Int
      view      = (-2.23, -1.15, 0.83, 1.15) :: View Float
      force arr = indexArray arr (Z:.0:.0) `seq` arr
  arr1e <- mandelbrot size size limit F.Even $ A.use $ A.fromList Z [view]
  arr1f <- mandelbrot size size limit F.ThirdFirst $ A.use $ A.fromList Z [view]
  arr1l <- mandelbrot size size limit F.ThirdLast $ A.use $ A.fromList Z [view]
  arr1qf <- mandelbrot size size limit F.FourthFirst $ A.use $ A.fromList Z [view]
  arr1ql <- mandelbrot size size limit F.FourthLast $ A.use $ A.fromList Z [view]
  arr1ff <- mandelbrot size size limit F.FifthFirst $ A.use $ A.fromList Z [view]
  arr1fl <- mandelbrot size size limit F.FifthLast $ A.use $ A.fromList Z [view]
  arr1tf <- mandelbrot size size limit F.TenthFirst $ A.use $ A.fromList Z [view]
  arr1tl <- mandelbrot size size limit F.TenthLast $ A.use $ A.fromList Z [view]
  arr2 <- return $ mandelbrot' size size limit $ A.use $ A.fromList Z [view]
  let arr1e' = F.combine arr1e
      arr1f' = F.combine arr1f
      arr1l' = F.combine arr1l
      arr1qf' = F.combine arr1qf
      arr1ql' = F.combine arr1ql
      arr1ff' = F.combine arr1ff
      arr1fl' = F.combine arr1fl
      arr1tf' = F.combine arr1tf
      arr1tl' = F.combine arr1tl
  if b' == "multi"
  then defaultMain
           [bgroup "Mandel" [ bench ("multi 10%: n = " P.++ (show size)) $ whnf C.runMulti arr1tf'
                            , bench ("multi 20%: n = " P.++ (show size)) $ whnf C.runMulti arr1ff'
                            , bench ("multi 25%: n = " P.++ (show size)) $ whnf C.runMulti arr1qf'
                            , bench ("multi 33%: n = " P.++ (show size)) $ whnf C.runMulti arr1f'
                            , bench ("multi 50%: n = " P.++ (show size)) $ whnf C.runMulti arr1e'
                            , bench ("multi 66%: n = " P.++ (show size)) $ whnf C.runMulti arr1l'
                            , bench ("multi 75%: n = " P.++ (show size)) $ whnf C.runMulti arr1ql'
                            , bench ("multi 80%: n = " P.++ (show size)) $ whnf C.runMulti arr1fl'
                            , bench ("multi 90%: n = " P.++ (show size)) $ whnf C.runMulti arr1tl'
                            ]]
  else defaultMain
           [bgroup "Mandel" [ bench ("normal: n = " P.++ (show size)) $ whnf C.run arr2]]



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
    -> F.SplitBy
    -> Acc (Scalar (View a))
    -> F.TuneM (F.Acc (Array DIM2 Int32))
mandelbrot screenX screenY depth i view =
  F.generateSplit (constant (Z:.screenY:.screenX))
       (\ix -> let c = initial ix
               in  A.snd $ A.while (\zi -> A.snd zi A.<* lIMIT &&* dot (A.fst zi) A.<* 4)
                       (\zi -> lift1 (next c) zi)
                       (lift (c, constant 0))) i
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

