{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception (evaluate)
import           Criterion.Main
import           Data.Array.Accelerate
                  (Array,DIM2,lift,lift1,Z(Z),(:.)(..),the,indexArray,
                   unlift,Exp,unindex2,IsFloating,constant,(&&*),Scalar,Acc,Elt)
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate.Data.Complex
import qualified Data.Array.Accelerate.Fission as F
import           Data.Int
import           Data.Word
import           Prelude as P

import           Data.Array.Accelerate.Interpreter as I


test :: (F.Acc (Array DIM2 Int32))
test =
  let size      = 800
      limit     = 255
      view      = (-2.23, -1.15, 0.83, 1.15) :: View Float
      -- force arr = indexArray arr (Z:.0:.0) `seq` arr
      compute (sz,lmt) =
        mandelbrot sz sz lmt $ A.use $ A.fromList Z [view]

  in compute (size,limit)

main :: IO ()
main =
  let size      = 800
      limit     = 255
      view      = (-2.23, -1.15, 0.83, 1.15) :: View Float
      force arr = indexArray arr (Z:.0:.0) `seq` arr
      compute (sz,lmt) =
        do tuned <- F.runTune2 $ F.combine $
                    mandelbrot sz sz lmt $ A.use $ A.fromList Z [view]
           evaluate $ force $ I.run tuned
  in defaultMain
         [bgroup "Mandel" [ bench ("size = " P.++ (show size)) $
                            whnfIO $ compute (size,limit)]]



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
    -> (F.Acc (Array DIM2 Int32))
mandelbrot screenX screenY depth view =
  F.generate (constant (Z:.screenY:.screenX))
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
