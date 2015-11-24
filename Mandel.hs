{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import           Criterion.Main
import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex
import           Data.Array.Accelerate.Interpreter  as C
import           Data.Bits
import           Data.Word
import           Prelude                            as P


main =
  let n         = 800
      limit     = 255
      view      = (-2.23, -1.15, 0.83, 1.15) :: View Float
      force arr = indexArray arr (Z:.0:.0) `seq` arr
      world     = setPrec Float $ World view undefined Nothing Nothing Nothing
      setPrec f (World p _ z h v) =
          let render :: (Elt a, IsFloating a) => Render a
              render = C.run1 $ A.map (prettyRGBA (constant (P.fromIntegral limit))) . mandelbrot n n limit
          in case f of
               Float  -> World (convertView p :: View Float)  render z h v
               Double -> World (convertView p :: View Double) render z h v
  in defaultMain
         [bgroup "Mandel" [ bench ("n = " P.++ (show n)) $ whnf (force . renderWorld) world ]]



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
    -> Acc (Array DIM2 Int32)
mandelbrot screenX screenY depth view =
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



data Zoom       = In  | Out
data Move       = Fwd | Rev

data Precision  = Float | Double

data World where
  World :: (Elt a, RealFloat a)
        => View a
        -> Render a
        -> Maybe Zoom
        -> Maybe Move   -- horizontal movement
        -> Maybe Move   -- vertical movement
        -> World


renderWorld :: World -> Bitmap
renderWorld (World view render _ _ _) = render $ A.fromList Z [view]



convertView :: (Real a, Fractional b) => View a -> View b
convertView (x,y,x',y') = (realToFrac x, realToFrac y, realToFrac x', realToFrac y')


prettyRGBA :: Exp Int32 -> Exp Int32 -> Exp RGBA32
prettyRGBA cmax c = c ==* cmax ? ( 0xFF000000, escapeToColour (cmax - c) )

escapeToColour :: Exp Int32 -> Exp RGBA32
escapeToColour m = constant 0xFFFFFFFF - (packRGBA32 $ lift (a,b,g,r))
  where
    r   = A.fromIntegral (3 * m)
    g   = A.fromIntegral (5 * m)
    b   = A.fromIntegral (7 * m)
    a   = constant 0


packRGBA32, packRGBA32le, packRGBA32be
    :: Exp (Word8, Word8, Word8, Word8)
    -> Exp RGBA32
packRGBA32 = packRGBA32le

packRGBA32le (unlift -> (r, g, b, a))
   =  A.fromIntegral a `A.shiftL` 24
  .|. A.fromIntegral b `A.shiftL` 16
  .|. A.fromIntegral g `A.shiftL` 8
  .|. A.fromIntegral r

packRGBA32be (unlift -> (r, g, b, a))
   =  A.fromIntegral r `A.shiftL` 24
  .|. A.fromIntegral g `A.shiftL` 16
  .|. A.fromIntegral b `A.shiftL` 8
  .|. A.fromIntegral a
