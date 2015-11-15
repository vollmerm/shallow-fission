module Main where

import           Data.Array.Accelerate ((:.) (..), Array, Elt, Exp, Shape)
import           Data.Array.Accelerate as A
import           Fission1              as F
import           Prelude               as P hiding (concat, map)

-- | Calculate accelerations on these particles in a naïve O(n^2) way.
--
--   This maps a _sequential_ reduction to get the total contribution for this
--   body from all other bodies in the system.
--
calcAccels :: Exp R -> A.Acc (Vector PointMass) -> TuneM (F.Acc (Vector Accel))
calcAccels epsilon bodies'
  = let bodies          = mkacc bodies'
        move body       = A.sfoldl (\acc next -> acc .+. accel epsilon body next)
                                   (vec 0)
                                   (constant Z)
                                   bodies' -- Is this correct?
    in
    F.map move bodies

accel   :: Exp R                -- ^ Smoothing parameter
        -> Exp PointMass        -- ^ The point being accelerated
        -> Exp PointMass        -- ^ Neighbouring point
        -> Exp Accel
accel epsilon pmi pmj = s *. r
  where
    mj          = massOfPointMass pmj
    r           = positionOfPointMass pmj .-. positionOfPointMass pmi
    rsqr        = dot r r + epsilon * epsilon
    invr        = 1 / sqrt rsqr
    invr3       = invr * invr * invr
    s           = mj * invr3


-- Body ------------------------------------------------------------------------
--

-- | Make a stationary Body of unit mass
--
unitBody :: Exp (Vec R) -> Exp Body
unitBody pos = lift (pointmass, vec 0, vec 0)
  where
    pointmass = lift (pos, constant 1)          :: Exp PointMass

-- | Take the Velocity of a Body
--
velocityOfBody :: Exp Body -> Exp Velocity
velocityOfBody body = vel
  where
    (_, vel, _) = unlift body   :: (Exp PointMass, Exp Velocity, Exp Accel)

-- | Take the Acceleration of a Body
--
accelOfBody :: Exp Body -> Exp Accel
accelOfBody body = acc
  where
    (_, _, acc) = unlift body   :: (Exp PointMass, Exp Velocity, Exp Accel)

-- | Take the PointMass of a Body
--
pointMassOfBody :: Exp Body -> Exp PointMass
pointMassOfBody body = mp
  where
    (mp, _, _)  = unlift body   :: (Exp PointMass, Exp Velocity, Exp Accel)

-- | Take the position or mass of a PointMass
--
positionOfPointMass :: Exp PointMass -> Exp Position
positionOfPointMass = A.fst

massOfPointMass :: Exp PointMass -> Exp Mass
massOfPointMass = A.snd

-- Types -----------------------------------------------------------------------
-- We're using tuples instead of ADTs and defining Elt instances
--

-- | Not all compute devices support double precision
--
type R          = Float

-- | A data point in space
--
type Vec a      = (a, a, a)

-- | Units of time
--
type Time       = R

-- | The velocity of a point.
--
type Velocity   = Vec R

-- | The acceleration of a point.
--
type Accel      = Vec R

-- | A point in 2D space with its mass.
--
type Mass       = R
type Position   = Vec R
type PointMass  = (Position, Mass)

-- | Bodies consist of a Position and Mass, but also carry their velocity and
--   acceleration between steps of the simulation.
--
type Body       = (PointMass, Velocity, Accel)

-- | The magnitude of a vector.
--
magnitude :: (Elt a, IsFloating a) => Exp (Vec a) -> Exp a
magnitude v = sqrt (dot v v)


-- | Dot product of a vector
--
dot :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp a
dot v1 v2
  = let (x1,y1,z1) = unlift v1
        (x2,y2,z2) = unlift v2
    in
    x1 * x2 + y1 * y2 + z1 * z2


-- | Normalise a vector, so it has a magnitude of 1.
--
normalise :: (Elt a, IsFloating a) => Exp (Vec a) -> Exp (Vec a)
normalise v = (1 / magnitude v) *. v

-- | Replicate a value into a vector
--
vec :: Elt a => Exp a -> Exp (Vec a)
vec x = lift (x,x,x)

-- | Basic arithmetic component-wise
--
infixl 7 .*.
infixl 6 .+.
infixl 6 .-.

(.+.), (.-.), (.*.) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp (Vec a)
(.+.) = vzipWith (+)
(.-.) = vzipWith (-)
(.*.) = vzipWith (*)

-- | Apply a scalar value component-wise to each element of the vector
--
infixl 7 *.
infixl 6 +.
infixl 6 -.

(+.), (-.), (*.) :: (Elt a, IsNum a) => Exp a -> Exp (Vec a) -> Exp (Vec a)
(+.) c = vmap (c+)
(-.) c = vmap (c-)
(*.) c = vmap (c*)

-- | Arithmetic lifted to our vector type. As far as possible, want to gloss
--   over whether we are calculating in 2D or 3D.
--
vmap :: (Elt a, Elt b) => (Exp a -> Exp b) -> Exp (Vec a) -> Exp (Vec b)
vmap f v
  = let (x1,y1,z1) = unlift v
    in
    lift (f x1, f y1, f z1)

vzipWith :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> Exp (Vec a) -> Exp (Vec b) -> Exp (Vec c)
vzipWith f v1 v2
  = let (x1,y1,z1) = unlift v1
        (x2,y2,z2) = unlift v2
    in
    lift (f x1 x2, f y1 y2, f z1 z2)
