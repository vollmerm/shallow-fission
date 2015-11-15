module Main where

import           Data.Array.Accelerate ((:.) (..), Array, Elt, Exp, IsFloating,
                                        IsNum, Scalar, Shape, Vector, Z (..),
                                        constant, lift, the, unlift)
import qualified Data.Array.Accelerate as A
import           Fission1              as F
import           Prelude               as P hiding (concat, map)


main = undefined


-- | Calculate accelerations on these particles in a naïve O(n^2) way.
--
--   This maps a _sequential_ reduction to get the total contribution for this
--   body from all other bodies in the system.
--
calcAccels :: Exp R -> F.Acc (Vector PointMass)
           -> TuneM (F.Acc (Vector Accel))
calcAccels epsilon bodies
  = let move body       = F.sfoldl (\acc next -> acc .+. accel epsilon body next)
                                   (vec 0)
                                   (constant Z)
                                   bodies -- Is this correct?
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

-- | Set the mass of a Body.
--
setMassOfBody :: Exp Mass -> Exp Body -> Exp Body
setMassOfBody mass body = lift (pointmass, vel, acc)
  where
    vel         = velocityOfBody body
    acc         = accelOfBody body
    pos         = positionOfPointMass (pointMassOfBody body)
    pointmass   = lift (pos, mass)      :: Exp PointMass


-- | Set the acceleration of a Body.
--
setAccelOfBody :: Exp Accel -> Exp Body -> Exp Body
setAccelOfBody acc body = lift (pm, vel, acc)
  where
    pm          = pointMassOfBody body
    vel         = velocityOfBody body


-- | Set the starting velocity of a Body.
--   It is set to rotate around the origin, with the speed proportional
--   to the sqrt of the distance from it. This seems to make nice simulations.
--
setStartVelOfBody :: Exp R -> Exp Body -> Exp Body
setStartVelOfBody startVel body = lift (pm, vel'', acc)
  where
    pm          = pointMassOfBody body
    acc         = accelOfBody body
    pos         = positionOfPointMass pm

    pos'        = normalise pos
    vel'        = lift (y', -x', z')
    vel''       = (sqrt (magnitude pos) * startVel) *. vel'

    (x',y',z')  = unlift pos'   :: Vec (Exp R)


-- | Advance a body forwards in time.
--
advanceBody :: Exp Time -> Exp Body -> Exp Body
advanceBody time body = lift ( pm', vel', acc )
  where
    pm          = pointMassOfBody body
    pos         = positionOfPointMass pm
    vel         = velocityOfBody body
    acc         = accelOfBody body
    mass        = massOfPointMass pm

    pm'         = lift (pos', mass)             :: Exp PointMass
    pos'        = pos .+. time *. vel
    vel'        = vel .+. time *. acc



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


data World
  = World
  {
    worldBodies :: !(Vector Body)                       -- ^ Bodies in the simulation
  , worldSteps  :: {-# UNPACK #-} !Int                  -- ^ Number of steps taken in the simulation so far
  , worldTime   :: {-# UNPACK #-} !Time                 -- ^ Current simulation time
  }


-- | Move bodies under the influence of acceleration
--
advanceBodies
    :: (Acc (Vector PointMass) ->
        TuneM (Acc (Vector Accel)))     -- ^ Function to compute accelerations at each point
    -> A.Acc (Scalar Time)              -- ^ Time step
    -> A.Acc (Vector Body)              -- ^ Bodies
    -> TuneM (Acc (Vector Body))
advanceBodies calcAccels timeStep bodies = do
  let bodies' = mkacc bodies
  bodies'' <- F.map pointMassOfBody bodies'
  accels   <- calcAccels bodies''
  let advance b a = let m = massOfPointMass (pointMassOfBody b)
                        a' = m *. a
                    in advanceBody (the timeStep) (setAccelOfBody a' b)
  F.zipWith advance bodies' accels
  -- = let
  --       -- Calculate the accelerations on each body.
  --       accels          = calcAccels
  --                       $ A.map pointMassOfBody bodies

  --       -- Apply the accelerations to the bodies and advance them
  --       advance b a     = let m         = massOfPointMass (pointMassOfBody b)
  --                             a'        = m *. a
  --                         in advanceBody (the timeStep) (setAccelOfBody a' b)
  --   in
  --   A.zipWith advance bodies accels


-- | Advance a cluster of bodies forward in time
--
advanceWorld
    :: (Scalar Time -> Vector Body -> Vector Body)      -- ^ Function to update body positions
    -> Time
    -> World
    -> World
advanceWorld advance timeStep world
  = let
        -- Update the bodies
        bodies' = advance (A.fromList Z [timeStep]) (worldBodies world)

        -- Update the world
        steps'  = worldSteps world + 1
        time'   = worldTime  world + timeStep

    in  world   { worldBodies   = bodies'
                , worldSteps    = steps'
                , worldTime     = time' }
