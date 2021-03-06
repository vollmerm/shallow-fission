
module Solver
  where

import Common.Type
import Common.Body
import Common.Util

import Data.Array.Accelerate                    as A


-- | Calculate accelerations on these particles in a naïve O(n^2) way.
--
--   This maps a _sequential_ reduction to get the total contribution for this
--   body from all other bodies in the system.
--
calcAccels :: Exp R -> Acc (Vector PointMass) -> Acc (Vector Accel)
calcAccels epsilon bodies
  = let move body       = A.sfoldl (\acc next -> acc .+. accel epsilon body next)
                                   (vec 0)
                                   (constant Z)
                                   bodies
    in
    A.map move bodies

