{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE ParallelListComp          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Data.Array.Accelerate             as A hiding ( Split )
import qualified Data.Array.Accelerate   as A
import Data.Array.Accelerate.Interpreter as A
import Prelude                           as P

import Control.Exception
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Typeable
import Text.Printf

import Control.Monad
import Control.Monad.Logic


--------------------------------------------------------------------------------
-- LEVEL 1: Pre-fission, arrays conceptually in one or more pieces
--------------------------------------------------------------------------------
--
-- This is the level exposed to shim/library writer
--

data Out a where
  Bind   :: Out (Acc a) -> (Acc a -> b) -> Out b
  Return :: a -> Out a
  --
  Join   :: Out (Acc a)                 -- evaluate 'a' and 'b' to completion...
         -> Out (Acc b)                 -- ...possibly on different devices
         -> (Acc a -> Acc b -> c)       -- pass both along to some subcomputation
         -> Out c

data Arr a where
  Arr :: Int            -- split dimension
      -> Int            -- number of pieces (n)
      -> (Int -> Out a) -- generator for each piece [1..n]
      -> Arr a


pzipWith
    :: (Shape sh, Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)
    -> Arr (Acc (Array sh a))
    -> Arr (Acc (Array sh b))
    -> Arr (Acc (Array sh c))
pzipWith f (Arr dx nx gx) (Arr _dy _ny gy) =
  -- guard (dx == dy)
  -- guard (nx == ny)
  -- There is a cartesian product of options here, where for each `i` we could
  -- either Bind (fuse) or Join (force) it. This is not expressed in the current
  -- interface.
  --
  -- pure (Arr dx nx (\i -> Bind (gx i) $ \x ->
  --                        Bind (gy i) $ \y ->
  --                        Return (A.zipWith f x y)))
  -- <|>
  Arr dx nx (\i -> Join (gx i) (gy i) (\x y -> A.zipWith f x y))


pmap :: (Shape sh, Elt a, Elt b)
     => (Exp a -> Exp b)
     -> Arr (Acc (Array sh a))
     -> Arr (Acc (Array sh b))
pmap f (Arr d n gx) =
  Arr d n (\i -> Bind (gx i) (A.map f))


puse :: Arrays a => a -> Arr (Acc a)
puse a = Arr 0 1 (\_ -> Return (use a))


pfold :: (Shape sh, Elt e)
      => (Exp e -> Exp e -> Exp e)
      -> Exp e
      -> Arr (Acc (Array (sh :. Int) e))
      -> Arr (Acc (Array sh e))
pfold f z (Arr dx nx gx) =
  let piece i = Bind (gx i) (A.fold f z)
      with    = case dx of
                  0 -> A.zipWith f
                  -- 1 -> concatV     -- onoes!! types!!
                  _ -> error "pfold: unhandeled dimension"
  in
  Arr 0 1 (\_ -> P.foldl1 (\x y -> Join x y with)
               $ P.map piece [1 .. nx])


--------------------------------------------------------------------------------
-- LEVEL 2: Fission decision baked into a data structure
--------------------------------------------------------------------------------
--
-- This step disentangles the multi-piece array representation and inserts
-- explicit fork/join nodes representing the parallel computations on each
-- piece. Also generates (Acc -> Acc) functions in the process.
--

data S a where
  SReturn :: a -> S a
  SBind   :: S (Acc a) -> (Acc a -> Acc b) -> S (Acc b)

  SSplit  :: S (Acc a) -> (Acc a -> S b) -> (Acc a -> S c) -> S (b,c)



-- data ScheduleState = SS

schedule :: Arr (Acc a) -> S (Acc a)
schedule (Arr dx nx gx) = undefined



-- schedule1 :: (Arr (Acc a) -> Arr (Acc b)) -> Acc a -> S (Acc a)
-- schedule1 = undefined


--------------------------------------------------------------------------------
-- LEVEL 3: Execution (backend) decision baked into data structure
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

concatV
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Acc (Array (sh :. Int :. Int) e)
    -> Acc (Array (sh :. Int :. Int) e)
    -> Acc (Array (sh :. Int :. Int) e)
concatV xs ys =
  let sh1 :. xj :. xi = unlift (shape xs)  :: Exp sh :. Exp Int :. Exp Int
      sh2 :. yj :. yi = unlift (shape ys)  :: Exp sh :. Exp Int :. Exp Int
  in
  generate (A.lift $ (sh1 `intersect` sh2) :. (xj + yj) :. (min xi yi))
           (\ix -> let sh :. j :. i = unlift ix :: Exp sh :. Exp Int :. Exp Int
                   in  j A.<* xj ? (xs ! ix, ys ! A.lift (sh :. (j-xj) :. i)))

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

p0, p1 :: Vector Int
p0 = [0..10]
p1 = [1,3..15]

