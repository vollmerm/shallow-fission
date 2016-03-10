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

{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Data.Array.Accelerate              as A hiding ( Split )
import qualified Data.Array.Accelerate    as A
import Data.Array.Accelerate.Interpreter  as A
import Prelude                            as P

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Logic
import Control.Parallel
import Data.Monoid
import Data.Typeable
import System.Random.MWC                  as MWC
import Text.Printf


--------------------------------------------------------------------------------
-- LEVEL 1: Pre-fission, arrays conceptually in one or more pieces
--------------------------------------------------------------------------------
--
-- This is the level exposed to shim/library writer
--

data Out a where
  -- Return :: a -> Out a
  Use    :: a -> Out (Acc a)
  --
  Bind   :: (Show a, Arrays a)
         => Out (Acc a)
         -> (Acc a -> b)
         -> Out b
  --
  Join   :: (Show a, Show b, Arrays a, Arrays b)
         => Out (Acc a)                 -- evaluate 'a' and 'b' to completion...
         -> Out (Acc b)                 -- ...possibly on different devices
         -> (Acc a -> Acc b -> c)       -- pass both along to some subcomputation
         -> Out c

data Arr a where
  Arr :: Int            -- split dimension
      -> Int            -- number of pieces (n)
      -> (Int -> Out a) -- generator for each piece [0 .. n-1]
      -> Arr a


psplit1
  :: (Shape sh, Slice sh, Elt e)
  => Double
  -> Arr (Acc (Array (sh :. Int) e))
  -> Arr (Acc (Array (sh :. Int) e))
psplit1 p (Arr dx nx gx)
  | dx == 1 || nx == 1
  = let gx' i | m == 0    = Bind (gx n) (P.fst . split1 p)
              | otherwise = Bind (gx n) (P.snd . split1 p)
              where
                (n,m) = divMod i 2
    in
    Arr 1 (nx*2) gx'
psplit1 _ _
  = error "psplit1: can't recursively subdivide on different dimensions ):"



-- This forces the computation over two pieces, but instead we should really
-- leave that up to the schedule/tune phase.
--
pgenerate
    :: forall sh e. (Inf sh, Shape sh, Elt e)
    => Exp sh
    -> (Exp sh -> Exp e)
    -> Arr (Acc (Array sh e))
pgenerate sh f =
  let
      dummy :: Scalar ()
      dummy = fromList Z [()]
      --
      arr = Arr 0 1 (\_ -> Bind (Use dummy) (\_ -> A.generate sh f))
  in
  case inf (undefined::sh) of
    Inf1 -> psplit1 0.5 arr
    _    -> arr


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
  Arr dx nx (\i -> Join (gx i) (gy i) (A.zipWith f))


pmap :: (Shape sh, Elt a, Elt b)
     => (Exp a -> Exp b)
     -> Arr (Acc (Array sh a))
     -> Arr (Acc (Array sh b))
pmap f (Arr d n gx) =
  Arr d n (\i -> Bind (gx i) (A.map f))


puse :: Arrays a => a -> Arr (Acc a)
puse a = Arr 0 1 (\_ -> Use a)


-- TLM: after the fold completes we only have one piece. We must be able to
--      decide whether or not to split again.
--
pfold :: forall sh e. (Inf sh, Shape sh, Elt e)
      => (Exp e -> Exp e -> Exp e)
      -> Exp e
      -> Arr (Acc (Array (sh :. Int) e))
      -> Arr (Acc (Array sh e))
pfold f z (Arr dx nx gx) =
  let piece i = Bind (gx i) (A.fold f z)
      with    = case dx of
                  1                               -> A.zipWith f
                  2 | Inf1 <- inf (undefined::sh) -> (A.++)
                  3 | Inf2 <- inf (undefined::sh) -> concat2
                  _                               -> error "pfold: unhandeled dimension"
  in
  Arr 0 1 (\_ -> P.foldl1 (\x y -> Join x y with) -- decide splits??
               $ P.map piece [0 .. nx-1])


-- arr :: (Shape sh, Elt e) => Out (Acc (Array sh e)) -> Arr (Acc (Array sh e))
-- arr x = Arr 0 1 (\_ -> x)


--------------------------------------------------------------------------------
-- LEVEL 2: Fission decision baked into a data structure
--------------------------------------------------------------------------------
--
-- This step disentangles the multi-piece array representation and inserts
-- explicit fork/join nodes representing the parallel computations on each
-- piece. Also generates (Acc -> Acc) functions in the process.
--

data S a where
  SUse    :: a -> S (Acc a)
  SBind   :: (Show a, Arrays a) => S (Acc a) -> (Acc a -> b) -> S b
  SJoin   :: (Show a, Show b, Arrays a, Arrays b) => S (Acc a) -> S (Acc b) -> (Acc a -> Acc b -> c) -> S c

  -- SReturn :: a -> S a
  -- SSplit  :: S (Acc a) -> (Acc a -> S b) -> (Acc a -> S c) -> (b -> c -> d) -> S d

instance (Show a, Arrays a) => Show (S (Acc a)) where
  show (SUse x)      = printf "(SUse %s)" (show x)
  show (SBind x f)   = printf "(SBind %s (%s))" (show x) (show f)
  show (SJoin x y f) = printf "(SJoin %s %s (%s))" (show x) (show y) (show f)
  -- show (SReturn x)   = printf "(SReturn %s)" (show x)


-- Convert an Out to an S
--
sout :: Out (Acc a) -> S (Acc a)
sout (Use x)      = SUse x
-- sout (Return x)   = SReturn x
sout (Bind x f)   = SBind (sout x) f
sout (Join x y f) = SJoin (sout x) (sout y) f

-- Flip coins / ask an oracle and decide whether to fuse or compute things at
-- each step.
--
stune :: S (Acc a) -> S (Acc a)
stune (SBind (SBind y f) g) = stune (SBind y (g . f))
stune (SJoin x y f)         = SJoin (stune x) (stune y) f
stune x                     = x


-- Schedule a computation for execution
--
schedule
    :: forall sh e. (Inf sh, Shape sh, Elt e)
    => Arr (Acc (Array sh e))
    -> S   (Acc (Array sh e))
schedule (Arr _  1  gx) = sout (gx 1)
schedule (Arr dx nx gx) =
  let
      pieces    = P.map gx [0 .. nx-1]
      combine f = P.foldl1 (\x y -> Join x y f) pieces
  in
  sout $ case dx of
           1 | Inf1 <- inf (undefined::sh) -> combine (A.++)
           2 | Inf2 <- inf (undefined::sh) -> combine concat2
           _                               -> error "schedule: unhandled dimension"


-- Decide how to split the input array over the given executor.
--
schedule1
    :: forall sh sh' a b. (Inf sh, Inf sh', Shape sh, Shape sh', Slice sh, Elt a, Elt b)
    => (Arr (Acc (Array sh a)) -> Arr (Acc (Array sh' b)))
    -> Array sh a
    -> S (Acc (Array sh' b))
schedule1 f a
  | Inf0 <- inf (undefined::sh) = schedule (f (Arr 0 1 (\_ -> Use a)))
  | Inf1 <- inf (undefined::sh) = schedule (f (use1 0.5 a))     -- TODO: pick random split point
  | Inf2 <- inf (undefined::sh) = schedule (f (use2 0.5 a))     -- TODO: choose to split either up/down or left/right


-- TODO: potentially split the array on the host (by, e.g., running with the
--       LLVM CPU backend) so that we don't have to upload the entire array to
--       the device to then immediately slice out a small portion.
--
use1 :: (Shape sh, Slice sh, Elt e)
     => Double
     -> Array (sh :. Int) e
     -> Arr (Acc (Array (sh :. Int) e))
use1 p a =
  let xy    = split1 p
      arr 0 = Bind (Use a) (P.fst . xy)
      arr 1 = Bind (Use a) (P.snd . xy)
  in
  Arr 1 2 arr

use2 :: (Shape sh, Slice sh, Elt e)
     => Double
     -> Array (sh :. Int :. Int) e
     -> Arr (Acc (Array (sh :. Int :. Int) e))
use2 p a =
  let xy    = split2 p
      arr 0 = Bind (Use a) (P.fst . xy)
      arr 1 = Bind (Use a) (P.snd . xy)
  in
  Arr 2 2 arr


--------------------------------------------------------------------------------
-- LEVEL 3: Execution (backend) decision baked into data structure
--------------------------------------------------------------------------------

data E a where
  EUse  :: a -> E a
  EBind :: (Arrays a) => E a -> (a -> b) -> E b
  EJoin :: (Arrays a, Arrays b) => E a -> E b -> (a -> b -> c) -> E c

run2 :: (Arrays a, Arrays b, Arrays c) => (Acc a -> Acc b -> Acc c) -> (a -> b -> c)
run2 f x y = run1 (A.uncurry f) (x,y)


-- In this step, assign each operation to a specific backend
--
exec :: Arrays a => S (Acc a) -> E a
exec (SJoin x y f) = EJoin (exec x) (exec y) (run2 f)
exec (SBind x f)   = EBind (exec x) (run1 f)
exec (SUse x)      = EUse x


-- Finally, we have something that we can actually execute
--
eval :: E a -> a
eval (EJoin x y f) = let x' = eval x
                         y' = eval y
                     in x' `par` y' `pseq` f x' y'
eval (EBind x f)   = f (eval x)
eval (EUse x)      = x


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------


split1
    :: (Shape sh, Slice sh, Elt e)
    => Double
    -> Acc (Array (sh :. Int) e)
    -> (Acc (Array (sh :. Int) e), Acc (Array (sh :. Int) e))
split1 p a =
  let sz  = indexHead (shape a)
      n   = A.round (constant p * A.fromIntegral sz)
  in
  (A.take n a, A.drop n a)

split2
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Double
    -> Acc (Array (sh :. Int :. Int) e)
    -> ( Acc (Array (sh :. Int :. Int) e)
       , Acc (Array (sh :. Int :. Int) e)
       )
split2 p a =
  let sh :. n :. m = unlift (shape a) :: Exp sh :. Exp Int :. Exp Int
      n'           = A.round (constant p * A.fromIntegral n)
  in
  ( backpermute (A.lift (sh :. n'     :. m)) id a
  , backpermute (A.lift (sh :. n - n' :. m)) (\ix -> let sh :. j :. i = unlift ix :: Exp sh :. Exp Int :. Exp Int
                                                     in A.lift (sh :. j + n' :. i)) a
  )

concat2
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Acc (Array (sh :. Int :. Int) e)
    -> Acc (Array (sh :. Int :. Int) e)
    -> Acc (Array (sh :. Int :. Int) e)
concat2 xs ys =
  let sh1 :. xj :. xi = unlift (shape xs)  :: Exp sh :. Exp Int :. Exp Int
      sh2 :. yj :. yi = unlift (shape ys)  :: Exp sh :. Exp Int :. Exp Int
  in
  generate (A.lift $ (sh1 `intersect` sh2) :. (xj + yj) :. (min xi yi))
           (\ix -> let sh :. j :. i = unlift ix :: Exp sh :. Exp Int :. Exp Int
                   in  j A.<* xj ? (xs ! ix, ys ! A.lift (sh :. (j-xj) :. i)))


data Inf' sh where
  Inf0 ::                         Inf' Z
  Inf1 :: (Shape sh, Slice sh) => Inf' (sh :. Int)
  Inf2 :: (Shape sh, Slice sh) => Inf' (sh :. Int :. Int)

deriving instance Show (Inf' sh)

class Inf sh where
  inf :: sh -> Inf' sh

instance Inf Z where
  inf _ = Inf0

instance Inf DIM1 where
  inf _ = Inf1

instance (Shape sh, Slice sh) => Inf (sh :. Int :. Int) where
  inf _ = Inf2


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

p0, p1 :: Vector Int
p0 = [0..10]
p1 = [1,3..15]

p2 :: Arr (Acc (Vector Int))
p2 =
  let lr    = split1 0.5
      arr 0 = Bind (Use p0) (P.fst . lr)
      arr 1 = Bind (Use p0) (P.snd . lr)
  in
  Arr 1 2 arr

p3 :: Array DIM2 Float
p3 = fromList (Z :. 5 :. 20) [0,0.1..]

p4 :: Arr (Acc (Array DIM2 Float))
p4 =
  let tb    = split2 0.5
      arr 0 = Bind (Use p3) (P.fst . tb)
      arr 1 = Bind (Use p3) (P.snd . tb)
  in
  Arr 2 2 arr

p5 :: Arr (Acc (Array DIM2 Float))
p5 =
  let llrr  = split1 0.5
      ll    = split1 0.5 . P.fst . llrr
      rr    = split1 0.5 . P.snd . llrr
      arr 0 = Bind (Use p3) (P.fst . ll)
      arr 1 = Bind (Use p3) (P.snd . ll)
      arr 2 = Bind (Use p3) (P.fst . rr)
      arr 3 = Bind (Use p3) (P.snd . rr)
  in
  Arr 1 4 arr


t0 = run $ A.fold (+) 0 (use p3)
t1 = eval . exec . stune . schedule $ pfold (+) 0 p4
t2 = eval . exec         . schedule $ pfold (+) 0 p5


t3 = run $ A.map (*2) (use p0)
t4 = eval . exec $ schedule1 (pmap (*2)) p0

