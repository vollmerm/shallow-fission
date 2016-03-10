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
  Bind   :: (Arrays a, Show a)
         => Out (Acc a)
         -> (Acc a -> b)
         -> Out b
  --
  Join   :: (Arrays a, Arrays b, Show a, Show b)
         => Out (Acc a)                 -- evaluate 'a' and 'b' to completion...
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
                  0                               -> A.zipWith f
                  1 | Inf1 <- inf (undefined::sh) -> (A.++)
                  2 | Inf2 <- inf (undefined::sh) -> concat2
                  _                               -> error "pfold: unhandeled dimension"
  in
  Arr 0 1 (\_ -> P.foldl1 (\x y -> Join x y with) -- decide splits??
               $ P.map piece [1 .. nx])


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
  -- SReturn :: a -> S a
  SBind   :: (Arrays a, Show a) => S (Acc a) -> (Acc a -> b) -> S b
  SJoin   :: (Arrays a, Arrays b, Show a, Show b) => S (Acc a) -> S (Acc b) -> (Acc a -> Acc b -> c) -> S c
  -- SSplit  :: S (Acc a) -> (Acc a -> S b) -> (Acc a -> S c) -> (b -> c -> d) -> S d

instance (Show a, Arrays a) => Show (S (Acc a)) where
  show (SUse x)      = printf "(SUse %s)" (show x)
  -- show (SReturn x)   = printf "(SReturn %s)" (show x)
  show (SBind x f)   = printf "(SBind %s (%s))" (show x) (show f)
  show (SJoin x y f) = printf "(SJoin %s %s (%s))" (show x) (show y) (show f)


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


schedule
    :: (Inf sh, Shape sh, Elt e)
    => Arr (Acc (Array sh e))
    -> S   (Acc (Array sh e))
schedule (Arr _  1  gx) = sout (gx 1)
-- schedule (Arr dx nx gx) = undefined

-- schedule :: Arr (Acc a) -> S (Acc b)
-- schedule = undefined


-- schedule1
--     :: forall sh e b. (Inf sh, Shape sh, Elt e, Arrays b)
--     => (Arr (Acc (Array sh e)) -> Arr (Acc b))
--     -> Acc (Array sh e)
--     -> S (Acc b)
-- schedule1 f a
--   | Inf0 <- inf (undefined::sh) = schedule (f (Arr 0 1 (\_ -> Return a)))
--   | Inf1 <- inf (undefined::sh) =
--       let (x,y) = split1 0.5 a      -- TODO: pick random split point
--           arr 0 = Return x
--           arr 1 = Return y
--       in
--       schedule (f (Arr 0 2 arr))


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
      arr 1 = Bind (Use p0) (P.fst . lr)
      arr 2 = Bind (Use p0) (P.snd . lr)
  in
  Arr 0 2 arr

p3 :: Array DIM2 Float
p3 = fromList (Z :. 4 :. 10) [0,0.1..]

p4 :: Arr (Acc (Array DIM2 Float))
p4 =
  let tb    = split2 0.5
      arr 1 = Bind (Use p3) (P.fst . tb)
      arr 2 = Bind (Use p3) (P.snd . tb)
  in
  Arr 1 2 arr

p5 :: Arr (Acc (Array DIM2 Float))
p5 =
  let lr    = split1 0.8
      arr 1 = Bind (Use p3) (P.fst . lr)
      arr 2 = Bind (Use p3) (P.snd . lr)
  in
  Arr 0 2 arr


t0 = run $ A.fold (+) 0 (use p3)
t1 = eval . exec . stune . schedule $ pfold (+) 0 p4
t2 = eval . exec         . schedule $ pfold (+) 0 p5

