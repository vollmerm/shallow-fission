{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}

import Data.Array.Accelerate             as A -- hiding ( Acc, compute )
import qualified Data.Array.Accelerate   as A
import Data.Array.Accelerate.Interpreter as A
import Prelude                           as P

import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Typeable
import Text.Printf


data PAcc a where
  One  :: Acc a -> PAcc a
  Many :: Int -> [Acc a] -> PAcc a

data P a where
  PDo   :: (Arrays a, Arrays b)
        => (PAcc a -> P (PAcc b))
        -> P (PAcc a)
        -> P (PAcc b)

  PJoin :: (Arrays a, Arrays b, Arrays c)
        => (PAcc a -> PAcc b -> P (PAcc c))
        -> P (PAcc a)
        -> P (PAcc b)
        -> P (PAcc c)

  PUse  :: (Arrays a, Show a)
        => a
        -> P (PAcc a)

-- partial evaluator; collapses some terms into each other (I think).
peval :: P a -> P a
peval p =
  case p of
    PUse a      -> PUse a
    PDo f x     -> PDo f (peval x)        -- TODO: make decisions here:
                                          --  (1) split result of evaluating x
                                          --  (2) force computation of f
    -- case peval x of
    --   PUse a    -> f (One (use a))     -- decision: should we split the input?
    --   y         -> PBind (peval y) f   -- decision: force the result before applying to f
    PJoin f x y -> PJoin f (peval x) (peval y)


{--
apply :: (Arrays a, Arrays b, Arrays c)
      => (PAcc a -> P (PAcc b))
      -> (PAcc b -> P (PAcc c))
      -> (PAcc a -> P (PAcc c))
apply g f a =
  case g a of
    PUse x       -> f $ One (use x)
    x            -> PBind (peval x) f
    -- PBind x g'   -> PBind (peval (g a)) f
    -- PJoin g' x y -> PBind (peval (g a)) f
--}

-- schedules :: P (PAcc a) -> [S (Acc a)]
-- schedules (PUse a)    = [SUse a]
-- schedule (PBind x f) = SBind (schedule x) (schedule . f . One)


-- Now we have a problem that we need to turn a (P a) into (a). i.e., we need to
-- unwrap the burrito, and we can't really do that.
--

-- schedule :: P (PAcc a) -> [S (Acc a)]
-- schedule (PUse a)   = [SUse a]
-- schedule (PDo xs f) = [ SBind x


-- par :: (PAcc a -> PAcc b) -> [Acc a -> Acc b]
-- par f = fs'
--   where
--     fs'

runP :: P (PAcc a) -> PAcc a
runP (PUse a)      = One (use a)
runP (PDo f x)     = runP $ f $ runP x
runP (PJoin f x y) = runP $ f (runP x) (runP y)


data P' a where
  P'Do :: (Arrays a, Arrays b)
       => (PAcc a -> PAcc b)
       -> P' (PAcc a)
       -> P' (PAcc b)

  P'Join :: (Arrays a, Arrays b, Arrays c)
         => (PAcc a -> PAcc b -> PAcc c)
         -> P' (PAcc a)
         -> P' (PAcc b)
         -> P' (PAcc c)

  P'Use :: (Arrays a, Show a)
        => a
        -> P' (PAcc a)


select :: P (PAcc a) -> P' (PAcc a)
select (PUse a)      = P'Use a
select (PDo f x)     = P'Do (runP . f) (select x)
select (PJoin f x y) = P'Join (\a b -> runP (f a b)) (select x) (select y)


-- runP1 :: (PAcc a -> P (PAcc b)) -> PAcc a -> PAcc b
-- runP1 f (One x) =

-- spar :: Arrays b => (Acc a -> S (Acc b)) -> PAcc a -> S (Acc b)
-- spar f (One x)     = f x
-- spar f (Many s xs) = SFork $ P.map f xs


-- schedule :: P' (PAcc a) -> S (Acc a)
-- schedule (P'Use a)  = SUse a
-- schedule (P'Do f x) = undefined


prun1 :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> b)
      -> (PAcc a -> PAcc b)
      -> a -- ???
      -> b -- ???
prun1 go1 f = undefined -- ???


-- Representation where we have selected which chunking strategy to use.
--
data S a where
  SBind :: (Arrays a, Arrays b)
        => S (Acc a)
        -> (Acc a -> Acc b)
        -> S (Acc b)

  SJoin :: (Arrays a, Arrays b, Arrays c)
        => (Acc a -> Acc b -> Acc c)
        -> S (Acc a)
        -> S (Acc b)
        -> S (Acc c)

  -- SFork :: Arrays a
  --       => [S (Acc a)]
  --       -> S (Acc a)

  SUse  :: (Arrays a, Show a)
        => a
        -> S (Acc a)

instance Show (S a) where
  show (SBind x f)   = printf "(SBind %s %s)" (show x) (show f)
  show (SJoin f x y) = printf "(SJoin %s %s %s)" (show f) (show x) (show y)
  show (SUse x)      = printf "(SUse %s)" (show x)


-- Representation with the choice of executor embedded in it.
--
data E a where
  EBind :: (Arrays a, Arrays b)
        => E a
        -> (a -> b)
        -> E b

  EJoin :: (Arrays a, Arrays b, Arrays c)
        => (a -> b -> c)
        -> E a
        -> E b
        -> E c

  -- EFork :: Arrays a
  --       => [E a]
  --       -> E a

  EUse  :: Arrays a
        => a
        -> E a

run2 :: (Arrays a, Arrays b, Arrays c) => (Acc a -> Acc b -> Acc c) -> (a -> b -> c)
run2 f x y = run1 (A.uncurry f) (x,y)


-- sfork :: S (Acc a) -> [S (Acc a)]
-- sfork (SUse a)        = [SUse a]
-- sfork (SJoin f xs ys) = [ f x y | x <-

-- spar :: S (Acc a) -> [Acc a]
-- spar (SUse a)  = [use a]
-- spar (SFork f) = concatMap spar f
-- spar (SJoin f sx sy) = [ f x y | x <- spar sx | y <- spar sy ]
-- spar (SBind xs f)    = concatMap spar [ f x | x <- spar xs ]


-- In this step, assign each operation to a specific backend
--
exec :: S (Acc a) -> E a
exec (SJoin f x y) = EJoin (run2 f) (exec x) (exec y)
exec (SBind x f)   = EBind (exec x) (run1 f)
exec (SUse x)      = EUse x


-- Finally, we have something that we can actually execute
--
eval :: E a -> a
eval (EJoin f x y) = f (eval x) (eval y)
eval (EBind x f)   = f (eval x)
eval (EUse x)      = x

{--
--------------------------------------------------------------------------------
-- Redoing everything that we did the first time around
--------------------------------------------------------------------------------

data Term t where
  Con :: t -> Term t
  Lam :: (Term s -> Term t) -> Term (s -> t)
  App :: Term (s -> t) -> Term s -> Term t

  Bind :: Term a -> (a -> Term b) -> Term b
  Join :: (Term a -> Term b -> Term c) -> Term a -> Term b -> Term c


interp :: Term t -> t
interp (Con v)      = v
interp (Lam f)      = \x -> interp (f (Con x))
interp (App f x)    = (interp f) (interp x)
interp (Bind x f)   = interp (f (interp x))
interp (Join f x y) = interp $ f x y


-- Church encode some numbers.
-- I know what that means now!!
--
t0 :: Term ((Int -> Int) -> Int -> Int)
t0 = Lam $ \f -> Lam $ \x -> x

t1 :: Term ((Int -> Int) -> Int -> Int)
t1 = Lam $ \f -> Lam $ \x -> App f x

t2 :: Term ((Int -> Int) -> Int -> Int)
t2 = Lam $ \f -> Lam $ \x -> App f (App f x)

t3 :: Term ((Int -> Int) -> Int -> Int)
t3 = Lam $ \f -> Lam $ \x -> App f (App f (App f x))


plus :: Term (((Int -> Int) -> Int -> Int) -> ((Int -> Int) -> Int -> Int) -> (Int -> Int) -> Int -> Int)
plus = Lam $ \m ->
       Lam $ \n ->
       Lam $ \f ->
       Lam $ \x -> m `App` f `App` (n `App` f `App` x)


plus23 = plus `App` t2 `App` t3


--

data Acc a where
  Acc :: Int -> [A.Acc a] -> Acc a

deriving instance Arrays a => Show (Acc a)


xs :: A.Vector Int
xs = fromList (Z:.10) [0..]

-- map' f x = Con (A.map f) `App` x

use' x = Con (Acc 0 [A.use x])

map' f xs =
  Bind xs $ \(Acc s as) ->
    Con (Acc s (P.map (\a -> A.map f a) as))

zipWith' f xs ys =
  Bind xs $ \(Acc sx xs') ->
  Bind ys $ \(Acc sy ys') ->
    assert (sx == sy) $
    Con $ Acc sx (P.zipWith f xs' ys')

-- fold' f z xs =
--   Bind xs $ \(Acc sx xs') ->


computeInterp :: Arrays a => Term (Acc a) -> Acc a
computeInterp (Con v)      = compute v
computeInterp (App f x)    = (computeInterp f) (computeInterp x)
computeInterp (Bind x f)   = computeInterp (f (computeInterp x))
computeInterp (Join f x y) = computeInterp $ f x y

compute :: Arrays a => Acc a -> Acc a
compute (Acc s as) = Acc s (P.map A.compute as)
--}

