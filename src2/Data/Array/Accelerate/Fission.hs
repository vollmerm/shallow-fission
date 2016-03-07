{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}

module Data.Array.Accelerate.Fission where

import           Control.Monad()
import           Data.Array.Accelerate as A hiding (Acc, Divide, Split)
import qualified Data.Array.Accelerate as A
--import           Data.Split
import           Data.Typeable
import           Prelude               as P
import           Text.Printf

import Data.Array.Accelerate.Interpreter as A

data Rep a where
    Return :: (Arrays a)
           => FAcc a
           -> Rep (FAcc a)

    Bind   :: (Arrays a, Arrays b)
           => Rep (FAcc b)
           -> (FAcc b -> Rep (FAcc a))
           -> Rep (FAcc a)

    Join   :: (Arrays a, Arrays b, Arrays c)
           => (Rep (FAcc b) -> Rep (FAcc c) -> Rep (FAcc a))
           -> Rep (FAcc b)
           -> Rep (FAcc c)
           -> Rep (FAcc a)

    -- Use    :: (Arrays a)
    --        => a
    --        -> Rep (FAcc a) -- avoid collapsing use with bind


data FAcc a where
    FAcc :: Int -> [A.Acc a] -> FAcc a
    deriving Show

type Acc a = Rep (FAcc a)


-- instance Show a => Show (Rep a) where
--     show (Return a) = printf "(Return %s)" $ show a
--     show (Bind b f) = printf "(Bind %s %s)" (show b) (show f)

fizzCompute :: Arrays a => FAcc a -> FAcc a
fizzCompute (FAcc s as) = FAcc s $ P.map A.compute as


computeEval :: Rep (FAcc a) -> FAcc a
computeEval (Return a)   = a
computeEval (Bind b f)   = computeEval $ f $ fizzCompute (computeEval b)
computeEval (Join f a b) = computeEval $ f a b
-- computeEval (Use a)      = FAcc 0 [use a]

nocomputeEval :: Rep (FAcc a) -> (FAcc a)
nocomputeEval (Return a)   = a
nocomputeEval (Bind b f)   = computeEval $ f (computeEval b)
nocomputeEval (Join f a b) = computeEval $ f a b
-- nocomputeEval (Use a)      = FAcc 0 [use a]



fizzMap :: (Shape sh, Elt a, Elt b) =>
           (Exp a -> Exp b)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
fizzMap f a = Bind a f'
    where f' (FAcc s as) = Return $ FAcc s $ P.map (A.map f) as

fizzZipWith :: (Shape sh, Elt a, Elt b, Elt c) =>
               (Exp a -> Exp b -> Exp c)
            -> Acc (Array sh a)
            -> Acc (Array sh b)
            -> Acc (Array sh c)
fizzZipWith f a1 a2 =
    Bind a1 $ Bind a2 . f'
    where f' (FAcc s1 a1') (FAcc _s2 a2') =
              Return $ FAcc s1 $ P.zipWith (A.zipWith f) a1' a2'

fizzZipWith' :: (Shape sh, Elt a, Elt b, Elt c) =>
                (Exp a -> Exp b -> Exp c)
             -> Acc (Array sh a)
             -> Acc (Array sh b)
             -> Acc (Array sh c)
fizzZipWith' f = Join (fizzZipWith f)

fizzFold :: forall sh e. (Shape sh, Elt e) =>
            (Exp e -> Exp e -> Exp e)
         -> Exp e
         -> Acc (Array (sh :. Int) e)
         -> Acc (Array sh e)
fizzFold f z a = Bind a f'
    where f' (FAcc s as) =
              Bind (Return $ FAcc s $ P.map (A.fold f z) as)
                   (\a' -> case s of
                            0 -> join0 f a'
                            1 -> concatV a'
                            _ -> error "fizzFold: DIM case not handled.")


concatV :: FAcc (Array sh e) -> Acc (Array sh e)
concatV _a = undefined


join0 :: (Shape sh, Elt e) =>
         (Exp e -> Exp e -> Exp e)
      -> FAcc (Array sh e)
      -> Acc (Array sh e)
-- join0 f (FAcc s [a]) = Return $ FAcc s [a]
join0 f (FAcc s [a]) = Join const (Return $ FAcc s [a]) (Return $ FAcc s [a])
join0 f (FAcc s (a:as)) =
    Join (fizzZipWith f) (Return $ FAcc s [a]) (join0 f $ FAcc s as)

arr :: Acc (Array DIM2 Float)
-- arr = Use $ A.fromList (Z :. 10 :. 10) [0..]
-- arr = Return $ FAcc 0 [use $ A.fromList (Z :. 10 :. 10) [0..]]
arr = Return $ FAcc 0 [use $ A.fromList (Z :. 10 :. 10) [0..],
                    use $ A.fromList (Z :. 10 :. 10) [0..]]

foo1 :: (Shape sh) => Acc (Array sh Float) -> Acc (Array sh Float)
foo1 as = fizzMap (+ 1) as
-- Bind (Use a) (\a -> Return (map (+ 1) a))

foo2 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh Float)
foo2 as = fizzMap (* 2) (foo1 as)
-- Bind (Bind (Use a) (\a -> Return (map (+ 1) a))) (\a -> (Return map (* 2) a))

fooz :: Shape sh => Acc (Array sh Float) -> Acc (Array sh Float)
fooz as = fizzZipWith' (+) (foo2 as) (foo1 as)

fooz1 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh Float)
fooz1 as = let a = foo2 as in fizzZipWith' (+) a a

foo3 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh (Float,Float))
foo3 as = fizzMap (\x -> A.lift (x,1::Float)) (foo2 as)

foo4 :: Shape sh => Acc (Array (sh :. Int) Float) -> Acc (Array sh Float)
foo4 as = fizzFold (+) 0 (foo2 as)

partialEval :: Rep a -> Rep a
partialEval (Return a) = Return a
partialEval (Bind b f) =
    let b' = partialEval b
    in case b' of
         Return a -> f a
         -- Use a -> Bind (Use a) f
         Bind b'' g -> Bind b'' $ repCompose g f
         _ -> Bind b' f
partialEval (Join f a b) = Join f (partialEval a) (partialEval b)
-- partialEval (Use a) = Use a

repCompose
    :: Arrays c
    => (FAcc a -> Rep (FAcc b))
    -> (FAcc b -> Rep (FAcc c))
    -> FAcc a
    -> Rep (FAcc c)
repCompose g f a =
    case g a of
      Return a' -> f a'
      Bind b f' -> Bind (partialEval (Bind b f')) f
      Join f' a' b -> Bind (partialEval (Join f' a' b)) f
      -- Use a' -> Bind (Use a') f


-- data Rep' a where
--     Return' :: (Arrays a)
--             => a -> Rep' a
--     Bind'   :: (Arrays a, Arrays b)
--             => Rep' b -> (b -> Rep' a) -> Rep' a
--     Join'   :: (Arrays a, Arrays b, Arrays c)
--             => (Rep' b -> Rep' c -> Rep' a)
--             -> Rep' b -> Rep' c -> Rep' a
--     Use'    :: (Arrays a)
--             => a -> Rep' a

-- data Rep' a where
--     Return' :: (Arrays a) => A.Acc a -> Rep' (A.Acc a)
--     Bind'   :: (Arrays a) => Rep' (A.Acc b) -> (A.Acc b -> Rep' (A.Acc a)) -> Rep' (A.Acc a)
--     Join'   :: (Arrays a, Arrays b, Arrays c) =>
--                (Rep' (A.Acc b) -> Rep' (A.Acc c) -> Rep' (A.Acc a)) ->
--                Rep' (A.Acc b) -> Rep' (A.Acc c) -> Rep' (A.Acc a)
--     Use'    :: (Arrays a) => A.Acc a -> Rep' (A.Acc a)

-- naiveTranslate :: (Arrays a) => Rep (FAcc a) -> Rep' (A.Acc a)
-- naiveTranslate (Return (FAcc _ [a])) = Return' a
-- naiveTranslate (Bind b f) = Bind' (naiveTranslate b) f'
--     where f' a = Return' $ extractAcc $ nocomputeEval (f $ FAcc 0 [a])
-- naiveTranslate (Join f a b) = Join' undefined (naiveTranslate a) (naiveTranslate b)
-- naiveTranslate (Use (FAcc _ [a])) = Use' a

extractAcc :: Arrays a => FAcc a -> A.Acc a
extractAcc (FAcc _ [a]) = a

wrapAcc :: Arrays a => A.Acc a -> FAcc a
wrapAcc a = FAcc 0 [a]

data Schedule a where
    Compute :: (Arrays a, Arrays b) => Schedule b -> (b -> a) -> Schedule a
    Use'    :: (Arrays a) => a -> Schedule a
--    Return' :: (Arrays a) => A.Acc a -> Schedule a

run1' :: (Arrays a, Arrays b) => (A.Acc a -> A.Acc b) -> (a -> b)
run1' = undefined

naiveTranslate :: Rep (FAcc a) -> Schedule a
naiveTranslate (Return (FAcc _ [a])) = undefined -- Return' a
naiveTranslate (Bind b f) = Compute s' $ run1' f'
    where s' = naiveTranslate (Bind b f)
          f' a = undefined -- extractAcc $ nocomputeEval $ f $ wrapAcc a
                 -- can't write this...



-- Representation where we have selected which chunking strategy to use.
--
data S a where
  SBind :: (Arrays a, Arrays b)
        => S (A.Acc a)
        -> (A.Acc a -> A.Acc b)
        -> S (A.Acc b)

  SJoin :: (Arrays a, Arrays b, Arrays c)
        => (A.Acc a -> A.Acc b -> A.Acc c)
        -> S (A.Acc a)
        -> S (A.Acc b)
        -> S (A.Acc c)

  SReturn :: Arrays a         -- TLM: look into removing Return from the language
          => A.Acc a
          -> S (A.Acc a)

  SFork :: (Arrays a, Arrays b)
        => S (A.Acc a)
        -> S (A.Acc b)
        -> S (A.Acc a, A.Acc b)

  SUse  :: (Arrays a, Show a)
        => a
        -> S (A.Acc a)

instance Show (S a) where
  show (SBind x f)   = printf "(SBind %s (%s))" (show x) (show f)
  show (SJoin f x y) = printf "(SJoin (%s) %s %s)" (show f) (show x) (show y)
  show (SReturn x)   = printf "(SReturn %s)" (show x)
  show (SUse a)      = printf "(SUse %s)" (show a)
  show (SFork a b)   = printf "(SFork %s %s)" (show a) (show b)


schedule :: Rep (FAcc a) -> S (A.Acc a)
schedule (Return f)   = SReturn (extractAcc f)  -- TLM: don't want these!
schedule (Bind x f)   = SBind (schedule x) (extractAcc . computeEval . f . wrapAcc)
schedule (Join f x y) = SJoin (\x' y' -> extractAcc $ computeEval (f (Return $ wrapAcc x') (Return $ wrapAcc y')))
                              (schedule x)
                              (schedule y)


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

run2 :: (Arrays a, Arrays b, Arrays c) => (A.Acc a -> A.Acc b -> A.Acc c) -> (a -> b -> c)
run2 f x y = run1 (A.uncurry f) (x,y)

-- In this step, assign each operation to a specific backend
--
exec :: S (A.Acc a) -> E a
exec (SJoin f x y) = EJoin (run2 f) (exec x) (exec y)
exec (SBind x f)   = EBind (exec x) (run1 f)
exec (SUse x)      = EUse x

