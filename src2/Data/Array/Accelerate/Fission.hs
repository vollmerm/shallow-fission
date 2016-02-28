{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Array.Accelerate.Fission where

import           Data.Split

import           Data.Array.Accelerate as A hiding (Acc, Divide, Split)
import qualified Data.Array.Accelerate as A
import           Prelude               as P

import           Control.Monad


-- Interpreter sketch

data Rep a where
    Return :: a -> Rep a
    Bind   :: Rep b -> (b -> Rep a) -> Rep a
    Join   :: (b -> c -> Rep a) -> Rep b -> Rep c -> Rep a
    Use    :: a -> Rep a -- avoid collapsing use with bind

instance Show (Rep a) where
    show (Return _) = "(Return <data>)"
    show (Bind b _) = "(Bind " P.++ show b P.++ " <function>)"
    show (Join _ a b) = "(Join <function> " P.++ show a P.++ " " P.++ show b P.++ ")"
    show (Use _) = "(Use <data>)"

data FAcc a where
    FAcc :: Int -> [A.Acc a] -> FAcc a
    deriving Show

type Acc a = Rep (FAcc a)


fizzMap :: (Shape sh, Elt a, Elt b) =>
           (Exp a -> Exp b)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
fizzMap f a = Bind a f'
    where f' (FAcc s as) = Return $ FAcc s $ P.map (A.map f) as
-- fizzMap f (Acc s as) = Return $ Acc s $ P.map (A.map f) as

fizzFold :: forall sh e. (Shape sh, Elt e) =>
            (Exp e -> Exp e -> Exp e)
         -> Exp e
         -> Acc (Array (sh :. Int) e)
         -> Acc (Array sh e)
fizzFold f z a = Bind a f'
    where f' (FAcc s as) =
              Bind (Return $ FAcc s $ P.map (A.fold f z) as)
                   (\a -> case s of
                            0 -> join0 f a
                            1 -> concatV a
                            _ -> error "fizzFold: DIM case not handled.")
-- fizzFold f z (Acc s as) =
--     Bind (Return $ Acc s $ P.map (A.fold f z) as)
         -- (\b -> case s of
         --          0 -> join0 f b
         --          1 -> concatV b
         --          _ -> error "fizzFold: DIM case not handled.")

fizzZipWith' :: (Shape sh, Elt a, Elt b, Elt c) =>
                (Exp a -> Exp b -> Exp c)
             -> Acc (Array sh a)
             -> Acc (Array sh b)
             -> Acc (Array sh c)
fizzZipWith' f a1 a2 =
    Bind a1 (\a1 -> (Bind a2 (\a2 -> f' a1 a2)))
    where f' (FAcc s1 a1) (FAcc _s2 a2) =
              Return $ FAcc s1 $ P.zipWith (A.zipWith f) a1 a2

zipWith' :: (Shape sh, Elt a, Elt b, Elt c) =>
            (Exp a -> Exp b -> Exp c)
         -> FAcc (Array sh a)
         -> FAcc (Array sh b)
         -> Acc (Array sh c)
zipWith' f (FAcc s1 a1) (FAcc _s2 a2) =
    -- s1 and s2 might not be equal...
    Return $ FAcc s1 $ P.zipWith (A.zipWith f) a1 a2

concatV :: FAcc (Array sh e) -> Acc (Array sh e)
concatV _a = undefined

join0 :: (Shape sh, Elt e) =>
         (Exp e -> Exp e -> Exp e)
      -> FAcc (Array sh e)
      -> Acc (Array sh e)
join0 f (FAcc s [a]) = Return $ FAcc s [a]
join0 f (FAcc s (a:as)) =
    Join (zipWith' f) (Return $ FAcc s [a]) (join0 f $ FAcc s as)
-- join0 _ (Acc s [a]) = Return $ Acc s [a]
-- join0 f (Acc s (a:as)) =
--     Join (zipWith' f) (Return $ Acc s [a]) (join0 f (Acc s as))
-- join0 _ _ = error "impossible"




arr :: Acc (Array DIM2 Float)
arr = Use $ FAcc 0 [use $ A.fromList (Z :. 10 :. 10) [0..],
                    use $ A.fromList (Z :. 10 :. 10) [0..]]

foo1 :: (Shape sh) => Acc (Array sh Float) -> Acc (Array sh Float)
foo1 as = fizzMap (+ 1) as
-- Bind (Use a) (\a -> Return (map (+ 1) a))

foo2 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh Float)
foo2 as = fizzMap (* 2) (foo1 as)
-- Bind (Bind (Use a) (\a -> Return (map (+ 1) a))) (\a -> (Return map (* 2) a))

foo3 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh (Float,Float))
foo3 as = fizzMap (\x -> A.lift (x,1::Float)) (foo2 as)
-- Bind (Bind (Bind (Use a) (\a -> Return (map (+ 1) a))) (\a -> (Return map (* 2) a))) (\a -> Return (map f a))

foo4 :: Shape sh => Acc (Array (sh :. Int) Float) -> Acc (Array sh Float)
foo4 as = fizzFold (+) 0 (foo2 as)
-- Bind (Bind (Bind (Bind (Use a) (\a -> Return (map (+ 1) a))) (\a -> (Return map (* 2) a))) (\a -> Return (map f a))) (\a -> Bind (Return fold a) (\a -> Join a))

foo5 :: Shape sh => Acc (Array (sh :. Int) Float) -> Acc (Array sh Float)
foo5 as = fizzMap (* 5) (foo4 as)

naiveEval :: Rep a -> a
naiveEval (Return a)   = a
naiveEval (Bind b f)   = naiveEval $ f (naiveEval b) --  $ Acc 0 $ fizzCompute (naiveEval b)
naiveEval (Join f a b) = naiveEval $ f (naiveEval a) (naiveEval b)
naiveEval (Use a)      = a

-- Can't write this:
-- computeEval :: Rep (FAcc a) -> FAcc a
-- computeEval (Return a)   = a
-- computeEval (Bind b f)   = computeEval $ f $ fizzCompute (computeEval b)
-- computeEval (Join f a b) = computeEval $ f (computeEval a) (computeEval b)
-- computeEval (Use a)      = a


fizzCompute :: forall a. Arrays a => FAcc a -> FAcc a
fizzCompute (FAcc s as) = FAcc s $ P.map (A.compute) as

-- λ> naiveEval $ foo1 arr
-- Acc 0 [let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0])
-- in map (\x0 -> 1.0 + x0) a0]


partialEval :: Rep a -> Rep a
partialEval (Return a) = Return a
partialEval (Bind b f) =
    let b' = partialEval b
    in case b' of
         Return a -> f a
         Use a -> Bind (Use a) f
         Bind b'' g -> Bind b'' $ repCompose g f
partialEval (Join f a b) = Join f (partialEval a) (partialEval b)
partialEval (Use a) = Use a

repCompose :: (a -> Rep b) -> (b -> Rep c) -> a -> Rep c
repCompose g f a =
    case g a of
      Return a -> f a
      Bind b f' -> Bind (partialEval (Bind b f')) f
      Join f' a b -> Bind (partialEval (Join f' a b)) f
      Use a -> Bind (Use a) f
