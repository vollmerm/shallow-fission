{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Array.Accelerate.Fission where

import           Control.Monad
import           Data.Array.Accelerate as A hiding (Acc, Divide, Split)
import qualified Data.Array.Accelerate as A
import           Data.Split
import           Prelude               as P
import           Text.Printf

data Rep a where
    Return :: (Arrays a)
           => (FAcc a) -> Rep (FAcc a)
    Bind   :: (Arrays a, Arrays b)
           => Rep (FAcc b) -> (FAcc b -> Rep (FAcc a)) -> Rep (FAcc a)
    Join   :: (Arrays a, Arrays b, Arrays c)
           => (Rep (FAcc b) -> Rep (FAcc c) -> Rep (FAcc a))
           -> Rep (FAcc b) -> Rep (FAcc c) -> Rep (FAcc a)
    Use    :: (Arrays a)
           => (FAcc a) -> Rep (FAcc a) -- avoid collapsing use with bind


data FAcc a where
    FAcc :: Int -> [A.Acc a] -> FAcc a
    deriving Show

type Acc a = Rep (FAcc a)


-- instance Show a => Show (Rep a) where
--     show (Return a) = printf "(Return %s)" $ show a
--     show (Bind b f) = printf "(Bind %s %s)" (show b) (show f)
fizzCompute :: Arrays a => FAcc a -> FAcc a
fizzCompute (FAcc s as) = FAcc s $ P.map (A.compute) as


computeEval :: Rep a -> a
computeEval (Return a)   = a
computeEval (Bind b f)   = computeEval $ f $ fizzCompute (computeEval b)
computeEval (Join f a b) = computeEval $ f a b
computeEval (Use a)      = a

nocomputeEval :: Rep a -> a
nocomputeEval (Return a)   = a
nocomputeEval (Bind b f)   = computeEval $ f (computeEval b)
nocomputeEval (Join f a b) = computeEval $ f a b
nocomputeEval (Use a)      = a



fizzMap :: (Shape sh, Elt a, Elt b) =>
           (Exp a -> Exp b)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
fizzMap f a = Bind a f'
    where f' (FAcc s as) = Return $ FAcc s $ P.map (A.map f) as

fizzZipWith' :: (Shape sh, Elt a, Elt b, Elt c) =>
                (Exp a -> Exp b -> Exp c)
             -> Acc (Array sh a)
             -> Acc (Array sh b)
             -> Acc (Array sh c)
fizzZipWith' f a1 a2 =
    Bind a1 (\a1 -> (Bind a2 (\a2 -> f' a1 a2)))
    where f' (FAcc s1 a1) (FAcc _s2 a2) =
              Return $ FAcc s1 $ P.zipWith (A.zipWith f) a1 a2

arr :: Acc (Array DIM2 Float)
arr = Use $ FAcc 0 [use $ A.fromList (Z :. 10 :. 10) [0..]]
-- arr = Use $ FAcc 0 [use $ A.fromList (Z :. 10 :. 10) [0..],
--                     use $ A.fromList (Z :. 10 :. 10) [0..]]

foo1 :: (Shape sh) => Acc (Array sh Float) -> Acc (Array sh Float)
foo1 as = fizzMap (+ 1) as
-- Bind (Use a) (\a -> Return (map (+ 1) a))

foo2 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh Float)
foo2 as = fizzMap (* 2) (foo1 as)
-- Bind (Bind (Use a) (\a -> Return (map (+ 1) a))) (\a -> (Return map (* 2) a))

fooz :: Shape sh => Acc (Array sh Float) -> Acc (Array sh Float)
fooz as = Join (fizzZipWith' (+)) (foo2 as) (foo1 as)



partialEval :: Rep a -> Rep a
partialEval (Return a) = Return a
partialEval (Bind b f) =
    let b' = partialEval b
    in case b' of
         Return a -> f a
         Use a -> Bind (Use a) f
         Bind b'' g -> Bind b'' $ repCompose g f
         _ -> Bind b' f
partialEval (Join f a b) = Join f (partialEval a) (partialEval b)
partialEval (Use a) = Use a

repCompose :: (Arrays c) => (FAcc a -> Rep (FAcc b)) -> (FAcc b -> Rep (FAcc c))
           -> FAcc a -> Rep (FAcc c)
repCompose g f a =
    case g a of
      Return a' -> f a'
      Bind b f' -> Bind (partialEval (Bind b f')) f
      Join f' a' b -> Bind (partialEval (Join f' a' b)) f
      Use a' -> Bind (Use a') f
--      b -> Bind b f
