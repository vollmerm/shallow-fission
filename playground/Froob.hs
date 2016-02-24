{-# LANGUAGE GADTs #-}

import Data.Array.Accelerate as A hiding ( Split )
import Prelude               as P


-- map f as
--   = P.map (A.map f) as


-- fizzmap f arr =
--   [ case arr of
--       Compute s as   -> Compute s (P.map ((Unit . A.map f) .) as)
--       -- Join s as join -> Join s as (fizzmap f . join)
--   ]


concat :: [Acc a] -> Acc a
concat = undefined

data FAcc a where
  Unit    :: Acc a -> FAcc a
  Compute :: Split a -> [Acc a -> FAcc b]                     -> FAcc (a -> b)
  Join    :: FAcc (a -> b) -> (Split b -> Acc b -> FAcc c) -> FAcc (a -> c)


data Split a = Split
  { splitAt  :: Double -> a -> (a,a)
  , combine  :: a -> a -> a
  , splitDim :: Int
  }
  --
  -- Bind   :: (Acc a -> FAcc b) -> FAcc a -> FAcc b


-- autotune :: Devices -> FAcc (a -> b) -> a -> b
-- autotune gpus (Join s f j) = \as ->
--   let f' = P.zipWith run1With gpus f


