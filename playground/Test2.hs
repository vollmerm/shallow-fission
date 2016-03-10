{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp          #-}

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
  One  :: Acc a          -> PAcc a
  Many :: Int -> [Acc a] -> PAcc a


{--
data P a where
  PFork :: (Arrays a, Arrays b)
        => P (Acc a)
        -> P (Acc b)
        -> P (PAcc a)

  PJoin :: (Arrays a, Arrays a)
        -> (Acc a -> Acc a -> Acc a)
        -> P (PAcc a)
        -> P (Acc a)

  PUse  :: Arrays a
        => a
        -> P (Acc a)
--}



{--}
data P a where
  PDo   :: (Arrays a, Arrays b)
        => (PAcc a -> PAcc b)
        -> P (PAcc a)
        -> P (PAcc b)

  PJoin :: (Arrays a, Arrays b, Arrays c)
        => (Acc a -> Acc b -> Acc c)
        -> P (Acc a)
        -> P (Acc b)
        -> P (Acc c)

  PUse  :: Arrays a
        => a
        -> P (Acc a)


pmap :: (Shape sh, Elt a, Elt b)
     => (Exp a -> Exp b)
     -> P (PAcc (Array sh a))
     -> P (PAcc (Array sh b))
pmap f = PDo f'
  where
    f' (One x)     = One (A.map f x)
    f' (Many s xs) = Many s (P.map (A.map f) xs)
--}

{--
pfold :: (Shape sh, Elt e)
      => (Exp e -> Exp e -> Exp e)
      -> Exp e
      -> P (PAcc (Array (sh :. Int) e))
      -> P (PAcc (Array sh e))
pfold f z = PDo join . PDo reduce
  where
    join (One x)       = One x
    join (Many dim xs) = case dim of
                           0 -> undefined
                           1 -> undefined

    reduce (One x)     = One (A.fold f z x)
    reduce (Many s xs) = Many s (P.map (A.fold f z) xs)

join0 f xs = PJoin (A.zipWith f)
--}

