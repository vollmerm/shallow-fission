{-# LANGUAGE GADTs                     #-}
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


data P a where
  PReturn :: Arrays a
          => [Acc a]
          -> P [Acc a]

  PDo :: (Arrays a, Arrays b)
      => ([Acc a] -> P [Acc b])
      -> P [Acc a]
      -> P [Acc b]


pmap f = PDo f'
  where
    f' = PReturn . P.map (A.map f)






-- Representation where we have selected which chunking strategy to use.
--
data S a where
  SDo :: (Arrays a, Arrays b)
      => (Acc a -> Acc b)
      -> S (Acc a)
      -> S (Acc b)

  SJoin :: (Arrays a, Arrays b, Arrays c)
        => (Acc a -> Acc b -> Acc c)
        -> S (Acc a)
        -> S (Acc b)
        -> S (Acc c)

  -- are we going to need projections (inl, inr)? \:
  SFork :: (Arrays a, Arrays b)
        => S (Acc a)
        -> S (Acc b)
        -> S (Acc a, Acc b)

  SUse  :: (Arrays a, Show a)
        => a
        -> S (Acc a)


-- Representation with the choice of executor embedded in it.
--
data E a where
  EDo :: (Arrays a, Arrays b)
      => (a -> b)
      -> E a
      -> E b

  EJoin :: (Arrays a, Arrays b, Arrays c)
        => (a -> b -> c)
        -> E a
        -> E b
        -> E c

  EFork :: (Arrays a, Arrays b)
        => E a
        -> E b
        -> E (a, b)

  EUse  :: Arrays a
        => a
        -> E a

