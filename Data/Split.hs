{-# LANGUAGE DeriveFunctor #-}
module Data.Split where

import           Control.Monad
import           Control.Monad.Reader
import qualified Data.List            as L
import           Numeric.Natural
import           Prelude              hiding (concat, map, replicate, zipWith)
import qualified Prelude              as P
import           System.IO            (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- Shallow Language of fissionable computations
--------------------------------------------------------------------------------

-- | The language of multi-device computations.

data Rep b c a = Concat c [a]
               | Split c a
               | Compute a
               | Branch b (Rep b c a) (Rep b c a)
               | Unit a
                 deriving Functor

newtype Wrap b c a m = MkWrap (Natural -> m (Rep b c a))

----------------------------------------
-- Smart constructors:
----------------------------------------

mkConcat :: (Ord c) => c -> Rep b c a -> Rep b c a -> Rep b c a
mkConcat d3 x y =
    case (x,y) of
      ((Concat d1 ls1),(Concat d2 ls2))
          | d1 == d2 && d1 == d3 -> Concat d3 (ls1 ++ ls2)

      -- In the remaining cases, Splits get eaten by concats:
      ((Split _ a),(Split _ b)) -> Concat d3 [a,b]
      ((Concat d1 as),(Split _ b))
          | d1 == d3 -> Concat d3 (as ++ [b])
      ((Split _ a),(Concat d1 bs))
          | d1 == d3 -> Concat d3 (a : bs)

      _ -> error "mkConcat: Brain explodes for now..."


mkSplit :: (Ord c) => c -> Rep b c a -> Rep b c a
mkSplit d1 rep =
    case rep of
      (Concat d2 _ls)
          | d1 == d2 -> rep
          | otherwise -> error "mkSplit/unfinished"
      (Split _ ar) -> Split d1 ar
      (Compute ar) -> Split d1 ar
      (Branch b a1 a2) -> Branch b (mkSplit d1 a1) (mkSplit d1 a2)
      (Unit ar) -> Split d1 ar

mkCompute :: Rep b c a -> Rep b c a
mkCompute x =
    case x of
      (Compute rep) -> Compute rep
      _ -> x
