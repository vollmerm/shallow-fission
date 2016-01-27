module Data.Split where

import           Control.Monad
import           Control.Monad.Reader
import qualified Data.List            as L
import           Prelude              hiding (concat, map, replicate, zipWith)
import qualified Prelude              as P
import           System.IO            (hPutStrLn, stderr)


type TuneM a = ReaderT [(String,Int)] IO a

type NumSplits = Int
type SplitBy   = Int

newtype Wrap a b = MkWrap (NumSplits -> TuneM (Rep b a))

--------------------------------------------------------------------------------
-- Shallow Language of fissionable computations
--------------------------------------------------------------------------------

-- | The language of multi-device computations.
--
-- The chunked nature of a computation does not affect its type.
-- Thus, for example, `Split (Split a)` is not a different type than `Split a`.
--
-- Concatenation flattens any structure and goes back to a single chunk.
data Rep b a = Concat SplitBy [a]
             | Split  SplitBy a
             | Compute (Rep b a)
             -- Device selection (?)
             -- | IfDevice b (Rep b a) (Rep b a)
             -- etc


----------------------------------------
-- Smart constructors:
----------------------------------------

mkConcat :: SplitBy -> Rep b a -> Rep b a -> Rep b a
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


mkSplit :: SplitBy -> Rep b a -> Rep b a
mkSplit d1 rep =
    case rep of
      (Concat d2 _ls)
          | d1 == d2 -> rep
          | otherwise -> error "mkSplit/unfinished"
      (Split _ ar) -> Split d1 ar

mkCompute :: Rep b a -> Rep b a
mkCompute (Compute rep) = Compute rep
mkCompute rep = Compute rep

