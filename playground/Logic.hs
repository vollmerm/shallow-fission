
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Logic.Class

odds :: MonadPlus m => m Int
odds = return 1 `mplus` (odds >>= \k -> return (k+2))

t1, t2, t3 :: MonadPlus m => m Int
t1 = mzero
t2 = return 10 `mplus` return 20 `mplus` return 30
t3 = msum (map return [10,20,30])

t4 :: MonadLogic m => m Int
t4 = do
  x <- odds `interleave` t3
  if even x
    then return x
    else mzero

oddsPlus :: MonadPlus m => Int -> m Int
oddsPlus n = odds >>= \x -> return (x + n)

t5 :: MonadLogic m => m Int
t5 = do
  x <- (return 0 `mplus` return 1) >>- oddsPlus
  if even x
    then return x
    else mzero

