{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Array.Fission where

import Data.Array.Accelerate                                      as A hiding ( Acc, Split )
import qualified Data.Array.Accelerate                            as A
import Prelude                                                    as P hiding ( splitAt )

import System.Random.MWC
import Control.Monad.Reader


-- data Request a where
--     Request :: [Par a] -> Request [a]

-- data Par a where
--     Do   :: a -> Par a
--     Par  :: Par a -> Par b -> Par (a,b)
--     Join :: Par a -> Par b -> (a -> b -> Par c) -> Par c


-- type Acc a = [Par (A.Acc a)]


-- use :: Arrays arrs => arrs -> Acc arrs
-- use = return . Do . A.use

-- map :: (Shape sh, Elt a, Elt b)
--     => (Exp a -> Exp b)
--     -> Acc (Array sh a)
--     -> Acc (Array sh b)


map f xs = do
  xs' <- tune xs
  return (P.map (A.map f) xs')


type TuneM a = ReaderT GenIO IO a

tune :: (Splittable (A.Acc a), Arrays a) => A.Acc a -> TuneM [A.Acc a]
tune a = do
  action <- oneof [minBound .. maxBound]
  case action of
    NOP   -> return [a]
    Force -> return [ A.compute a ]
    Fizz  -> do
      s <- oneof splittable
      p <- splitpoint
      let (x,y) = splitAt s p a
      (P.++) <$> tune x <*> tune y

data Tune = NOP | Force | Fizz
  deriving (Bounded, Enum, Show)

splitpoint :: TuneM Double
splitpoint = do
  mwc   <- ask
  uniformR (0,1) mwc

oneof :: [a] -> TuneM a
oneof these = do
  mwc  <- ask
  this <- uniformR (0, P.length these - 1) mwc
  return (these P.!! this)



-- type Tune = IO

-- eval :: Par a -> Tune [a]
-- eval (Do x)    = return [x]
-- eval (Par x y) =

