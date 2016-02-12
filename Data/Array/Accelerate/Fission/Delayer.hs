{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Array.Accelerate.Fission.Delayer
    (
     Acc(..)
    , A.Exp, (A.:.)(..)
    , Rep(..)
    , dosplit
    , TuneM, runTune
    , run', run
    , use
    , liftAcc
    )
    where



-- import Control.Monad
import           Control.Exception                          (assert)
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.List                                  as L
import           Data.Typeable
import           System.IO                                  (hPutStrLn, stderr)
import           System.IO.Unsafe                           (unsafePerformIO)
-- import Unsafe.Coerce
import           Prelude                                    hiding (concat, map,
                                                             replicate, zipWith)
import qualified Prelude                                    as P
-- import Data.Array.Accelerate                            ( DIM0, DIM1, DIM2, (:.)(..) )
import qualified Data.Array.Accelerate                      as A
import           Data.Array.Accelerate.Analysis.Match
import           Data.Array.Accelerate.Array.Sugar          hiding (Split, dim)
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA                 as B
#else
import qualified Data.Array.Accelerate.Interpreter          as B
#endif
import qualified Data.Array.Accelerate.Array.Representation as R
import           Data.Array.Accelerate.Fission.Util
import           Debug.Trace
import           Numeric.Natural

data Rep a = forall b. Pipe    (Rep b) (b -> a)
           | forall b. Split   (Rep b) (b -> (a,a))
           | forall b. Combine (Rep b) ((b,b) -> a)
           |           Unit    a

newtype Acc a m = MkAcc (m (Rep a))

type TuneM = IO

dosplit = undefined
runTune = undefined
run' = undefined
run = undefined
use = undefined
liftAcc = undefined


withSplit = undefined


map
  :: forall ix a b. (Shape ix, Elt a, Elt b) =>
     (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> Acc (Array ix b)
map f arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = map0 f arr
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = mapn f arr
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = mapn f arr
    | otherwise = map0 f arr
    where map0 :: (Shape ix, Elt a, Elt b) =>
                  (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> Acc (Array ix b)
          map0 f (MkAcc m) =
              MkAcc $ do
                r <- m
                doPipe f $ \a -> A.map f a
          mapn :: (Slice ix, Shape ix, Elt a, Elt b) =>
                  (A.Exp a -> A.Exp b)
               -> Acc (Array (ix :. Int) a)
               -> Acc (Array (ix :. Int) b)
          mapn f (MkAcc m) =
              MkAcc $ do
                r <- m
                withSplit f $ \a -> A.map f a
