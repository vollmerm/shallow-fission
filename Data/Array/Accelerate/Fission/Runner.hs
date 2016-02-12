{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Array.Accelerate.Fission.Runner
    (
     Acc
    , A.Exp, (A.:.)(..)
    , Rep(..), Wrap(..)
    , matchShape, splitExtruded, adjustDim, askTunerSplit
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

-- | The language of multi-device computations.

data Rep a = Concat [a]
           | Split a

newtype Wrap a m = MkWrap (Natural -> Exec -> m (Rep a))

type TuneM a = ReaderT [(String,Int)] IO a

type Acc a = Wrap (A.Acc a) (ReaderT [(String,Int)] IO)

type Exec = forall a. (Arrays a) => (A.Acc a) -> a

--mkacc :: (Natural -> ((A.Acc a) -> b -> a) -> TuneM (Rep Devs SplitBy (A.Acc a))) -> Acc a
--mkacc = MkWrap

type SplitBy = Int
type DimId = SplitBy

instance (Show a, A.Arrays a) => Show (Rep (A.Acc a)) where
  show (Concat ls) =
      "(Concat of "++ show (length ls)++" chunks:\n" ++
                         unlines [ show x | x <- ls ]++")"
  show (Split a) =
     "(Split of "++ show a++")"

runTune :: TuneM a -> IO a
runTune f = runReaderT f [("split",2)]

-- | This creates an actual split kernel.  We RARELY want to use this.
--   Rather, we want to NEVER create the unchunked versions in the first place
--   so as to not need this.
dosplit :: (A.Slice sh,Shape sh,Elt a)
        => DimId
        -> A.Acc (Array (sh A.:. Int) a)
        -> TuneM ( A.Acc (Array (sh A.:. Int) a),
                   A.Acc (Array (sh A.:. Int) a))
dosplit _dimid arr = return (arr1, arr2)
    where arrTl = A.indexTail $ A.shape arr
          arrHd = A.indexHead $ A.shape arr
          (chunk, leftover) = arrHd `quotRem` 2
          arr1Sh = arrTl :. chunk
          arr2Sh = arrTl :. (chunk + leftover)
          adjust i = let t = A.indexTail i
                         h = A.indexHead i
                     in A.lift $ t :. (h + chunk)
          arr1 = A.generate (A.lift arr1Sh) (\sh -> arr A.! sh)
          arr2 = A.generate (A.lift arr2Sh) (\sh -> arr A.! adjust sh)

go1 :: (Slice sh, Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array (sh :. Int) a)
    -> [Array (sh :. Int) a]
go1 exec (MkWrap fn) =
  unsafePerformIO $
   do rep <- runTune $ fn 2 $ \a -> exec a
      putStrLn ("Fission/RUN1: shallow-language term:\n" ++ show rep)
      case rep of
        (Concat arrs)
          | null arrs    -> error "Data.Array.Accelerate.Fusion.go1: nothing to do"
          | otherwise    -> return $! P.map exec arrs
        (Split arr) -> return $! [exec arr]

go0 :: (Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array sh a)
    -> [Array sh a]
go0 exec (MkWrap fn) =
  unsafePerformIO $
  do rep <- runTune $ fn 2 $ \a -> exec a
     putStrLn ("Fission/RUN0: shallow-language term:\n" ++ show rep)
     case rep of
      (Concat arrs)
        | null arrs    -> error "Data.Array.Accelerate.Fusion.go0: nothing to do"
        | [a] <- arrs  -> return $! [exec a]
        | otherwise    -> error "Data.Array.Accelerate.Fusion.go0: not implemented yet"
      (Split arr) -> return $! [exec arr]


run :: forall sh a. (Slice sh, Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array sh a)
    -> [Array sh a]
run exec arrs
  | Just REFL <- matchShape (undefined :: DIM0) (undefined :: sh) = go0 exec arrs
  | Just REFL <- matchShape (undefined :: DIM1) (undefined :: sh) = go1 exec arrs
  | Just REFL <- matchShape (undefined :: DIM2) (undefined :: sh) = go1 exec arrs
  | otherwise                                                     = go0 exec arrs

run' :: forall sh a.
        (Elt a, Slice sh, Shape sh) =>
        Acc (Array sh a) -> [Array sh a]
run' arrs = run B.run arrs

use :: Arrays arrays => (arrays,arrays) -> Acc arrays
use (a,b) = MkWrap $ \numSplits _runner ->
            do case numSplits of
                 2 -> return $ Concat [A.use a, A.use b]
                 _ -> error "Data.Array.Accelerate.Fusion.use: not handled yet"



-- FIXME: This should probably introduce a split node.
--liftAcc :: A.Acc a -> Acc a
liftAcc :: A.Acc a -> Acc a
liftAcc a = MkWrap $ \_ _ -> return $ Concat [a]
