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

type TuneM = IO
    
newtype Acc a = MkAcc (TuneM (Rep (A.Acc a)))

withSplit (Combine r g) f = do
  r' <- doPipe r f
  return $ Combine r' g
withSplit r f =
    return $ Combine (Pipe (Split r dosplit) f) doconcat

doPipe (Pipe r g) f = do
  r' <- doPipe r f -- trouble
  return $ Pipe r' g
doPipe r f = return $ Pipe r f


map :: forall ix a b. (Shape ix, Elt a, Elt b) =>
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
                doPipe r $ \a -> A.map f a
          mapn :: (Slice ix, Shape ix, Elt a, Elt b) =>
                  (A.Exp a -> A.Exp b)
               -> Acc (Array (ix :. Int) a)
               -> Acc (Array (ix :. Int) b)
          mapn f (MkAcc m) =
              MkAcc $ do
                r <- m
                withSplit r $ \a -> A.map f a

fold :: forall ix a. (Slice ix, Shape ix, Elt a) =>
        (A.Exp a -> A.Exp a -> A.Exp a)
     -> A.Exp a
     -> Acc (Array (ix :. Int) a)
     -> Acc (Array ix a)
fold f a arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = foldn f a arr
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = foldn f a arr
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = foldn f a arr
    | otherwise = foldn f a arr
    where foldn :: (Slice ix, Shape ix, Elt a) =>
                   (A.Exp a -> A.Exp a -> A.Exp a)
                -> A.Exp a
                -> Acc (Array (ix :. Int) a)
                -> (Acc (Array ix a))
          foldn f i (MkAcc m) = undefined

generate :: forall ix a. (Shape ix, Elt a, Slice ix) =>
            A.Exp ix
         -> (A.Exp ix -> A.Exp a)
         -> (Acc (Array ix a))
generate sh f
    | Just REFL <- matchShape (undefined :: Z)      (undefined :: ix) = generate0 sh f
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = generate1 sh f
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = generate1 sh f
    | otherwise = error "generate: haven't solved the N-dimensional general case."
    where generate0 :: (Elt a) => A.Exp Z -> (A.Exp Z -> A.Exp a) -> Acc (Array Z a)
          generate0 e f = undefined
          generate1 :: (Shape ix, Elt a, Slice ix) =>
                       A.Exp (ix A.:. Int)
                    -> (A.Exp (ix A.:. Int) -> A.Exp a)
                    -> (Acc (Array (ix A.:. Int) a))
          generate1 sh f = undefined

runTune = undefined
run' = undefined
run = undefined
use = undefined
liftAcc = undefined

dosplit :: (A.Slice sh,Shape sh,Elt a) =>
           A.Acc (Array (sh A.:. Int) a)
        -> (A.Acc (Array (sh A.:. Int) a),
             A.Acc (Array (sh A.:. Int) a))
dosplit arr = (arr1, arr2)
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

doconcat (a,b) = a A.++ b
