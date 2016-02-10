{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
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
import           Debug.Trace
import           Numeric.Natural

-- | The language of multi-device computations.

data Rep c a = Concat c [a]
               | Split c a
                 deriving Functor

newtype Wrap c a m = MkWrap (Natural -> Exec a -> m (Rep c a))

type TuneM a = ReaderT [(String,Int)] IO a

type Acc a = Wrap SplitBy (A.Acc a) (ReaderT [(String,Int)] IO)

type Exec a = forall a. (Arrays a) => (A.Acc a) -> a

--mkacc :: (Natural -> ((A.Acc a) -> b -> a) -> TuneM (Rep Devs SplitBy (A.Acc a))) -> Acc a
--mkacc = MkWrap

type SplitBy = Int
type DimId = SplitBy

instance (Show a, A.Arrays a) => Show (Rep Int (A.Acc a)) where
  show (Concat d ls) =
      "(Concat along dim "++ show d++" of "++ show (length ls)++" chunks:\n" ++
                         unlines [ show x | x <- ls ]++")"
  show (Split d a) =
     "(Split along dim "++show d++ " of "++ show a++")"

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

matchShape
  :: forall sh sh'. (Shape sh, Shape sh')
  => sh
  -> sh'
  -> Maybe (sh :=: sh')
matchShape _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh))
                                (eltType (undefined::sh'))
  = gcast REFL

  | otherwise
  = Nothing


go1 :: (Slice sh, Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array (sh :. Int) a)
    -> [Array (sh :. Int) a]
go1 exec (MkWrap fn) =
  unsafePerformIO $
   do rep <- runTune $ fn 2 $ \a -> exec a
      putStrLn ("Fission/RUN1: shallow-language term:\n" ++ show rep)
      case rep of
        (Concat dim arrs)
          | null arrs    -> error "Data.Array.Accelerate.Fusion.go1: nothing to do"
          | dim /= 0     -> error "Data.Array.Accelerate.Fusion.go1: I only know about dimension 0"
          | otherwise    -> return $! P.map exec arrs
        (Split _dim arr) -> return $! [exec arr]

go0 :: (Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array sh a)
    -> [Array sh a]
go0 exec (MkWrap fn) =
  unsafePerformIO $
  do rep <- runTune $ fn 2 $ \a -> exec a
     putStrLn ("Fission/RUN0: shallow-language term:\n" ++ show rep)
     case rep of
      (Concat _ arrs)
        | null arrs    -> error "Data.Array.Accelerate.Fusion.go0: nothing to do"
        | [a] <- arrs  -> return $! [exec a]
        | otherwise    -> error "Data.Array.Accelerate.Fusion.go0: not implemented yet"
      (Split _dim arr) -> return $! [exec arr]


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
                 2 -> return $ Concat 0 [A.use a, A.use b]
                 _ -> error "Data.Array.Accelerate.Fusion.use: not handled yet"



-- combine :: (Slice sh, Shape sh, Elt e) => Acc (Array (sh :. Int) e)
--         -> TuneM (A.Acc (Array (sh :. Int) e))
-- combine (MkWrap fn) =
--   trace ("FYI: CALLING Fission.combine") $
--   do res <- fn 2
--      lift $ do hPutStrLn stderr (L.replicate 80 '=')
--                hPutStrLn stderr ("combine: shallow-language term:\n" ++ show res)
--                hPutStrLn stderr (L.replicate 80 '=')
--      return $ case res of
--                (Concat 0 [a]) -> a
--                (Concat 0 as)  -> foldr1 (A.++) as
--                _ -> error "combine: unfinished cases"




askTunerSplit ::
  (Ord a, MonadReader [([Char], a)] m, A.IsIntegral a, Elt a) =>
  A.Exp a -> m (A.Exp a, A.Exp a)
askTunerSplit hd = do
  params <- ask
  let splitP = case lookup "split" params of
                 Nothing -> 2
                 Just e  -> e
      (chunk, leftover) = quotRem hd $ A.constant splitP
  return $ if splitP > 1
           then (chunk, (chunk*(A.constant (splitP-1)))+leftover)
           else if splitP < -1
                then (chunk*(A.constant ((abs splitP)-1)), chunk+leftover)
                else error "Can't split like that"




-- | Find the first extruded dimension and split that roughly in half.
--   Return two new slice expressions as well as the integer index of the dimension split.
splitExtruded :: forall ix. Slice ix => A.Exp ix -> (Int, A.Exp ix,A.Exp ix)
splitExtruded orig =
   go (0::Int) orig (A.sliceIndex (undefined::ix))
  where
   go :: forall ix0 s c d . Slice ix0 =>
         Int -> A.Exp ix0 -> R.SliceIndex (EltRepr ix0) s c d
      -> (Int, A.Exp ix0, A.Exp ix0)
   go _ _ R.SliceNil = undefined
   go d e (R.SliceFixed _) =
            caseSliceFixed e
               (let hd = A.indexHead e
                    tl = A.indexTail e
                    (q,r) = quotRem hd 2
                in (d, sliceSnoc (q+r) tl,
                       sliceSnoc q tl))
               (error "splitExtruded: impossible")
   -- Original rather than extruded dim, keep going:
   go d e (R.SliceAll rest) =
     caseSliceAll e
       (let hd = A.indexHead e
            tl = A.indexTail e
            (d2,sl1,sl2) = go (d+1) tl rest
        in (d2, sliceSnoc hd sl1,
                sliceSnoc hd sl2))
       (error "splitExtruded: impossible")

sliceSnoc :: forall sh a . (Slice sh, Elt a) =>
             A.Exp a -> A.Exp sh -> A.Exp (sh :. a)
sliceSnoc ea esh = A.lift (esh :. ea)


caseSliceFixed :: forall ix b . A.Slice ix
               => A.Exp ix
               -> (forall ix0 . (Slice ix0, ix ~ (ix0 A.:. Int)) => b)
               -> b -> b
caseSliceFixed = error "caseSliceFixed - Trevor can probably figure out how to implement this"

caseSliceAll :: forall ix b . A.Slice ix
               => A.Exp ix
               -> (forall ix0 . (Slice ix0, ix ~ (ix0 A.:. All)) => b)
               -> b -> b
caseSliceAll = error "caseSliceAll - Trevor can probably figure out how to implement this"

-- | The dimension we concat on may slide over due to insertion of new dims.
adjustDim :: Int -> R.SliceIndex ix slice coSlice sliceDim -> Int
adjustDim _ R.SliceNil   = error "adjustDim: overran the dimensions."
adjustDim d (R.SliceFixed r) = adjustDim d r
adjustDim 0 (R.SliceAll _)   = 0
adjustDim d (R.SliceAll r)   = 1 + adjustDim (d-1) r


-- FIXME: This should probably introduce a split node.
--liftAcc :: A.Acc a -> Acc a
liftAcc :: A.Acc a -> Acc a
liftAcc a = MkWrap $ \_ _ -> return $ Concat 0 [a]
