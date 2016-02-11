{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Array.Accelerate.Fission.Util
    ( matchShape, splitExtruded, adjustDim, askTunerSplit )
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


