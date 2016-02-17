{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Array.Accelerate.Fission.Functions
    (
     replicate, zipWith, generate, fold, map, transpose
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
import           Data.Array.Accelerate.Fission.Runner
import           Debug.Trace
import           Numeric.Natural
import           Unsafe.Coerce




map1n :: (Slice ix, Shape ix, Elt a, Elt b)
      => (A.Exp a -> A.Exp b)
      -> Acc (Array (ix :. Int) a)
      -> Int
      -> Acc (Array (ix :. Int) b)
map1n f (MkWrap m) _n = MkWrap $ \numSplits exec ->
    do (Concat d ls) <- m numSplits exec
       case ls of
         [] -> error "Nothing to do."
         -- FIXME: Here we should probably introduce a split of the specific arity,
         -- but we can't do that yet so we only do two-way:
         [arr] | numSplits > 1 -> do
           dim     <- return 0 --askTuner [0..n-1]
           (a1,a2) <- dosplit dim arr -- TODO: multi-way split.
           let m1 = A.map f a1
               m2 = A.map f a2
           return $ Concat dim [m1,m2]
         as -> let as' = P.map (A.map f) as
               in return $ Concat d as'

map0 :: (Shape ix, Elt a, Elt b) =>
        (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> Acc (Array ix b)
map0 f (MkWrap m) = MkWrap $ \numSplits exec ->
  do Concat d as <- m numSplits exec
     -- Here we don't change the chunking of what comes back:
     let as' = P.map (A.map f) as
     return $ Concat d as'

map
  :: forall ix a b. (Shape ix, Elt a, Elt b) =>
     (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> Acc (Array ix b)
map f arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = map0 f arr
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = map1n f arr 1
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = map1n f arr 2
    | otherwise = map0 f arr



replicate :: forall slix e . (Slice slix, Elt e)
          => A.Exp slix
          -> Acc (Array (SliceShape slix) e)
          -> Acc (Array (FullShape slix) e)

replicate e (MkWrap fn) = MkWrap $ \numSplits exec ->
  -- TODO: Need a new policy here for how to split.
  do res <- fn numSplits exec
     case res of
       (Concat _ []) -> error "Nothing to do"

       (Concat dim [arr]) ->
         -- Two options here.
         --  (1) we can manifest a split kernel and use the pieces
         --  (2) we can split along an extruded dimension, for free.
         return $
         if True
            then let (dim',e1,e2) = splitExtruded e
                 in Concat dim' [ A.replicate e1 arr
                                , A.replicate e2 arr ]
            else -- Finally, this is the do-nothing approach:
                 Concat dim [A.replicate e arr]

       (Concat dim ls@(_:_:_)) ->
          let dim2 = adjustDim dim (A.sliceIndex (undefined::slix))
          in return $ Concat dim2 (L.map (A.replicate e) ls )

       _ -> error "replicate: unhandled cases"



zipWith  :: (Slice sh, Shape sh, Elt a, Elt b, Elt c) =>
          (A.Exp a -> A.Exp b -> A.Exp c)
          -> Acc (Array (sh :. Int) a)
          -> Acc (Array (sh :. Int) b)
          -> Acc (Array (sh :. Int) c)
zipWith f (MkWrap f1) (MkWrap f2) = MkWrap $ \numSplits exec -> do
  Concat d1 x <- f1 numSplits exec
  Concat _d2 y <- f2 numSplits exec
  case (x,y) of
    ([], _) -> error "Nothing to do"
    (_,[])  -> error "Nothing to do"
    ([m1], [m2]) | numSplits > 1 ->
      do dim <- return 0
         (m11,m12) <- dosplit dim m1
         (m21,m22) <- dosplit dim m2
         let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    ([m1], [m2]) ->
      return $ Concat d1 [A.zipWith f m1 m2]

    ([m1], [m21,m22])  ->
      -- do let m2 = (A.compute m21) A.++ (A.compute m22)
      --    return $ MkAcc $ Concat 0 [(A.zipWith f m1 m2)]
      do dim <- return 0
         (m11,m12) <-  dosplit dim m1
         let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    ([m11,m12], [m21,m22]) ->
      do let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    _ -> error "Not implemented"




-- Fold will *explicitly* call the exec function (A.run) before
-- zipWith'ing the two pieces together.
foldn :: (Slice ix, Shape ix, Elt a) =>
         (A.Exp a -> A.Exp a -> A.Exp a)
      -> A.Exp a
      -> Acc (Array (ix :. Int) a)
      -> Int
      -> (Acc (Array ix a))
foldn f i (MkWrap fn) _dims = MkWrap $ \numSplits exec ->
  do inputs <- fn numSplits exec
     case inputs of
       (Concat _ [])     -> error "Nothing to do"
       (Concat d [arr]) ->
         do dim     <- return 0
            (a1,a2) <- dosplit dim arr
            -- m1 and m2 cause a RUN!!!
            -- let m1 = A.use $ exec $ A.fold f i a1
            --     m2 = A.use $ exec $ A.fold f i a2
            let [m1',m2'] = P.map A.use $ exec [A.fold f i a1, A.fold f i a2]
                m3 = A.zipWith f m1' m2'
            return $ Concat d [m3]

       (Concat d [m1,m2]) ->
         -- m1 and m2 cause a RUN!!!
         -- let m1' = A.use $ exec $ A.fold f i m1
         --     m2' = A.use $ exec $ A.fold f i m2
           let [m1',m2'] = P.map A.use $ exec [A.fold f i m1, A.fold f i m2]
               m3  = A.zipWith f m1' m2'
         in return $ Concat d [m3]

       -- FIXME: no reason to throw away task parallelism here.
       -- Just mimick the 2-way case with an N-way one.
       (Concat _d _ms) -> error "Data.Array.Accelerate.Fusion.foldn: cannot intelligently fold more than 2 pieces"
         -- do let arr = foldr1 (A.++) ms
         --    dim     <- return 0
         --    (a1,a2) <- dosplit dim arr
         --    -- m1 and m2 cause a RUN!!!
         --    let m1 = A.use $ exec $ A.fold f i a1
         --        m2 = A.use $ exec $ A.fold f i a2
         --        m3 = A.zipWith f m1 m2
         --    return $ Concat d [m3]
       _ -> error "Data.Array.Accelerate.Fission.foldn: non-concat cases not handled"


fold :: forall ix a. (Slice ix, Shape ix, Elt a) =>
        (A.Exp a -> A.Exp a -> A.Exp a)
     -> A.Exp a
     -> Acc (Array (ix :. Int) a)
     -> Acc (Array ix a)
fold f a arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = foldn f a arr 1
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = foldn f a arr 2
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = foldn f a arr 3
    | otherwise = foldn f a arr 0



generate0 :: (Elt a) => A.Exp Z -> (A.Exp Z -> A.Exp a) -> Acc (Array Z a)
-- Cannot meaningfully split zero dim:
generate0 e f = MkWrap $ \_numSplits _dev -> return $
  Concat 0 [ A.generate e f ]

generate :: forall ix a. (Shape ix, Elt a, Slice ix) => A.Exp ix -> (A.Exp ix -> A.Exp a)
              -> (Acc (Array ix a))
generate sh f
    | Just REFL <- matchShape (undefined :: Z)      (undefined :: ix) = generate0 sh f
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = generate1 sh f
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = generate1 sh f
    | otherwise = error "generate: haven't solved the N-dimensional general case."

generate1 :: (Shape ix, Elt a, Slice ix) => A.Exp (ix A.:. Int) -> (A.Exp (ix A.:. Int) -> A.Exp a)
          -> (Acc (Array (ix A.:. Int) a))
-- Here is where we should see the difference with the "demand driven"
-- approach.  We directly create the generate in the right
-- granularity.  The alternative would be to include Generate
-- expliticly in Rep, in which case optimizing `(Split . Generate)`
-- would be trivial.
generate1 sh f = MkWrap $ \numSplits dev ->
  if numSplits == 1
   then return $ Concat 0 [A.generate sh f]
   else
    assert (numSplits == 2) $ do
     let arrHd = A.indexHead sh
         arrTl = A.indexTail sh
     -- Different ways to do this:
     -- Can ask for N choices within the same range, and then sort.
     -- Or can make a series of dependent choices chopping up the
     -- remaining space.  The former works better if answers are, say,
     -- uniformly random.  The latter is potentially "left biased".
     -- splits <- sequence $ L.replicate (numSplits-1) $ askTuner ...

     -- But really we want to weight things towards even chunks...
     -- (chunkSz,remain) = A.indexHead e `quotRem` A.constant numSplitsa

     (ln1,ln2) <- askTunerSplit arrHd

     let arr1Sh = arrTl :. ln1
         adjust i = let t = A.indexTail i
                        h = A.indexHead i
                    in A.lift $ t :. (h + ln1)
         arr1 = A.generate (A.lift arr1Sh) f
     return $
        -- TODO: generalize to N-way:
        Concat 0 (arr1 :
                 [ A.generate (A.lift arr2Sh) $ f . adjust
                 | ln <- [ln2]
                 , let arr2Sh = arrTl :. ln
                 ])



-- FIXME: Transpose is currently not fissioned:
transpose :: Elt e => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
transpose (MkWrap fn) = MkWrap $ \_numSplits exec ->
  -- Here we tell the upstream to give us one chunk:
  do inp <- fn 1 exec
     case inp of
       (Concat _ arrs)  -> let [arr] = arrs in
                           return $ Concat 0 [A.transpose arr]
       -- Policy choice.  We could throw away the split, or maintain it:
       (Split 0 arr)    -> return $ Split 1  (A.transpose arr)
       (Split 1 arr)    -> return $ Split 0  (A.transpose arr)

