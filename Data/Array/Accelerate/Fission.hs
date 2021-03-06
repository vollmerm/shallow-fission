{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}

module Data.Array.Accelerate.Fission
       (
         -- * The (Fissioned) Array language
         Acc
       , replicate, zipWith, generate, fold, map

       -- * Derived Array operations (the Prelude)
       , transpose

       -- * Accelerate expression language, reexported
       , A.Exp, (A.:.)(..)
       -- TODO: need an efficient way to list the whole Exp language:

       -- * Extra utilities for working with chunked/fissioned data:
       , liftAcc, combine
       , mkSplit, mkConcat, dosplit, doConcat

       -- * Temporary, will move elsewhere
       , TuneM, runTune2

       -- * Temporary tests:
       , arr0, ac1, ac2, ac3
       , run', run -- TODO: replace this with a run-builder parameterized over backends.
       )
       where

-- import Control.Monad
import           System.IO (stderr, hPutStrLn)
import qualified Data.List as L
import           Control.Exception (assert)
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Monad
import           Control.Monad.Reader
import           Data.Typeable
-- import Unsafe.Coerce
import           Prelude hiding ( concat, replicate, zipWith, map )
import qualified Prelude as P
-- import Data.Array.Accelerate                            ( DIM0, DIM1, DIM2, (:.)(..) )
import           Data.Array.Accelerate.Analysis.Match
import           Data.Array.Accelerate.Array.Sugar hiding (dim, Split)
import qualified Data.Array.Accelerate as A
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as B
#else
import qualified Data.Array.Accelerate.Interpreter      as B -- For testing.
#endif
import           Debug.Trace
import qualified Data.Array.Accelerate.Array.Representation as R
import           Data.Split
import           Numeric.Natural
    
type TuneM a = ReaderT [(String,Int)] IO a

runTune2 :: TuneM a -> IO a
runTune2 f = runReaderT f [("split",2)]

data Devs = Dev1 | Dev2
          deriving Show

type Acc a = Wrap Devs SplitBy (A.Acc a) (ReaderT [(String,Int)] IO)

mkacc :: (Natural -> TuneM (Rep Devs SplitBy (A.Acc a))) -> Acc a
mkacc = MkWrap

type SplitBy = Int
type DimId = SplitBy

instance (Show b, Show a, A.Arrays a) => Show (Rep b Int (A.Acc a)) where
  show (Concat d ls) =
      "(Concat along dim "++ show d++" of "++ show (length ls)++" chunks:\n" ++
                         unlines [ show x | x <- ls ]++")"
  show (Split d a) =
     "(Split along dim "++show d++ " of "++ show a++")"

--------------------------------------------------------------------------------
-- Utilities

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

    --   return (splitArray (A.constant 0), splitArray (A.constant 1))
    -- where splitArray i =
    --           let shead             = A.indexHead $ A.shape arr
    --               (chunk, leftover) = shead `quotRem` 2
    --               start             = (i A.<* leftover) A.?
    --                                   (i * (chunk + 1),
    --                                    i * chunk + leftover)
    --               end               = ((i+1) A.<* leftover) A.?
    --                                   (start + chunk,
    --                                    (i+1) * chunk + leftover)
    --               bsh               = A.lift $ (A.indexTail $ A.shape arr) A.:. (end - start)
    --               f x               = A.lift $ (A.indexTail x) A.:. ((A.indexHead x) + start)
    --           in A.backpermute bsh f arr


doConcat :: t
doConcat = undefined

askTuner :: [Int] -> TuneM Int
askTuner ls = return $ head ls

_matchArrayShape
    :: forall acc aenv sh sh' e. (Shape sh, Shape sh')
    => {- dummy -} acc aenv (Array sh e)
    -> {- dummy -} sh'
    -> Maybe (sh :=: sh')
_matchArrayShape _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh)) (eltType (undefined::sh'))
  = gcast REFL

  | otherwise
  = Nothing

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

_matchSlice --- ?????
  :: forall sh sh'. (Slice sh, Slice sh')
  => sh
  -> sh'
  -> Maybe (sh :=: sh')
_matchSlice _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh))
                                (eltType (undefined::sh'))
  = gcast REFL

  | otherwise
  = Nothing

-- FIXME: This should probably introduce a split node.
--liftAcc :: A.Acc a -> Acc a
liftAcc a = mkacc $ \_ -> return $ Concat 0 [a]

--------------------------------------------------------------------------------
-- RUNNING
--------------------------------------------------------------------------------

go1 :: (Slice sh, Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array (sh :. Int) a)
    -> Array (sh :. Int) a
go1 exec (MkWrap fn) =
  unsafePerformIO $
   do rep <- runTune2 $ fn 2
      putStrLn ("Fission/RUN1: shallow-language term:\n" ++ show rep)
      case rep of
        (Concat dim arrs)
          | null arrs   -> error "Data.Array.Accelerate.Fusion.go1: nothing to do"
          | dim /= 0    -> error "Data.Array.Accelerate.Fusion.go1: I only know about dimension 0"
          | otherwise   -> return $! exec $ foldr1 (A.++) arrs

go0 :: (Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array sh a)
    -> Array sh a
go0 exec (MkWrap fn) =
  unsafePerformIO $
  do rep <- runTune2 $ fn 2
     putStrLn ("Fission/RUN0: shallow-language term:\n" ++ show rep)
     case rep of
      (Concat _ arrs)
        | null arrs   -> error "Data.Array.Accelerate.Fusion.go0: nothing to do"
        | [a] <- arrs -> return $! exec a
        | otherwise   -> error "Data.Array.Accelerate.Fusion.go0: not implemented yet"


run :: forall sh a. (Slice sh, Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array sh a)
    -> Array sh a
run exec arrs
  | Just REFL <- matchShape (undefined :: DIM0) (undefined :: sh) = go0 exec arrs
  | Just REFL <- matchShape (undefined :: DIM1) (undefined :: sh) = go1 exec arrs
  | Just REFL <- matchShape (undefined :: DIM2) (undefined :: sh) = go1 exec arrs
  | otherwise                                                     = go0 exec arrs

run' :: forall sh a.
        (Elt a, Slice sh, Shape sh) =>
        Acc (Array sh a) -> Array sh a
run' arrs = run B.run arrs

--------------------------------------------------------------------------------
-- Wrappers for Core Accelerate operations
--------------------------------------------------------------------------------

map1n :: (Slice ix, Shape ix, Elt a, Elt b)
      => (A.Exp a -> A.Exp b)
      -> Acc (Array (ix :. Int) a)
      -> Int
      -> Acc (Array (ix :. Int) b)
map1n f (MkWrap m) n = mkacc $ \numSplits ->
    do (Concat d ls) <- m numSplits
       case ls of
         [] -> error "Nothing to do."
         -- FIXME: Here we should probably introduce a split of the specific arity,
         -- but we can't do that yet so we only do two-way:
         [arr] | numSplits > 1 -> do
           dim     <- askTuner [0..n-1]
           (a1,a2) <- dosplit dim arr -- TODO: multi-way split.
           let m1 = A.map f a1
               m2 = A.map f a2
           return $ Concat dim [m1,m2]
         as -> let as' = P.map (A.map f) as
               in return $ Concat d as'

map0 :: (Shape ix, Elt a, Elt b) =>
        (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> Acc (Array ix b)
map0 f (MkWrap m) = mkacc $ \numSplits ->
  do Concat d as <- m numSplits
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

-- Doesn't work
-- replicaten :: (Slice slix, Elt e)
--            => A.Exp slix
--            -> Acc (Array (A.SliceShape slix) e)
--            -> Int
--            -> TuneM (Acc (Array (A.FullShape  slix) e))
-- replicaten e (MkAcc (Concat _ [])) n = error "Nothing to do"
-- replicaten e (MkAcc (Concat d [arr])) n =
--     do dim     <- askTuner [0..n-1]
--        (a1,a2) <- split dim arr
--        let m1 = A.replicate e a1
--            m2 = A.replicate e a2
--        return $ MkAcc $ Concat d [m1,m2] -- Is this even right?
-- replicaten _ _ _ = error "not implemented at all"

-- replicate :: forall ix e. (Slice ix, Elt e)
--           => A.Exp ix
--           -> Acc (Array (A.SliceShape ix) e)
--           -> TuneM (Acc (Array (A.FullShape ix) e))
-- replicate e arr
--     | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = error "don't care"
--     | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = replicaten e arr 1
--     | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = replicaten e arr 2
--     | otherwise = error "still don't care"


-- Here's the type we WANT, to match `A.replicate`
replicate :: forall slix e . (Slice slix, Elt e)
          => A.Exp slix
          -> Acc (Array (SliceShape slix) e)
          -> Acc (Array (FullShape slix) e)

replicate e (MkWrap fn) = mkacc $ \numSplits ->
  -- TODO: Need a new policy here for how to split.
  do res <- fn numSplits
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


-- Here's the type we ended up with on Michael's first try:
{-
replicate :: forall slix a sh e .
             (A.IsIntegral a, Slice slix, Slice sh, Shape (sh :. a), Elt e, Elt a,
              FullShape slix ~ (sh :. a))
          => A.Exp slix
          -> Acc (Array (SliceShape slix) e)
          -> Acc (Array (sh :. a) e)
-}
-- Michael's first attempt:
{-

       (Concat dim [arr])
--         | isExtruded translated_dim (sliceIndex (undefined::ix)) ->
--           error "replicate: doesn't yet support fissioning in extruded dimension"
         | otherwise ->
         -- generate (A.shape arr') (\s -> arr' A.! s)
         -- where arr' = A.replicate e arr
         let arr'   = A.replicate e arr
             shap   = A.shape arr'
             arrHd  = A.indexHead shap
             arrTl  = A.indexTail shap
             (chunk, leftover) = arrHd `quotRem` 2
             arr1Sh = arrTl :. chunk
             arr2Sh = arrTl :. (chunk + leftover)
             adjust i = let t = A.indexTail i
                            h = A.indexHead i
                        in A.lift $ t :. (h + chunk)
             arr1   = A.generate (A.lift arr1Sh) (\sh -> arr' A.! sh)
             arr2   = A.generate (A.lift arr2Sh) (\sh -> arr' A.! (adjust sh))
         in return $ Concat 0 [arr1,arr2]
-}


-- | The dimension we concat on may slide over due to insertion of new dims.
adjustDim :: Int -> R.SliceIndex ix slice coSlice sliceDim -> Int
adjustDim _ R.SliceNil   = error "adjustDim: overran the dimensions."
adjustDim d (R.SliceFixed r) = adjustDim d r
adjustDim 0 (R.SliceAll _)   = 0
adjustDim d (R.SliceAll r)   = 1 + adjustDim (d-1) r

----------------------------------------

-- The problem with this is that it leads to inaccessible code errors:
_caseSlice :: forall ix b . A.Slice ix =>
             A.Exp ix
          -> (forall ix0 . (Slice ix0, ix ~ (ix0 A.:. Int)) => () -> b)
          -> (forall ix0 . (Slice ix0, ix ~ (ix0 A.:. All)) => () -> b)
          -> ((ix ~ A.Z)   => () -> b)
          -> ((ix ~ A.All) => () -> b)
          -> b
_caseSlice = error "caseSlice - Trevor can probably figure out how to implement this"

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

----------------------------------------

-- | Check if a dimension of interest is extruded.
_isExtruded :: Slice ix => Int -> R.SliceIndex ix slice coSlice sliceDim -> Bool
_isExtruded origd origsl = go origd origsl
  where
   go :: Int -> R.SliceIndex ix slice coSlice sliceDim -> Bool
   go _ R.SliceNil = error "isExtruded: dimension index exceeds slice dimensions: "
   go 0 (R.SliceAll _)      = False
   go 0 (R.SliceFixed _)    = True
   go d (R.SliceAll rest)   = go (d-1) rest
   go d (R.SliceFixed rest) = go (d-1) rest

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

--------------------------------------------------------------------------------

generate0 :: (Elt a) => A.Exp Z -> (A.Exp Z -> A.Exp a) -> Acc (Array Z a)
-- Cannot meaningfully split zero dim:
generate0 e f = MkWrap $ \_numSplits -> return $
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
generate1 sh f = MkWrap $ \numSplits ->
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

--------------------------------------------------------------------------------

foldn :: (Slice ix, Shape ix, Elt a) =>
         (A.Exp a -> A.Exp a -> A.Exp a)
      -> A.Exp a
      -> Acc (Array (ix :. Int) a)
      -> Int
      -> (Acc (Array ix a))
foldn f i (MkWrap fn) dims = mkacc $ \numSplits ->
  do inputs <- fn numSplits
     case inputs of
       (Concat _ [])     -> error "Nothing to do"
       (Concat d [arr]) ->
         do dim     <- askTuner [0..dims-1]
            (a1,a2) <- dosplit dim arr
            let m1 = A.fold f i a1
                m2 = A.fold f i a2
                m3 = A.zipWith f m1 m2
            return $ Concat d [m3]

       (Concat d [m1,m2]) ->
         let m1' = A.fold f i m1
             m2' = A.fold f i m2
             m3  = A.zipWith f m1' m2'
         in return $ Concat d [m3]

       -- FIXME: no reason to throw away task parallelism here.
       -- Just mimick the 2-way case with an N-way one.
       (Concat d ms) ->
         do let arr = foldr1 (A.++) ms
            dim     <- askTuner [0..dims-1]
            (a1,a2) <- dosplit dim arr
            let m1 = A.fold f i a1
                m2 = A.fold f i a2
                m3 = A.zipWith f m1 m2
            return $ Concat d [m3]

{-

fold1n :: (Slice ix, Shape ix, Elt a) =>
          (A.Exp a -> A.Exp a -> A.Exp a) ->
          Acc (Array (ix :. Int) a) -> Int ->
          TuneM (Acc (Array ix a))
fold1n f (MkAcc (Concat _ [])) _n = error "Nothing to do"
fold1n f (MkAcc (Concat d [arr])) n =
    do dim     <- askTuner [0..n-1]
       (a1,a2) <- split dim arr
       let m1 = A.fold1 f a1
           m2 = A.fold1 f a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]
fold1n f (MkAcc (Concat d [m1,m2])) _n =
    let m1' = A.fold1 f m1
        m2' = A.fold1 f m2
        m3  = A.zipWith f m1' m2'
    in return $ MkAcc $ Concat d [m3]
fold1n f (MkAcc (Concat d ms)) n =
    do let arr = foldr1 (A.++) ms
       dim     <- askTuner [0..n-1]
       (a1,a2) <- split dim arr
       let m1 = A.fold1 f a1
           m2 = A.fold1 f a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]

fold1 :: forall ix a. (Slice ix, Shape ix, Elt a) =>
         (A.Exp a -> A.Exp a -> A.Exp a) ->
         Acc (Array (ix :. Int) a) ->
         TuneM (Acc (Array ix a))
fold1 f arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = fold1n f arr 1
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = fold1n f arr 2
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = fold1n f arr 3
    | otherwise = fold1n f arr 0

-}

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

--------------------------------------------------------------------------------

-- FIXME: Transpose is currently not fissioned:
transpose :: Elt e => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
transpose (MkWrap fn) = mkacc $ \_numSplits ->
  -- Here we tell the upstream to give us one chunk:
  do inp <- fn 1
     case inp of
       (Concat _ arrs)  -> let [arr] = arrs in
                           return $ Concat 0 [A.transpose arr]
       -- Policy choice.  We could throw away the split, or maintain it:
       (Split 0 arr)    -> return $ Split 1  (A.transpose arr)
       (Split 1 arr)    -> return $ Split 0  (A.transpose arr)

--------------------------------------------------------------------------------

zipWith  :: (Slice sh, Shape sh, Elt a, Elt b, Elt c) =>
          (A.Exp a -> A.Exp b -> A.Exp c)
          -> Acc (Array (sh :. Int) a)
          -> Acc (Array (sh :. Int) b)
          -> Acc (Array (sh :. Int) c)
zipWith f (MkWrap f1) (MkWrap f2) = mkacc $ \numSplits -> do
  Concat d1 x <- f1 numSplits
  Concat _d2 y <- f2 numSplits
  case (x,y) of
    ([], _) -> error "Nothing to do"
    (_,[])  -> error "Nothing to do"
    ([m1], [m2]) | numSplits > 1 ->
      do dim <- askTuner [0..10]
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
      do dim <- askTuner [0]
         (m11,m12) <- dosplit dim m1
         let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    ([m11,m12], [m21,m22]) ->
      do let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    _ -> error "Not implemented"



combine :: (Slice sh, Shape sh, Elt e) =>
           Acc (Array (sh :. Int) e)
        -> TuneM (A.Acc (Array (sh :. Int) e))
combine (MkWrap fn) =
  trace ("FYI: CALLING Fission.combine") $
  do res <- fn 2
     lift $ do hPutStrLn stderr (L.replicate 80 '=')
               hPutStrLn stderr ("combine: shallow-language term:\n" ++ show res)
               hPutStrLn stderr (L.replicate 80 '=')
     return $ case res of
               (Concat 0 [a]) -> a
               (Concat 0 as)  -> foldr1 (A.++) $ P.map A.compute as
               (Concat _ _) -> error "combine: unfinished cases"

-- combine0 (MkAcc (Concat 0 [a])) = a
{-
combine'
  :: (Slice sh, Shape sh, Elt e) =>
     Acc (Array (sh :. Int) e) -> A.Acc (Array (sh :. Int) e)
combine' (MkAcc (Concat 0 [a])) = a
combine' (MkAcc (Concat 0 as)) = foldr1 (A.++) as

sfoldl :: forall sh a b. (Shape sh, Slice sh, Elt a, Elt b)
       => (A.Exp a -> A.Exp b -> A.Exp a)
       -> A.Exp a
       -> A.Exp sh
       -> Acc (Array (sh :. Int) b)
       -> A.Exp a
sfoldl = undefined
-}

--------------------------------------------------------------------------------
-- TESTS
--------------------------------------------------------------------------------
--
-- TODO: Move me somewhere appropriate
--

arr0 :: Acc (A.Vector Double)
arr0 = liftAcc $ A.use (A.fromList (A.Z :. 10) [0..])

-- a1 = Fission1.map (+ 1) arr
-- a2 = do { a1' <- a1; Fission1.map (* 2) a1'}
-- a3 = do { a2' <- a2; Fission1.fold1 (+) a2' }
-- a4 = do { a1' <- a1; a2' <- a2; Fission1.zipWith (+) a1' a2' }

ac1 :: IO (A.Acc (Array (DIM0 :. Int) Double))
ac1 = runTune2 $ combine $ Data.Array.Accelerate.Fission.map (+ 1) arr0

ac2 :: IO (A.Acc (Array (DIM0 :. Int) Double))
ac2 = runTune2 $ let a = Data.Array.Accelerate.Fission.map (+ 1) arr0
                 in combine $ zipWith (+) a a

ac3 :: IO (A.Acc (Array DIM2 Double))
ac3 = runTune2 $ combine $
      replicate (A.constant (Z :. (3::Int) :. All)) arr0


-- a2' = do { a1' <- a1; a2' <- Fission1.map (* 2) a1'; return $ combine a2' }
-- a3' = do { a2' <- a2; a3' <- Fission1.fold1 (+) a2'; return $ combine0 a3' }
-- a4' = do { a1' <- a1; a2' <- a2; a4' <- Fission1.zipWith (+) a1' a2'; return $ combine a4' }
