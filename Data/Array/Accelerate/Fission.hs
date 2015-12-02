{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Array.Accelerate.Fission where

import Control.Monad
import Control.Monad
import Control.Monad.Reader
import Data.Typeable
import Unsafe.Coerce
import Prelude                                          as P hiding ( concat )

import Data.Array.Accelerate                            ( DIM0, DIM1, DIM2, (:.)(..) )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate                  as A

import qualified Data.Array.Accelerate.Interpreter      as I -- For testing.


type TuneM a = ReaderT [(String,Int)] IO a

runTune2 f = runReaderT f [("split",2)]

type NumSplits = Int

-- TODO:
newtype Acc a = MkAcc (NumSplits -> TuneM (Rep a))
--  deriving Show

-- | The language of multi-device computations.
data Rep a = Concat DimId [A.Acc a]
           -- Split?
           -- device selection?  what else

instance A.Arrays a => Show (Rep a) where
  show (Concat d ls) =
      "Concat along dim "++ show d++" of "++ show (length ls)++" chunks:\n" ++
                         unlines [ show x | x <- ls ]

type DimId = Int


--------------------------------------------------------------------------------
-- Utilities

split :: (A.Slice sh,Shape sh,Elt a)
      => DimId
      -> A.Acc (Array (sh A.:. Int) a)
      -> TuneM ( A.Acc (Array (sh A.:. Int) a),
                 A.Acc (Array (sh A.:. Int) a))
split _dimid arr = return (arr1, arr2)
    where arrTl = A.indexTail $ A.shape arr
          arrHd = A.indexHead $ A.shape arr
          (chunk, leftover) = arrHd `quotRem` 2
          arr1Sh = arrTl :. chunk
          arr2Sh = arrTl :. (chunk + leftover)
          adjust i = let t = A.indexTail i
                         h = A.indexHead i
                     in A.lift $ t :. (h + chunk)
          arr1 = A.generate (A.lift arr1Sh) (\sh -> arr A.! sh)
          arr2 = A.generate (A.lift arr2Sh) (\sh -> arr A.! (adjust sh))

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

concat :: DimId -> Rep a -> Rep a -> TuneM (Rep a)
concat d3 (Concat d1 ls1) (Concat d2 ls2)
 | d1 == d2 && d1 == d3 = return $ Concat d3 (ls1 ++ ls2)
 | otherwise = error "Brain explodes for now..."

askTuner :: [Int] -> TuneM Int
askTuner ls = return $ head ls

matchArrayShape
    :: forall acc aenv sh sh' e. (Shape sh, Shape sh')
    => {- dummy -} acc aenv (Array sh e)
    -> {- dummy -} sh'
    -> Maybe (sh :=: sh')
matchArrayShape _ _
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

matchSlice --- ?????
  :: forall sh sh'. (Slice sh, Slice sh')
  => sh
  -> sh'
  -> Maybe (sh :=: sh')
matchSlice _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh))
                                (eltType (undefined::sh'))
  = gcast REFL

  | otherwise
  = Nothing

-- FIXME: This should probably introduce a split node.
mkacc a = MkAcc $ \_ -> return $ Concat 0 [a]

--------------------------------------------------------------------------------
-- RUNNING
--------------------------------------------------------------------------------

{-
go1 :: (Slice sh, Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array (sh :. Int) a)
    -> Array (sh :. Int) a
go1 exec (MkAcc (Concat dim arrs))
  | null arrs   = error "Data.Array.Accelerate.Fusion.go1: nothing to do"
  | dim /= 0    = error "Data.Array.Accelerate.Fusion.go1: I only know about dimension 0"
  | otherwise   = exec $ foldr1 (A.++) arrs


go0 :: (Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array sh a)
    -> Array sh a
go0 exec (MkAcc (Concat _ arrs))
  | null arrs   = error "Data.Array.Accelerate.Fusion.go0: nothing to do"
  | [a] <- arrs = exec a
  | otherwise   = error "Data.Array.Accelerate.Fusion.go0: not implemented yet"


run :: forall sh a. (Slice sh, Shape sh, Elt a)
    => (forall arrs. Arrays arrs => A.Acc arrs -> arrs)
    -> Acc (Array sh a)
    -> Array sh a
run exec arrs
  -- | Just REFL <- matchShape (undefined :: DIM0) (undefined :: sh) = go0 exec arrs
  | Just REFL <- matchShape (undefined :: DIM1) (undefined :: sh) = go1 exec arrs
  | Just REFL <- matchShape (undefined :: DIM2) (undefined :: sh) = go1 exec arrs
  | otherwise                                                     = go0 exec arrs
-}

--------------------------------------------------------------------------------
-- Wrappers for Core Accelerate operations
--------------------------------------------------------------------------------

map1n :: (Slice ix, Shape ix, Elt a, Elt b) =>
         (A.Exp a -> A.Exp b) -> Acc (Array (ix :. Int) a) -> Int ->
         (Acc (Array (ix :. Int) b))
map1n f (MkAcc m) n = MkAcc $ \numSplits ->
    do (Concat d ls) <- m numSplits
       case ls of
         [] -> error "Nothing to do."
         -- FIXME: Here we should probably introduce a split of the specific arity,
         -- but we can't do that yet so we only do two-way:
         [arr] | numSplits > 1 -> do
           dim     <- askTuner [0..n-1]
           (a1,a2) <- split dim arr -- TODO: multi-way split.
           let m1 = A.map f a1
               m2 = A.map f a2
           return $ Concat dim [m1,m2]
         as -> let as' = P.map (\a -> A.map f a) as
               in return $ Concat d as'

map0 :: (Shape ix, Elt a, Elt b) =>
        (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> (Acc (Array ix b))
map0 f (MkAcc m) = MkAcc $ \numSplits ->
  do Concat d as <- m numSplits
     -- Here we don't change the chunking of what comes back:
     let as' = P.map (\a -> A.map f a) as
     return $ Concat d as'

map
  :: forall ix a b. (Shape ix, Elt a, Elt b) =>
     (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> (Acc (Array ix b))
map f arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = map0 f arr
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = map1n f arr 1
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = map1n f arr 2
    | otherwise = map0 f arr

{-

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

-- replicate :: (Slice ix, Elt e) => A.Exp ix -> Acc (Array (A.SliceShape ix) e)
--           -> TuneM (Acc (Array (A.FullShape ix) e))
replicate e (MkAcc (Concat _ [])) = error "Nothing to do"
replicate e (MkAcc (Concat d [arr])) =
    -- generate (A.shape arr') (\s -> arr' A.! s)
    -- where arr' = A.replicate e arr
    return $ MkAcc $ Concat 0 [arr1,arr2]
        where arr'   = A.replicate e arr
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
replicate _ _ = error "don't care yet"

transpose (MkAcc (Concat _ [arr])) = return $ MkAcc $ Concat 0 [A.transpose arr]


generate :: (Shape ix, Elt a, Slice ix) => A.Exp ix -> (A.Exp ix -> A.Exp a)
         -> TuneM (Acc (Array ix a))
generate e f = do
  arr     <- return $ A.generate e f
  return $ MkAcc $ Concat 0 [arr]

generateSplit :: forall ix a. (Shape ix, Elt a, Slice ix) => A.Exp ix -> (A.Exp ix -> A.Exp a)
              -> TuneM (Acc (Array ix a))
generateSplit sh f
    | Just REFL <- matchShape (undefined :: Z)      (undefined :: ix) = generate sh f
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = generate1 sh f
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = generate1 sh f
    | otherwise = generate sh f

generate1 :: (Shape ix, Elt a, Slice ix) => A.Exp (ix A.:. Int) -> (A.Exp (ix A.:. Int) -> A.Exp a)
          -> TuneM (Acc (Array (ix A.:. Int) a))
generate1 sh f = do
    let arrHd = A.indexHead sh
        arrTl = A.indexTail sh
    (ln1, ln2) <- askTunerSplit arrHd
    let   arr1Sh = arrTl :. ln1
          arr2Sh = arrTl :. ln2
          adjust i = let t = A.indexTail i
                         h = A.indexHead i
                     in A.lift $ t :. (h + ln1)
          arr1 = A.generate (A.lift arr1Sh) f
          arr2 = A.generate (A.lift arr2Sh) $ f . adjust
    return $ MkAcc $ Concat 0 [arr1,arr2]

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


foldn :: (Slice ix, Shape ix, Elt a) =>
         (A.Exp a -> A.Exp a -> A.Exp a) ->
         A.Exp a -> Acc (Array (ix :. Int) a) -> Int ->
         TuneM (Acc (Array ix a))
foldn f i (MkAcc (Concat _ [])) _n = error "Nothing to do"
foldn f i (MkAcc (Concat d [arr])) n =
    do dim     <- askTuner [0..n-1]
       (a1,a2) <- split dim arr
       let m1 = A.fold f i a1
           m2 = A.fold f i a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]
foldn f i (MkAcc (Concat d [m1,m2])) _n =
    let m1' = A.fold f i m1
        m2' = A.fold f i m2
        m3  = A.zipWith f m1' m2'
    in return $ MkAcc $ Concat d [m3]
foldn f i (MkAcc (Concat d ms)) n =
    do let arr = foldr1 (A.++) ms
       dim     <- askTuner [0..n-1]
       (a1,a2) <- split dim arr
       let m1 = A.fold f i a1
           m2 = A.fold f i a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]

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

fold :: forall ix a. (Slice ix, Shape ix, Elt a) =>
        (A.Exp a -> A.Exp a -> A.Exp a) ->
        A.Exp a -> Acc (Array (ix :. Int) a) ->
        TuneM (Acc (Array ix a))
fold f a arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = foldn f a arr 1
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = foldn f a arr 2
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = foldn f a arr 3
    | otherwise = foldn f a arr 0

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

zipWith  :: (Slice sh, Shape sh, Elt a, Elt b, Elt c) =>
          (A.Exp a -> A.Exp b -> A.Exp c)
          -> Acc (Array (sh :. Int) a)
          -> Acc (Array (sh :. Int) b)
          -> Acc (Array (sh :. Int) c)
zipWith f (MkAcc f1) (MkAcc f2) = MkAcc $ \numSplits -> do
  Concat d1 x <- f1 numSplits
  Concat d2 y <- f2 numSplits
  case (x,y) of
    ([], _) -> error "Nothing to do"
    (_,[])  -> error "Nothing to do"
    ([m1], [m2]) | numSplits > 1 ->
      do dim <- askTuner [0..10]
         (m11,m12) <- split dim m1
         (m21,m22) <- split dim m2
         let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    ([m1], [m2]) ->
      return $ Concat d1 [A.zipWith f m1 m2]

    ([m1], [m21,m22])  ->
      -- do let m2 = (A.compute m21) A.++ (A.compute m22)
      --    return $ MkAcc $ Concat 0 [(A.zipWith f m1 m2)]
      do dim <- askTuner [0]
         (m11,m12) <- split dim m1
         let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    (([m11,m12]), ([m21,m22])) ->
      do let m1' = A.zipWith f m11 m21
             m2' = A.zipWith f m12 m22
         return $ Concat d1 [m1',m2']

    _ -> error "Not implemented"



combine
  :: (Slice sh, Shape sh, Elt e) =>
     Acc (Array (sh :. Int) e) -> TuneM (A.Acc (Array (sh :. Int) e))
combine (MkAcc m) =
  do res <- m 2
     return $ case res of
               (Concat 0 [a]) -> a
               (Concat 0 as)  -> foldr1 (A.++) $ P.map A.compute as

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

-- TESTS
-- -----
--
-- TODO: Move me somewhere appropriate
--

arr :: Acc (A.Vector Double)
arr = mkacc $ A.use (A.fromList (A.Z :. 10) [0..])

-- a1 = Fission1.map (+ 1) arr
-- a2 = do { a1' <- a1; Fission1.map (* 2) a1'}
-- a3 = do { a2' <- a2; Fission1.fold1 (+) a2' }
-- a4 = do { a1' <- a1; a2' <- a2; Fission1.zipWith (+) a1' a2' }

a1 = runTune2 $ combine $ Data.Array.Accelerate.Fission.map (+ 1) arr

a2 = runTune2 $ let a = Data.Array.Accelerate.Fission.map (+ 1) arr
                in combine $ Data.Array.Accelerate.Fission.zipWith (+) a a

-- a2' = do { a1' <- a1; a2' <- Fission1.map (* 2) a1'; return $ combine a2' }
-- a3' = do { a2' <- a2; a3' <- Fission1.fold1 (+) a2'; return $ combine0 a3' }
-- a4' = do { a1' <- a1; a2' <- a2; a4' <- Fission1.zipWith (+) a1' a2'; return $ combine a4' }
