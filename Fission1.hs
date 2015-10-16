{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Fission1 where
import           Control.Monad
import           Data.Array.Accelerate                ((:.) (..), Array, Elt,
                                                       Shape)
import qualified Data.Array.Accelerate                as A
import           Data.Array.Accelerate.Analysis.Match
import           Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate.Interpreter    as I
import           Data.Typeable
import           Prelude                              as P hiding (concat)
type TuneM a = IO a

newtype Acc a = MkAcc (Rep a)
  deriving Show

data Rep a = Concat DimId [A.Acc a]
---        | Zipwith (A.Exp b -> A.Exp b -> A.Exp b) (A.Acc a, A.Acc a) ???

instance A.Arrays a => Show (Rep a) where
  show (Concat d ls) =
      "Concat along dim "++ show d++" of "++ show (length ls)++" chunks:\n" ++
                         unlines [ show x | x <- ls ]

type DimId = Int


mkacc a = MkAcc $ Concat 0 [a]

arr :: Acc (A.Vector Double)
arr = mkacc $ A.use (A.fromList (A.Z :. 10) [0..])

a1 = Fission1.map (+ 1) arr
a2 = do { a1' <- a1; Fission1.map (* 2) a1'}
a3 = do { a2' <- a2; Fission1.fold1 (+) a2' }
a4 = do { a1' <- a1; a2' <- a2; Fission1.zipWith (+) a1' a2' }

run1 :: (Slice ix, Shape ix, Elt a) =>
        Acc (Array (ix :. Int) a) -> Array (ix :. Int) a
run1 (MkAcc (Concat _ [])) = error "No arrays to concat"
run1 (MkAcc (Concat 0 as)) = I.run $ foldr1 (A.++) as

run0 :: (Shape ix, Elt a) =>
        Acc (Array ix a) -> Array ix a
run0 (MkAcc (Concat _ [])) = error "Nothing to do"
run0 (MkAcc (Concat _ [a])) = I.run a
run0 (MkAcc (Concat _ _as)) = error "Not implemented"

run :: forall ix a. (Slice ix, Shape ix, Elt a) =>
       Acc (Array ix a) -> Array ix a
run (MkAcc (Concat _ [])) = error "No arrays to concat"
run (MkAcc (Concat 0 as))
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = run0 (MkAcc (Concat 0 as))
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = run1 (MkAcc (Concat 0 as))
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = run1 (MkAcc (Concat 0 as))
    | otherwise = run0 (MkAcc (Concat 0 as))

map1 :: (Slice ix, Shape ix, Elt a, Elt b) =>
        (A.Exp a -> A.Exp b) -> Acc (Array (ix :. Int) a) -> TuneM (Acc (Array (ix :. Int) b))
map1 _ (MkAcc (Concat _ [])) = error "Nothing to do."
map1 f (MkAcc (Concat _ [arr])) =
    do dim     <- askTuner [0..10]
       (a1,a2) <- split dim arr
       let m1 = A.map f a1
           m2 = A.map f a2
       return $ MkAcc $ Concat dim [m1,m2]
map1 f (MkAcc (Concat d as)) =
    let as' = P.map (\a -> A.map f a) as
    in return $ MkAcc (Concat d as')

map0 :: (Shape ix, Elt a, Elt b) =>
        (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> TuneM (Acc (Array ix b))
map0 f (MkAcc (Concat d as)) =
    let as' = P.map (\a -> A.map f a) as
    in return $ MkAcc (Concat d as')

map
  :: forall ix a b. (Shape ix, Elt a, Elt b) =>
     (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> TuneM (Acc (Array ix b))
map f arr
    | Just REFL <- matchShape (undefined :: A.Z)    (undefined :: ix) = map0 f arr
    | Just REFL <- matchShape (undefined :: A.DIM1) (undefined :: ix) = map1 f arr
    | Just REFL <- matchShape (undefined :: A.DIM2) (undefined :: ix) = map1 f arr
    | otherwise = map0 f arr


fold :: (Slice ix, Shape ix, Elt a) =>
        (A.Exp a -> A.Exp a -> A.Exp a) ->
        A.Exp a -> Acc (Array (ix :. Int) a) -> TuneM (Acc (Array ix a))
fold f i (MkAcc (Concat _ [])) = error "Nothing to do"
fold f i (MkAcc (Concat d [arr])) =
    do dim     <- askTuner [0..10]
       (a1,a2) <- split dim arr
       let m1 = A.fold f i a1
           m2 = A.fold f i a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]
fold f i (MkAcc (Concat d [m1,m2])) =
    let m1' = A.fold f i m1
        m2' = A.fold f i m2
        m3  = A.zipWith f m1' m2'
    in return $ MkAcc $ Concat d [m3]
fold f i (MkAcc (Concat d ms)) =
    do let arr = foldr1 (A.++) ms
       dim     <- askTuner [0..10]
       (a1,a2) <- split dim arr
       let m1 = A.fold f i a1
           m2 = A.fold f i a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]

fold1 :: (Slice ix, Shape ix, Elt a) =>
         (A.Exp a -> A.Exp a -> A.Exp a) ->
         Acc (Array (ix :. Int) a) -> TuneM (Acc (Array ix a))
fold1 f (MkAcc (Concat _ [])) = error "Nothing to do"
fold1 f (MkAcc (Concat d [arr])) =
    do dim     <- askTuner [0..10]
       (a1,a2) <- split dim arr
       let m1 = A.fold1 f a1
           m2 = A.fold1 f a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]
fold1 f (MkAcc (Concat d [m1,m2])) =
    let m1' = A.fold1 f m1
        m2' = A.fold1 f m2
        m3  = A.zipWith f m1' m2'
    in return $ MkAcc $ Concat d [m3]
fold1 f (MkAcc (Concat d ms)) =
    do let arr = foldr1 (A.++) ms
       dim     <- askTuner [0..10]
       (a1,a2) <- split dim arr
       let m1 = A.fold1 f a1
           m2 = A.fold1 f a2
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]

zipWith f (MkAcc (Concat _ [])) _ = error "Nothing to do"
zipWith f _ (MkAcc (Concat _ [])) = error "Nothing to do"
zipWith f (MkAcc (Concat d1 [m1])) (MkAcc (Concat d2 [m2])) =
    do dim <- askTuner [0..10]
       (m11,m12) <- split dim m1
       (m21,m22) <- split dim m2
       let m1' = A.zipWith f m11 m21
           m2' = A.zipWith f m21 m22
       return $ MkAcc $ Concat d1 [m1',m2']
zipWith f (MkAcc (Concat d1 [m11,m12])) (MkAcc (Concat d2 [m21,m22])) =
    do let m1' = A.zipWith f m11 m21
           m2' = A.zipWith f m21 m22
       return $ MkAcc $ Concat d1 [m1',m2']
zipWith _ _ _ = error "Not implemented"

split :: (A.Slice sh,Shape sh,Elt a)
      => DimId
      -> A.Acc (Array (sh A.:. Int) a)
      -> TuneM ( A.Acc (Array (sh A.:. Int) a),
                 A.Acc (Array (sh A.:. Int) a))
split _dimid arr =
      return (splitArray (A.constant 0), splitArray (A.constant 1))
    where splitArray i =
              let shead             = A.indexHead $ A.shape arr
                  (chunk, leftover) = shead `quotRem` 2
                  start             = (i A.<* leftover) A.?
                                      (i * (chunk + 1),
                                       i * chunk + leftover)
                  end               = ((i+1) A.<* leftover) A.?
                                      (start + chunk,
                                       (i+1) * chunk + leftover)
                  bsh               = A.lift $ (A.indexTail $ A.shape arr) A.:. (end - start)
                  f x               = A.lift $ (A.indexTail x) A.:. ((A.indexHead x) + start)
              in A.backpermute bsh f arr


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
