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

data Rep a = Concat DimId [A.Acc a] -- could add new variant field Zipwith for fissioning fold

instance A.Arrays a => Show (Rep a) where
  show (Concat d ls) =
   "Concat along dim "++ show d++" of "++ show (length ls)++" chunks:\n" ++
   unlines [ show x | x <- ls ]

type DimId = Int

mkacc a = MkAcc $ Concat 0 [a]

arr :: Acc (A.Vector Double)
arr = mkacc $ A.use (A.fromList (A.Z :. 10) [0..])

a1 = Fission1.map (+ 1) arr

run1 :: (Slice ix, Shape ix, Elt a) =>
        Acc (Array (ix :. Int) a) -> Array (ix :. Int) a
run1 (MkAcc (Concat _ [])) = error "No arrays to concat"
run1 (MkAcc (Concat 0 as)) = I.run $ foldl1 (A.++) as

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

fold1 :: (Slice ix, Shape ix, Elt a) =>
         (A.Exp a -> A.Exp a -> A.Exp a) ->
         Acc (Array (ix :. Int) a) -> TuneM (Acc (Array ix a))
fold1 f (MkAcc (Concat _ [])) = error "Nothing to do"
fold1 f (MkAcc (Concat d [arr])) =
    do dim     <- askTuner [0..10]
       (a1,a2) <- split dim arr
       let m1 = A.fold1 f a1
           m2 = A.fold1 f a2
           -- This is what we did before
           m3 = A.zipWith f m1 m2
       return $ MkAcc $ Concat d [m3]

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
