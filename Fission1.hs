{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Fission1 where
import           Control.Monad
import           Data.Array.Accelerate (Array, Elt, Shape, (:.)(..))
import qualified Data.Array.Accelerate as A
import           Prelude               as P hiding (concat)
type TuneM a = IO a

newtype Acc a = MkAcc (Rep a)
  deriving Show

data Rep a = Concat DimId [A.Acc a]

instance A.Arrays a => Show (Rep a) where
  show (Concat d ls) =
   "Concat along dim "++ show d++" of "++ show (length ls)++" chunks:\n" ++
   unlines [ show x | x <- ls ]

type DimId = Int

arr :: A.Acc (A.Vector Double)
arr = A.use (A.fromList (A.Z :. 10) [0..])

map :: (Shape ix, Elt a, Elt b)
    => (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> TuneM (Acc (Array ix b))
map f (MkAcc (Concat _ [arr])) =
  do dim     <- askTuner [0..10]
     (a1,a2) <- undefined -- split dim arr
     let m1 = A.map f a1
         m2 = A.map f a2
     return $ MkAcc $ Concat dim [m1,m2]
map f (MkAcc (Concat d as)) =
  let as' = P.map (\a -> A.map f a) as
  in return $ MkAcc (Concat d as')

-- does split need to change the shape type?

-- split :: DimId -> A.Acc a -> TuneM (A.Acc a, A.Acc a)
--split = undefined

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
                  f x               = A.lift $ (A.indexTail x) A.:. start
              in A.backpermute bsh f arr


concat :: DimId -> Rep a -> Rep a -> TuneM (Rep a)
concat d3 (Concat d1 ls1) (Concat d2 ls2)
 | d1 == d2 && d1 == d3 = return $ Concat d3 (ls1 ++ ls2)
 | otherwise = error "Brain explodes for now..." 

askTuner :: [Int] -> TuneM Int
askTuner ls = return $ head ls
