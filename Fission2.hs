{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Fission2 where

import           Control.Monad
import           Data.Array.Accelerate (Array, Elt, Shape)
import qualified Data.Array.Accelerate as A
import           Prelude               as P hiding (concat)


type TuneM a = IO a

map :: (A.Slice sh,Shape sh,Elt a,Elt b)
    => (A.Exp a -> A.Exp b) -> A.Acc (Array (sh A.:. Int) a) -> TuneM (A.Acc (Array (sh A.:. Int) b))
map f arr = do
  (a1, a2) <- split arr
  let m1 = A.map f a1
      m2 = A.map f a2
  concat m1 m2

split :: (A.Slice sh,Shape sh,Elt a)
      => A.Acc (Array (sh A.:. Int) a) -> TuneM (A.Acc (Array (sh A.:. Int) a), A.Acc (Array (sh A.:. Int) a))
split arr = return (splitArray (A.constant 0), splitArray (A.constant 1))
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

concat :: (A.Slice sh,Shape sh,Elt a)
       => A.Acc (Array (sh A.:. Int) a) -> A.Acc (Array (sh A.:. Int) a) -> TuneM (A.Acc (Array (sh A.:. Int) a))
concat xs ys = return $ A.generate gsh f
    where (sh1 A.:. n) = A.unlift $ A.shape xs
          (sh2 A.:. m) = A.unlift $ A.shape ys
          gsh          = A.lift $ (A.intersect sh1 sh2) A.:. (n + m)
          f ix         = ((A.indexHead ix) A.<* n) A.?
                         (xs A.! ix,
                          ys A.! (A.lift ((A.indexTail ix) A.:. ((A.indexHead ix) - n))))
