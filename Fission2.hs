{-# LANGUAGE TypeOperators #-}
module Fission2 where

import           Control.Monad
import           Data.Array.Accelerate (Array, Elt, Shape)
import qualified Data.Array.Accelerate as A
import           Prelude               as P hiding (concat)


type TuneM a = IO a

map :: (Shape sh,Elt a,Elt b) => (A.Exp a -> A.Exp b) -> A.Acc (Array (sh A.:. Int) a) -> TuneM (A.Acc (Array (sh A.:. Int) b))
map f arr = do
  (a1, a2) <- split arr
  let m1 = A.map f a1
      m2 = A.map f a2
  concat m1 m2

split :: (Shape sh,Elt a) => A.Acc (Array (sh A.:. Int) a) -> TuneM (A.Acc (Array (sh A.:. Int) a), A.Acc (Array (sh A.:. Int) a))
split = undefined -- backpermute

concat :: (Shape sh,Elt a) => A.Acc (Array (sh A.:. Int) a) -> A.Acc (Array (sh A.:. Int) a) -> TuneM (A.Acc (Array (sh A.:. Int) a))
concat = undefined -- generate
