module Fission1 where
import           Control.Monad
import           Data.Array.Accelerate (Array, Elt, Shape)
import qualified Data.Array.Accelerate as A
import           Prelude               as P hiding (concat)
type TuneM a = IO a

newtype Acc a = MkAcc (Rep a)
data Rep a = One          (A.Acc a)
           | Concat DimId [A.Acc a]

type DimId = Int

map :: (Shape ix, Elt a, Elt b)
    => (A.Exp a -> A.Exp b) -> Acc (Array ix a) -> TuneM (Acc (Array ix b))
map f (MkAcc (One arr)) =
  do dim     <- askTuner [0..10]
     (a1,a2) <- split dim arr
     let m1 = A.map f a1
         m2 = A.map f a2
     return $ MkAcc $ Concat dim [m1,m2]
map f (MkAcc (Concat d as)) =
  let as' = P.map (\a -> A.map f a) as
  in return $ MkAcc (Concat d as')

-- does split need to change the shape type?
split :: DimId -> A.Acc a -> TuneM (A.Acc a, A.Acc a)
split = undefined

concat :: DimId -> Rep a -> Rep a -> TuneM (Rep a)
concat _ (Concat _ _) (Concat _ _) = undefined
concat _ (One _)      (Concat _ _) = undefined
concat _ (Concat _ _) (One _)      = undefined
concat _ (One _)      (One _)      = undefined

askTuner :: [Int] -> TuneM Int
askTuner = undefined
