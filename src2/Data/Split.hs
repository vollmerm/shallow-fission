{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Split
  where

import Data.Array.Accelerate                            as A hiding ( Split )

import Control.Monad
import Prelude                                          as P


data Split a = Split
  { split       :: Double -> Acc a -> (Acc a, Acc a)
  , combine     :: Acc a -> Acc a -> Acc a
  , splitDim    :: Int
  }

-- data Split c where
--     Split ::
--       { split    :: Elt e => Double -> Acc (c e) -> (Acc (c e), Acc (c e))
--       , combine  :: Elt e => Acc (c e) -> Acc (c e) -> Acc (c e)
--       , splitDim :: Int
--       }
--       -> Split c

class Splittable c where
  splittable :: MonadPlus t => t (Split c)

-- instance Splittable [a] where
--   splittable
--     = return
--     $ Split { split    = \p l -> let n = P.round (p * P.fromIntegral (P.length l)) in splitAt n l
--             , combine  = (P.++)
--             , splitDim = 0
--             }


instance Splittable (Scalar e) where
  splittable = mzero

instance Elt e => Splittable (Vector e) where
  splittable
    = return
    $ Split { split     = \p a -> let n = A.round (constant p * A.fromIntegral (size a)) in (A.take n a, A.drop n a)
            , combine   = (A.++)
            , splitDim  = 0
            }

instance Elt e => Splittable (Array DIM2 e) where
  splittable = return splitH `mplus` return splitV
    where
      splitH = Split { split = \p a ->
                        let Z :. _ :. m = unlift (shape a) :: Z :. Exp Int :. Exp Int
                            m'          = A.round (constant p * A.fromIntegral m)
                        in
                        (A.take m' a, A.drop m' a)
                     , combine  = (A.++)
                     , splitDim = 0
                     }
      splitV = Split { split = \p a ->
                        let Z :. n :. m = unlift (shape a) :: Z :. Exp Int :. Exp Int
                            n'          = the (unit (A.round (constant p * A.fromIntegral n)))
                        in
                        ( backpermute (lift (Z :. n'     :. m)) id a
                        , backpermute (lift (Z :. n - n' :. m)) (\ix -> let Z :. j :. i = unlift ix :: Z :. Exp Int :. Exp Int
                                                                        in lift (Z :. j + n' :. i)) a
                        )
                     , combine = \x y ->
                        let Z :. xj :. xi = unlift (shape x)  :: Z :. Exp Int :. Exp Int
                            Z :. yj :. yi = unlift (shape y)  :: Z :. Exp Int :. Exp Int
                        in
                        generate (index2 (xj + yj) (min xi yi))
                                 (\ix -> let Z :. j :. i = unlift ix :: Z :. Exp Int :. Exp Int
                                         in  j A.<* xj ? (x ! ix, y ! index2 (j-xj) i))
                     , splitDim = 1
                     }

