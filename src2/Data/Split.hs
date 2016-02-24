{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Split
  where

import Data.Array.Accelerate                            as A hiding ( Split )

import Control.Monad
import Prelude                                          as P

data Split a = Split
  { split       :: Double -> a -> (a,a)
  , combine     :: a -> a -> a
  , splitDim    :: Int
  }


class Splittable a where
  splittable :: MonadPlus t => t (Split a)


instance Splittable [a] where
  splittable
    = return
    $ Split { split    = \p l -> let n = P.round (p * P.fromIntegral (P.length l)) in splitAt n l
            , combine  = (P.++)
            , splitDim = 0
            }


instance Elt e => Splittable (Acc (Scalar e)) where
  splittable = mzero

instance Elt e => Splittable (Acc (Vector e)) where
  splittable
    = return
    $ Split { split     = \p a -> let n = A.round (constant p * A.fromIntegral (size a)) in (A.take n a, A.drop n a)
            , combine   = (A.++)
            , splitDim  = 0
            }

instance Elt e => Splittable (Acc (Array DIM2 e)) where
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

