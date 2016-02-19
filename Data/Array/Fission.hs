{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Array.Fission where

import Data.Array.Accelerate                                      as A hiding ( Acc, Split )
import qualified Data.Array.Accelerate                            as A
import Prelude                                                    as P hiding ( splitAt )

import System.Random.MWC
import Control.Monad.Reader


-- data Request a where
--     Request :: [Par a] -> Request [a]

-- data Par a where
--     Do   :: a -> Par a
--     Par  :: Par a -> Par b -> Par (a,b)
--     Join :: Par a -> Par b -> (a -> b -> Par c) -> Par c


-- type Acc a = [Par (A.Acc a)]


-- use :: Arrays arrs => arrs -> Acc arrs
-- use = return . Do . A.use

-- map :: (Shape sh, Elt a, Elt b)
--     => (Exp a -> Exp b)
--     -> Acc (Array sh a)
--     -> Acc (Array sh b)


map f xs = do
  xs' <- tune xs
  return (P.map (A.map f) xs')


type TuneM a = ReaderT GenIO IO a

tune :: (Splittable (A.Acc a), Arrays a) => A.Acc a -> TuneM [A.Acc a]
tune a = do
  action <- oneof [minBound .. maxBound]
  case action of
    NOP   -> return [a]
    Force -> return [ A.compute a ]
    Fizz  -> do
      s <- oneof splittable
      p <- splitpoint
      let (x,y) = splitAt s p a
      (P.++) <$> tune x <*> tune y

data Tune = NOP | Force | Fizz
  deriving (Bounded, Enum, Show)

splitpoint :: TuneM Double
splitpoint = do
  mwc   <- ask
  uniformR (0,1) mwc

oneof :: [a] -> TuneM a
oneof these = do
  mwc  <- ask
  this <- uniformR (0, P.length these - 1) mwc
  return (these P.!! this)



-- type Tune = IO

-- eval :: Par a -> Tune [a]
-- eval (Do x)    = return [x]
-- eval (Par x y) =

data Split a = Split
  { splitAt  :: Double -> a -> (a,a)
  , combine  :: a -> a -> a
  , splitDim :: Int
  }

class Splittable a where
  splittable :: [Split a]

  -- split :: Double           -- percentage split amount [0,1]
  --       -> a
  --       -> [(a,a)]


instance Elt e => Splittable (A.Acc (A.Scalar e)) where
  splittable = []

instance Elt e => Splittable (A.Acc (A.Vector e)) where
  splittable =
    [ Split { splitAt = \p a -> let n = A.round (A.constant p * A.fromIntegral (A.size a)) in (A.take n a, A.drop n a)
            , combine = (A.++)
            }
    ]

{--
  split at a =
    let n = A.round (A.constant at * A.fromIntegral (A.size a))
    in  [(A.take n a, A.drop n a)]
--}

instance Elt e => Splittable (A.Acc (A.Array A.DIM2 e)) where
  splittable = [ splitH, splitV ]
    where
      splitH = Split { splitAt = \p a ->
                        let Z :. _ :. m = A.unlift (shape a) :: Z :. Exp Int :. Exp Int
                            m'          = A.round (A.constant p * A.fromIntegral m)
                        in
                        (A.take m' a, A.drop m' a)
                     , combine = (A.++)
                     }
      splitV = Split { splitAt = \p a ->
                        let Z :. n :. m = A.unlift (shape a) :: Z :. Exp Int :. Exp Int
                            n'          = the (unit (A.round (A.constant p * A.fromIntegral n)))
                        in
                        ( A.backpermute (A.lift (Z :. n'     :. m)) id a
                        , A.backpermute (A.lift (Z :. n - n' :. m)) (\ix -> let Z :. j :. i = A.unlift ix :: Z :. Exp Int :. Exp Int
                                                                            in A.lift (Z :. j + n' :. i)) a
                        )
                     , combine = \x y ->
                        let Z :. xj :. xi = A.unlift (shape x)  :: Z :. Exp Int :. Exp Int
                            Z :. yj :. yi = A.unlift (shape y)  :: Z :. Exp Int :. Exp Int
                        in
                        A.generate (index2 (xj + yj) (min xi yi))
                                   (\ix -> let Z :. j :. i = A.unlift ix :: Z :. Exp Int :. Exp Int
                                           in  j A.<* xj ? (x ! ix, y ! index2 (j-xj) i))
                     }

{--
  split at arr =
    let Z :. n :. m = unlift (shape arr)    :: Z :. Exp Int :. Exp Int
        m'          = A.round (A.constant at * A.fromIntegral m)
        n'          = the (unit (A.round (A.constant at * A.fromIntegral n)))
    in
    [ (A.take m' arr, A.drop m' arr)
    , ( A.backpermute (lift (Z :. n'     :. m)) id arr
      , A.backpermute (lift (Z :. n - n' :. m)) (\ix -> let Z :. j :. i = unlift ix       :: Z :. Exp Int :. Exp Int
                                                        in  lift (Z :. j + n' :. i)) arr
      )
    ]
--}

