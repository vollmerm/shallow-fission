{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Data.Array.Accelerate             as A hiding ( Split )
import qualified Data.Array.Accelerate   as A
import Data.Array.Accelerate.Interpreter as A
import Prelude                           as P

import Control.Exception
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Typeable
import Text.Printf

import Control.Monad.Logic

import Data.Typeable


matchDim :: (Shape a, Shape b) => a -> b -> Maybe (a :~: b)
matchDim _ _ = Nothing


-- TLM: this should be some thing that you each operation can pull on. e.g.
--      zipWith needs to pull on both pieces. However, how does zipWith assert
--      that the sizes are the same? Maybe zipWith requires each input is
--      singular.
--
data In a where
  One  :: In a
  Many :: Int         -- split dimension. How do we inject type information
       -> Int         -- number of split pieces?
       -> In a

data Out a where
  Each :: (Int -> Acc a -> Out (Acc b)) -> Out (Acc b)
  All  :: [Acc a] -> Out b

  -- Join  :: (Int -> Acc a -> Acc b) -> ([Acc b] -> Acc c) -> Out c
  -- Join  :: Out a -> Out b -> (Acc a -> Acc b -> Acc c) -> Out c
  -- Split :: Int -> (Out a -> Out b -> Out c) -> Out c  -- ???


pmap :: (Shape sh, Elt a, Elt b)
     => (Exp a -> Exp b)
     ->  In  (Array sh a)
     -> [Out (Array sh b)]
pmap f _
  = pure $ Do (\_ -> A.map f)


-- pzipWith f One One = Do $ \_ xs ->
--                      Do $ \_ ys -> A.zipWith f xs ys


pfold :: (Shape sh, Elt e)
      => (Exp e -> Exp e -> Exp e)
      -> Exp e
      ->  In  (Array (sh :. Int) e)
      -> [Out (Array sh e)]
pfold f z One          = pure $ Do (\_ -> A.fold f z)
pfold f z (Many dim _) =
  case dim of
    0 -> pure $ Join (\_ -> A.fold f z) (P.foldl1 (A.zipWith f))
    -- 1 -> pure $ Join (\_ -> A.fold f z) (P.foldl1 concatV)     -- type info!!
    _ -> empty



concatV
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Acc (Array (sh :. Int :. Int) e)
    -> Acc (Array (sh :. Int :. Int) e)
    -> Acc (Array (sh :. Int :. Int) e)
concatV x y =
  let sh1 :. xj :. xi = unlift (shape x)  :: Exp sh :. Exp Int :. Exp Int
      sh2 :. yj :. yi = unlift (shape y)  :: Exp sh :. Exp Int :. Exp Int
  in
  generate (A.lift $ (sh1 `intersect` sh2) :. (xj + yj) :. (min xi yi))
           (\ix -> let sh :. j :. i = unlift ix :: Exp sh :. Exp Int :. Exp Int
                   in  j A.<* xj ? (x ! ix, y ! A.lift (sh :. (j-xj) :. i)))

{--
data PAcc a where
  One  :: Acc a                           -> PAcc a
  Many :: Monad m => Int -> [ m (Acc a) ] -> PAcc a


pmap f xs = do
  xs' <- xs
  case xs' of
    One    a  -> return $ One (A.map f a)
    Many s as -> return $ Many s (P.map (fmap (A.map f)) as)


pfold :: (Shape sh, Elt e, Monad m)
      => (Exp e -> Exp e -> Exp e)
      -> Exp e
      -> m (PAcc (Array (sh :. Int) e))
      -> m (PAcc (Array sh e))
pfold f z xs = do
  xs' <- xs
  case xs' of
    One a       -> return $ One (A.fold f z a)
    Many dim as -> do
      as' <- sequence (P.map (fmap (A.fold f z as)) as)
      case dim of
        0 -> return
--}

