{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Array.Fission where

-- import Data.Split

import Data.Array.Accelerate                                      as A hiding ( Acc, Split )
import qualified Data.Array.Accelerate                            as A
import Prelude                                                    as P

import Control.Monad


data F a where
  Do   :: a -> F a
  Join :: (a -> b -> t (F c)) -> F a -> F b -> F c


data Acc a where
  Acc :: Int -> [A.Acc a] -> Acc a
  -- One  :: A.Acc a -> Acc a
  -- Many :: Int -> [A.Acc a] -> Acc a


amap :: (A.Acc a -> A.Acc b) -> Acc a -> Acc b
amap f (Acc s as) = Acc s (P.map f as)


map' :: (MonadPlus t, Shape sh, Elt a, Elt b)
     => (Exp a -> Exp b)
     -> Acc (Array sh a)
     -> t (F (Acc (Array sh b)))
map' f as = return $ Do $ amap (A.map f) as

  -- = return . Do $ Acc s (fmap (A.map f) as)

  -- One a       -> return . Do $ One (A.map f a)
  -- Many dim as -> return . Do $ Many dim (fmap (A.map f) as)

  -- return $ Do (P.map (A.map f) as)



fold' :: forall t sh e. (MonadPlus t, Shape sh, Elt e)
      => (Exp e -> Exp e -> Exp e)
      -> Exp e
      -> Acc (Array (sh :. Int) e)
      -> t (F (Acc (Array sh e)))
fold' f z as@(Acc dim _) =
  let as' = amap (A.fold f z) as
  in case dim of
       0 -> join0 f as'
       1 -> concatV as'
       _ -> mzero


concatV
    :: (MonadPlus t, Shape sh, Elt e)
    => Acc (Array sh e)
    -> t (F (Acc (Array sh e)))
concatV = undefined

join0 :: (MonadPlus t, Shape sh, Elt e)
      => (Exp e -> Exp e -> Exp e)
      -> Acc (Array sh e)
      -> t (F (Acc (Array sh e)))
join0 = undefined
-- join0 f = return $ P.foldl1 (Join (zipWith' f)) . P.map (return . Do) . fanout

fanout :: Acc a -> [Acc a]
fanout (Acc s as) = P.map (\a -> Acc s [a]) as


{--
  where
    fold'0 f z [a]    = Do $ Acc 0 [A.fold f z a]
    fold'0 f z (a:as) =
      Join (fold'0 f z [a]) (fold'0 f z as)
           (zipWith' f :: Acc (Array sh e) -> Acc (Array sh e) -> t (F (Acc (Array sh e))))

    fold'1 f z [a]    = Do $ Acc 1 [A.fold f z a]
    fold'1 f z (a:as) = error "TODO: fold on non-zero split dimension"
--}


zipWith'
    :: (MonadPlus t, Shape sh, Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)
    -> Acc (Array sh a)
    -> Acc (Array sh b)
    -> t (F (Acc (Array sh c)))

zipWith' f (Acc s1 a1) (Acc s2 a2) = do
  guard (s1 == s2)
  guard (P.length a1 == P.length a2)
  return $ Do $ Acc s1 (P.zipWith (A.zipWith f) a1 a2)









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

{--
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
--}


-- type Tune = IO

-- eval :: Par a -> Tune [a]
-- eval (Do x)    = return [x]
-- eval (Par x y) =

