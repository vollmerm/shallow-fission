{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Array.Accelerate.Fission where

import           Data.Split

import           Data.Array.Accelerate as A hiding (Acc, Divide, Split)
import qualified Data.Array.Accelerate as A
import           Prelude               as P

import           Control.Monad


-- Interpreter sketch

data Rep a where
    Return :: a -> Rep a
    Bind   :: Rep b -> (b -> Rep a) -> Rep a
    Join   :: (b -> c -> Rep a) -> Rep b -> Rep c -> Rep a
    Use    :: a -> Rep a -- avoid collapsing use with bind

instance Show (Rep a) where
    show (Return _) = "(Return <data>)"
    show (Bind b _) = "(Bind " P.++ show b P.++ " <function>)"
    show (Join _ a b) = "(Join <function> " P.++ show a P.++ " " P.++ show b P.++ ")"
    show (Use _) = "(Use <data>)"

data FAcc a where
    FAcc :: Int -> [A.Acc a] -> FAcc a
    deriving Show

type Acc a = Rep (FAcc a)

fizzMap :: (Shape sh, Elt a, Elt b) =>
           (Exp a -> Exp b)
        -> Acc (Array sh a)
        -> Acc (Array sh b)
fizzMap f a = Bind a f'
    where f' (FAcc s as) = Return $ FAcc s $ P.map (A.map f) as
-- fizzMap f (Acc s as) = Return $ Acc s $ P.map (A.map f) as

fizzFold :: forall sh e. (Shape sh, Elt e) =>
            (Exp e -> Exp e -> Exp e)
         -> Exp e
         -> Acc (Array (sh :. Int) e)
         -> Acc (Array sh e)
fizzFold f z a = Bind a f'
    where f' (FAcc s as) =
              Bind (Return $ FAcc s $ P.map (A.fold f z) as)
                   (\a -> case s of
                            0 -> join0 f a
                            1 -> concatV a
                            _ -> error "fizzFold: DIM case not handled.")
-- fizzFold f z (Acc s as) =
--     Bind (Return $ Acc s $ P.map (A.fold f z) as)
         -- (\b -> case s of
         --          0 -> join0 f b
         --          1 -> concatV b
         --          _ -> error "fizzFold: DIM case not handled.")

fizzZipWith' :: (Shape sh, Elt a, Elt b, Elt c) =>
                (Exp a -> Exp b -> Exp c)
             -> Acc (Array sh a)
             -> Acc (Array sh b)
             -> Acc (Array sh c)
fizzZipWith' f a1 a2 =
    Bind a1 (\a1 -> (Bind a2 (\a2 -> f' a1 a2)))
    where f' (FAcc s1 a1) (FAcc _s2 a2) =
              Return $ FAcc s1 $ P.zipWith (A.zipWith f) a1 a2

zipWith' :: (Shape sh, Elt a, Elt b, Elt c) =>
            (Exp a -> Exp b -> Exp c)
         -> FAcc (Array sh a)
         -> FAcc (Array sh b)
         -> Acc (Array sh c)
zipWith' f (FAcc s1 a1) (FAcc _s2 a2) =
    -- s1 and s2 might not be equal...
    Return $ FAcc s1 $ P.zipWith (A.zipWith f) a1 a2

concatV :: FAcc (Array sh e) -> Acc (Array sh e)
concatV _a = undefined

join0 :: (Shape sh, Elt e) =>
         (Exp e -> Exp e -> Exp e)
      -> FAcc (Array sh e)
      -> Acc (Array sh e)
join0 f (FAcc s [a]) = Return $ FAcc s [a]
join0 f (FAcc s (a:as)) =
    Join (zipWith' f) (Return $ FAcc s [a]) (join0 f $ FAcc s as)
-- join0 _ (Acc s [a]) = Return $ Acc s [a]
-- join0 f (Acc s (a:as)) =
--     Join (zipWith' f) (Return $ Acc s [a]) (join0 f (Acc s as))
-- join0 _ _ = error "impossible"




arr :: Acc (Array DIM2 Float)
arr = Use $ FAcc 0 [use $ A.fromList (Z :. 10 :. 10) [0..],
                    use $ A.fromList (Z :. 10 :. 10) [0..]]

foo1 :: (Shape sh) => Acc (Array sh Float) -> Acc (Array sh Float)
foo1 as = fizzMap (+ 1) as
-- Return map

foo2 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh Float)
foo2 as = fizzMap (* 2) (foo1 as)
--foo2 as = Bind (foo1 as) $ fizzMap (* 2)
-- Bind (Return map) map

foo3 :: Shape sh => Acc (Array sh Float) -> Acc (Array sh (Float,Float))
foo3 as = fizzMap (\x -> A.lift (x,1::Float)) (foo2 as)
--foo3 as = Bind (foo2 as) $ fizzMap (\x -> A.lift (x,1::Float))
-- Bind (Bind (Return map) map) map

foo4 :: Shape sh => Acc (Array (sh :. Int) Float) -> Acc (Array sh Float)
foo4 as = fizzFold (+) 0 (foo2 as)
--foo4 as = Bind (foo2 as) $ fizzFold (+) 0
-- Bind (Bind (Return map) map) (Bind fold (Join zipwith))

foo5 :: Shape sh => Acc (Array (sh :. Int) Float) -> Acc (Array sh Float)
foo5 as = fizzMap (* 5) (foo4 as)
--foo5 as = Bind (foo4 as) $ fizzMap (* 5)
-- Bind (Bind (Bind (Return map) map) (Bind fold (Join zipwith))) map

naiveEval :: Rep a -> a
naiveEval (Return a)   = a
naiveEval (Bind b f)   = naiveEval $ f (naiveEval b) --  $ Acc 0 $ fizzCompute (naiveEval b)
naiveEval (Join f a b) = naiveEval $ f (naiveEval a) (naiveEval b)
naiveEval (Use a)      = a

-- fizzCompute :: forall a. Arrays a => Acc a -> Acc a
-- fizzCompute (FAcc s as) = FAcc s $ P.map (A.compute) as

-- λ> naiveEval $ foo1 arr
-- Acc 0 [let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0])
-- in map (\x0 -> 1.0 + x0) a0]


simplify :: Rep a -> Rep a
simplify (Return a) = Return a
simplify (Bind b f) = simplifyFunc f (simplify b)
simplify (Join f a b) = Join f (simplify a) (simplify b)
simplify (Use a) = Use a

simplifyFunc :: (b -> Rep a) -> Rep b -> Rep a
simplifyFunc f (Return a) = f a
simplifyFunc f (Bind b g) =
    let b' = simplify b
    in case b' of
         Return a -> simplifyFunc f (g a)
         _ -> Bind (Bind b' g) f
simplifyFunc f (Join g a b) =
    let a' = simplify a
        b' = simplify b
    in Bind (Join g a' b') f
simplifyFunc f (Use a) =
    Bind (Use a) f

-- simplifyFunc f (Join g a b) =
--     let a' = simplify a
--         b' = simplify b
--     in case (a',b') of
--          (Return a, Return b) -> Bind (g a b) f
--          _ -> Bind (Join g a' b') f

-- data F a where
--   Do   :: a -> F a
--   Join :: (a -> b -> t (F c)) -> F a -> F b -> F c


-- data Acc a where
--   Acc :: Int -> [A.Acc a] -> Acc a
--   -- One  :: A.Acc a -> Acc a
--   -- Many :: Int -> [A.Acc a] -> Acc a


-- amap :: (A.Acc a -> A.Acc b) -> Acc a -> Acc b
-- amap f (Acc s as) = Acc s (P.map f as)


-- map' :: (MonadPlus t, Shape sh, Elt a, Elt b)
--      => (Exp a -> Exp b)
--      -> Acc (Array sh a)
--      -> t (F (Acc (Array sh b)))
-- map' f as = return $ Do $ amap (A.map f) as

--   -- = return . Do $ Acc s (fmap (A.map f) as)

--   -- One a       -> return . Do $ One (A.map f a)
--   -- Many dim as -> return . Do $ Many dim (fmap (A.map f) as)

--   -- return $ Do (P.map (A.map f) as)



-- fold' :: forall t sh e. (MonadPlus t, Shape sh, Elt e)
--       => (Exp e -> Exp e -> Exp e)
--       -> Exp e
--       -> Acc (Array (sh :. Int) e)
--       -> t (F (Acc (Array sh e)))
-- fold' f z as@(Acc dim _) =
--   let as' = amap (A.fold f z) as
--   in case dim of
--        0 -> join0 f as'
--        1 -> concatV as'
--        _ -> mzero


-- concatV
--     :: (MonadPlus t, Shape sh, Elt e)
--     => Acc (Array sh e)
--     -> t (F (Acc (Array sh e)))
-- concatV = undefined

-- join0 :: (MonadPlus t, Shape sh, Elt e)
--       => (Exp e -> Exp e -> Exp e)
--       -> Acc (Array sh e)
--       -> t (F (Acc (Array sh e)))
-- join0 = undefined
-- -- join0 f = return $ P.foldl1 (Join (zipWith' f)) . P.map (return . Do) . fanout

-- fanout :: Acc a -> [Acc a]
-- fanout (Acc s as) = P.map (\a -> Acc s [a]) as


-- {--
--   where
--     fold'0 f z [a]    = Do $ Acc 0 [A.fold f z a]
--     fold'0 f z (a:as) =
--       Join (fold'0 f z [a]) (fold'0 f z as)
--            (zipWith' f :: Acc (Array sh e) -> Acc (Array sh e) -> t (F (Acc (Array sh e))))

--     fold'1 f z [a]    = Do $ Acc 1 [A.fold f z a]
--     fold'1 f z (a:as) = error "TODO: fold on non-zero split dimension"
-- --}


-- zipWith'
--     :: (MonadPlus t, Shape sh, Elt a, Elt b, Elt c)
--     => (Exp a -> Exp b -> Exp c)
--     -> Acc (Array sh a)
--     -> Acc (Array sh b)
--     -> t (F (Acc (Array sh c)))

-- zipWith' f (Acc s1 a1) (Acc s2 a2) = do
--   guard (s1 == s2)
--   guard (P.length a1 == P.length a2)
--   return $ Do $ Acc s1 (P.zipWith (A.zipWith f) a1 a2)









-- -- data Request a where
-- --     Request :: [Par a] -> Request [a]

-- -- data Par a where
-- --     Do   :: a -> Par a
-- --     Par  :: Par a -> Par b -> Par (a,b)
-- --     Join :: Par a -> Par b -> (a -> b -> Par c) -> Par c


-- -- type Acc a = [Par (A.Acc a)]


-- -- use :: Arrays arrs => arrs -> Acc arrs
-- -- use = return . Do . A.use

-- -- map :: (Shape sh, Elt a, Elt b)
-- --     => (Exp a -> Exp b)
-- --     -> Acc (Array sh a)
-- --     -> Acc (Array sh b)

-- {--
-- map f xs = do
--   xs' <- tune xs
--   return (P.map (A.map f) xs')


-- type TuneM a = ReaderT GenIO IO a

-- tune :: (Splittable (A.Acc a), Arrays a) => A.Acc a -> TuneM [A.Acc a]
-- tune a = do
--   action <- oneof [minBound .. maxBound]
--   case action of
--     NOP   -> return [a]
--     Force -> return [ A.compute a ]
--     Fizz  -> do
--       s <- oneof splittable
--       p <- splitpoint
--       let (x,y) = splitAt s p a
--       (P.++) <$> tune x <*> tune y

-- data Tune = NOP | Force | Fizz
--   deriving (Bounded, Enum, Show)

-- splitpoint :: TuneM Double
-- splitpoint = do
--   mwc   <- ask
--   uniformR (0,1) mwc

-- oneof :: [a] -> TuneM a
-- oneof these = do
--   mwc  <- ask
--   this <- uniformR (0, P.length these - 1) mwc
--   return (these P.!! this)
-- --}


-- -- type Tune = IO

-- -- eval :: Par a -> Tune [a]
-- -- eval (Do x)    = return [x]
-- -- eval (Par x y) =
