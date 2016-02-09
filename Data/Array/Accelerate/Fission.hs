
module Data.Array.Accelerate.Fission
       (
         module Data.Array.Accelerate.Fission.Runner
       , module Data.Array.Accelerate.Fission.Functions
       )
       where
import           Data.Array.Accelerate.Fission.Functions
import           Data.Array.Accelerate.Fission.Runner

--------------------------------------------------------------------------------
-- TESTS
--------------------------------------------------------------------------------
--
-- TODO: Move me somewhere appropriate
--

-- arr0 :: Acc (A.Vector Double)
-- arr0 = liftAcc $ A.use (A.fromList (A.Z :. 10) [0..])

-- -- a1 = Fission1.map (+ 1) arr
-- -- a2 = do { a1' <- a1; Fission1.map (* 2) a1'}
-- -- a3 = do { a2' <- a2; Fission1.fold1 (+) a2' }
-- -- a4 = do { a1' <- a1; a2' <- a2; Fission1.zipWith (+) a1' a2' }

-- ac1 :: IO (A.Acc (Array (DIM0 :. Int) Double))
-- ac1 = runTune2 $ combine $ Data.Array.Accelerate.Fission.map (+ 1) arr0

-- ac2 :: IO (A.Acc (Array (DIM0 :. Int) Double))
-- ac2 = runTune2 $ let a = Data.Array.Accelerate.Fission.map (+ 1) arr0
--                  in combine $ zipWith (+) a a

-- ac3 :: IO (A.Acc (Array DIM2 Double))
-- ac3 = runTune2 $ combine $
--       replicate (A.constant (Z :. (3::Int) :. All)) arr0


-- a2' = do { a1' <- a1; a2' <- Fission1.map (* 2) a1'; return $ combine a2' }
-- a3' = do { a2' <- a2; a3' <- Fission1.fold1 (+) a2'; return $ combine0 a3' }
-- a4' = do { a1' <- a1; a2' <- a2; a4' <- Fission1.zipWith (+) a1' a2'; return $ combine a4' }

-- λ> let arr1 = A.use $ A.fromList (Z :. 5 :. 5) [0.0..] :: A.Acc (Array A.DIM2 Float)
-- λ> :set -XTypeOperators
-- λ> let Z :. rowsA :. _ = A.unlift (A.shape arr1) :: Z :. A.Exp Int :. A.Exp Int
-- λ> let Z :. _ :. colsA = A.unlift (A.shape arr1) :: Z :. A.Exp Int :. A.Exp Int
-- λ> let tmp1 = replicate (A.lift $ Z :. All :. colsA :. All) $ liftAcc arr1
-- λ> tmp1
-- tmp1 :: Acc (Array (((Z :. Int) :. Int) :. Int) Float)
-- λ> let tmp2 = replicate (A.lift $ Z :. rowsA :. All :. All) $ transpose (liftAcc arr1)
-- λ> tmp2
-- tmp2 :: Acc (Array (((Z :. Int) :. Int) :. Int) Float)
-- λ> let tmp3 = zipWith (*) tmp1 tmp2
-- λ> tmp3
-- tmp3 :: Acc (Array (((Z :. Int) :. Int) :. Int) Float)
-- λ> show tmp3
