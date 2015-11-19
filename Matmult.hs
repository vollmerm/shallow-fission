{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import           Criterion.Main
import           Data.Array.Accelerate             ((:.) (..), All (..), Array,
                                                    Elt, Exp, IsFloating, IsNum,
                                                    Scalar, Shape (..), Vector,
                                                    Z (..), constant, lift, the,
                                                    unlift)
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as S
import qualified Data.Array.Accelerate.CUDA        as C
import           Fission1                          as F
import           Prelude                           as P hiding (concat, map)
import           System.Environment

arr1 = A.use $ A.fromList (Z :. 10 :. 10) [0.0..] :: A.Acc (Array A.DIM2 Float)

main = do
  n' <- getEnv "N"
  b' <- getEnv "BACKEND"
  let n   = read n' :: Int
      arr = A.use $ A.fromList (Z :. n :. n) [0.0..] :: A.Acc (Array A.DIM2 Float)
      brr = A.use $ A.fromList (Z :. n :. n) [100.0..] :: A.Acc (Array A.DIM2 Float)
  arr1 <- matMul arr
  arr2 <- return $ mmultp' (arr,arr)
  if b' == "multi"
  -- then undefined
  then defaultMain [
            bgroup "MatMult" [ bench ("multi: n = " ++ (show n)) $ whnf C.runMulti arr1
                       ]
           ]
  else defaultMain [
            bgroup "MatMult" [ bench ("normal: n = " ++ (show n)) $ whnf C.run arr2
                       ]
           ]


type Matrix a = Array A.DIM2 a

mmultp' :: forall e.
           (Elt e, IsNum e) =>
          (A.Acc (Matrix e), A.Acc (Matrix e)) -> A.Acc (Matrix e)
mmultp' (arr,brr) = matMul' arr brr

matMul' :: (IsNum e, Elt e) => A.Acc (Matrix e) -> A.Acc (Matrix e)
        -> A.Acc (Matrix e)
matMul' arr brr
  = A.fold (+) 0
   $ A.zipWith (*) arrRepl brrRepl
   where
     Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
     Z :. _     :. colsB = unlift (A.shape brr)    :: Z :. Exp Int :. Exp Int

     arrRepl             = A.replicate (lift $ Z :. All   :. colsB :. All) arr
     brrRepl             = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)


-- matMul :: (IsNum e, Elt e) => A.Acc (Matrix e) -> A.Acc (Matrix e)
--        -> TuneM (Acc (Matrix e))
-- matMul arr brr
--     = do let c = A.zipWith (*) arrRepl brrRepl
--          F.fold (+) 0 (mkacc c)
--     where
--       Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
--       Z :. _     :. colsB = unlift (A.shape brr)    :: Z :. Exp Int :. Exp Int

--       arrRepl             = A.replicate (lift $ Z :. All   :. colsB :. All) arr
--       brrRepl             = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)

-----
-- This generates a weird looking ast, and has 4 kernels (generate, 2 fold, generate)
-- We should be able to do it in 3, I think... (2 fold, generate)
matMul :: (IsNum e, Elt e) => A.Acc (Matrix e)
       -> TuneM (A.Acc (Matrix e))
matMul arr
    = do arr'    <- return $ arr
         arrRepl <- return $ A.replicate (lift $ Z :. All   :. colsB :. All) arr'
         arrt    <- return $ A.transpose arr'
         brrRepl <- return $ A.replicate (lift $ Z :. rowsA :. All   :. All) arrt
         c       <- return $ A.zipWith (*) arrRepl brrRepl
         r       <- F.fold (+) 0 $ mkacc c
         return  $  F.combine' r
    where
      Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
      Z :. _     :. colsB = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int

-- matMul :: (IsNum e, Elt e) => A.Acc (Matrix e)
--        -> TuneM (Acc (Matrix e))
-- matMul arr
--     = do arr'    <- return $ mkacc arr
--          arrRepl <- F.replicate (lift $ Z :. All   :. colsB :. All) arr'
--          c       <- F.zipWith (*) arrRepl arrRepl
--          F.fold (+) 0 c
--     where
--       Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
--       Z :. _     :. colsB = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int

