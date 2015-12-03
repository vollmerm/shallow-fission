{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import           Criterion.Main
import           System.Environment
import           Prelude as P hiding (concat, map)

import           Data.Array.Accelerate (Exp, IsNum, lift, unlift)
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate.Array.Sugar as S
import           Data.Array.Accelerate.Fission as F

import qualified Data.Array.Accelerate.Interpreter as I

-- import qualified Data.Array.Accelerate.Trafo.Sharing -- hidden module.
import qualified Data.Array.Accelerate.Trafo as Tr
import           Debug.Trace


arr1 :: A.Acc (Array A.DIM2 Float)
arr1 = A.use $ A.fromList (Z :. 10 :. 10) [0.0..]

main :: IO ()
main = do
  n' <- getEnv "N"
  b' <- getEnv "BACKEND"
  let n   = read n' :: Int
      arr = A.use $ A.fromList (Z :. n :. n) [0.0..] :: A.Acc (Array A.DIM2 Float)
      _brr = A.use $ A.fromList (Z :. n :. n) [100.0..] :: A.Acc (Array A.DIM2 Float)

      -- arr2 = mmultp' (arr,arr)

  putStrLn "Calling tuner to build matMul program..."
  arr2 <- runTune2 $ matMul arr

  let noFuse = Tr.convertAccWith (Tr.phases { Tr.enableAccFusion = False , Tr.floatOutAccFromExp = False})
                  arr2

  putStrLn "\n================================================================================"
  putStrLn $ "printed AST WITHOUT fusion matMult:\n" ++ (show noFuse)

  putStrLn "\n================================================================================"
  putStrLn $ "printed AST with fusion matMult:\n" ++ (show arr2)

  putStrLn $ "Result of running:\n " ++ show (I.run arr2)
{-

  if b' == "multi"
  then undefined
  -- then defaultMain [
  --           bgroup "MatMult" [ bench ("multi: n =" ++ (show n)) $ whnf I.runMulti arr1
  --                      ]
  --          ]
  else defaultMain [
            bgroup "MatMult" [ bench ("normal: n =" ++ (show n)) $ whnf I.run arr2
                       ]
           ]
        -}

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
--          F.fold (+) 0 (liftAcc c)
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
    -- = do arr'    <- return $ arr
            -- arrRepl <- return $ A.replicate (lift $ Z :. All   :. colsB :. All) arr'
            -- arrt    <- return $ A.transpose arr'
            -- brrRepl <- return $ A.replicate (lift $ Z :. rowsA :. All   :. All) arrt
            -- c       <- return $ liftAcc $ A.zipWith (*) arrRepl brrRepl
    -- this one has the extra backpermutes:
    = let arr'    = liftAcc arr
          arrRepl = F.replicate (lift $ Z :. All   :. colsB :. All) arr'
          arrt    = F.transpose arr'
          brrRepl = F.replicate (lift $ Z :. rowsA :. All   :. All) arrt
          c       = F.zipWith (*) arrRepl brrRepl
          r = F.fold (+) 0 c
       in trace ("FYI: Calling matMul, fissioning version") $
          F.combine r
    where
      Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
      Z :. _     :. colsB = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int

-- matMul :: (IsNum e, Elt e) => A.Acc (Matrix e)
--        -> TuneM (Acc (Matrix e))
-- matMul arr
--     = do arr'    <- return $ liftAcc arr
--          arrRepl <- F.replicate (lift $ Z :. All   :. colsB :. All) arr'
--          c       <- F.zipWith (*) arrRepl arrRepl
--          F.fold (+) 0 c
--     where
--       Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
--       Z :. _     :. colsB = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
-- test1  :: (IsNum e, Elt e) => A.Acc (Matrix e)
--        -> TuneM (Acc (Matrix e))

test1
    :: (Elt a, IsNum a)
    => A.Acc (Matrix a)
    -> TuneM (A.Acc (Matrix a))
test1 arr
    = do let arrRepl = A.replicate (lift $ Z :. All   :. colsB :. All) arr
             brrRepl = A.replicate (lift $ Z :. rowsA :. All   :. All) $ A.transpose arr
             newArr  = A.zipWith (*) arrRepl brrRepl
         (a1,a2) <- split 0 newArr
         return $ A.zipWith (+) (A.fold (+) 0 a1) (A.fold (+) 0 a2)
    where
      Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
      Z :. _     :. colsB = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
