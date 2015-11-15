{-# LANGUAGE FlexibleContexts #-}
module Main where
import           Criterion.Main
import           Data.Array.Accelerate      ((:.) (..), Array, Elt, Shape)
import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.CUDA as C
import           Fission1                   as F
import           Prelude                    as P hiding (concat)

testArrN :: A.Acc (A.Vector Double)
testArrN = A.use $ A.fromList (A.Z :. 100000) [0..]

whileLoopF :: Int -> TuneM (Acc (A.Vector Double))
whileLoopF x = do
  let arr = mkacc $ testArrN
  arr' <- F.map (+ 1.0) arr 
  F.map (\y -> A.iterate ((A.constant x) * 100) sqrt y) arr'

whileLoopN x = A.map (\y -> A.iterate ((A.constant x) * 100) sqrt y) (A.map (+ 1.0) testArrN)

main = do
  progf1 <- whileLoopF 10000
  progf2 <- whileLoopF 15000
  progf3 <- whileLoopF 20000
  progf4 <- whileLoopF 25000
  progf5 <- whileLoopF 30000
  let progf1' = F.combine progf1
      progf2' = F.combine progf2
      progf3' = F.combine progf3
      progf4' = F.combine progf4
      progf5' = F.combine progf5
  defaultMain [
        bgroup "while loop" [ bench "M 10000" $ whnf C.runMulti progf1'
                            , bench "M 15000" $ whnf C.runMulti progf2'
                            , bench "M 20000" $ whnf C.runMulti progf3'
                            , bench "M 25000" $ whnf C.runMulti progf4'
                            , bench "M 30000" $ whnf C.runMulti progf5'
                            , bench "F 10000" $ whnf C.run progf1'
                            , bench "F 15000" $ whnf C.run progf2'
                            , bench "F 20000" $ whnf C.run progf3'
                            , bench "F 25000" $ whnf C.run progf4'
                            , bench "F 30000" $ whnf C.run progf5'
                            , bench "N 10000" $ whnf C.run (whileLoopN 10000)
                            , bench "N 15000" $ whnf C.run (whileLoopN 15000)
                            , bench "N 20000" $ whnf C.run (whileLoopN 20000)
                            , bench "N 25000" $ whnf C.run (whileLoopN 25000)
                            , bench "N 30000" $ whnf C.run (whileLoopN 30000)
                            ]
       ]
  
