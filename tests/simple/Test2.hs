{-# LANGUAGE FlexibleContexts #-}
module Main where
import           Criterion.Main
import           Data.Array.Accelerate             ((:.) (..), Array, Elt, Shape)
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Interpreter as I
import           Data.Array.Accelerate.Fission     as F
import           Prelude                           as P hiding (concat)

testArrN :: Int -> A.Acc (A.Vector Double)
testArrN x = A.use $ A.fromList (A.Z :. x) [0..]

whileLoopF :: Int -> (Acc (A.Vector Double))
whileLoopF x =
  let arr = liftAcc $ A.use (A.fromList (A.Z :. x) [0..])
      arr' = F.map (+ 1.0) arr
  in F.map (\y -> A.iterate ((A.constant x) * 10) sqrt y) arr'

whileLoopN x = A.map (\y -> A.iterate ((A.constant x) * 10) sqrt y) (A.map (+ 1.0) (testArrN x))

main = do
  progf1 <- runTune2 $ F.combine $ whileLoopF 10000
  progf2 <- runTune2 $ F.combine $ whileLoopF 15000
  progf3 <- runTune2 $ F.combine $ whileLoopF 20000
  progf4 <- runTune2 $ F.combine $ whileLoopF 25000
  progf5 <- runTune2 $ F.combine $ whileLoopF 30000
  defaultMain [
        bgroup "while loop" [ bench "F10000" $ whnf I.run progf1
                            , bench "F15000" $ whnf I.run progf2
                            , bench "F20000" $ whnf I.run progf3
                            , bench "F25000" $ whnf I.run progf4
                            , bench "F30000" $ whnf I.run progf5
                            , bench "N10000" $ whnf I.run (whileLoopN 10000)
                            , bench "N15000" $ whnf I.run (whileLoopN 15000)
                            , bench "N20000" $ whnf I.run (whileLoopN 20000)
                            , bench "N25000" $ whnf I.run (whileLoopN 25000)
                            , bench "N30000" $ whnf I.run (whileLoopN 30000)
                            ]
       ]
