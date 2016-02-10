{-# LANGUAGE FlexibleContexts #-}
module Main where
import           Criterion.Main
import           Data.Array.Accelerate             ((:.) (..), Array, Elt,
                                                    Shape)
import qualified Data.Array.Accelerate             as A
import           Data.Array.Accelerate.Fission     as F
import qualified Data.Array.Accelerate.Interpreter as I
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

  let progf1 = whileLoopF 10000
      progf2 = whileLoopF 15000
      progf3 = whileLoopF 20000
      progf4 = whileLoopF 25000
      progf5 = whileLoopF 30000
  defaultMain [
        bgroup "while loop" [ bench "F10000" $ whnf F.run' progf1
                            , bench "F15000" $ whnf F.run' progf2
                            , bench "F20000" $ whnf F.run' progf3
                            , bench "F25000" $ whnf F.run' progf4
                            , bench "F30000" $ whnf F.run' progf5
                            , bench "N10000" $ whnf I.run (whileLoopN 10000)
                            , bench "N15000" $ whnf I.run (whileLoopN 15000)
                            , bench "N20000" $ whnf I.run (whileLoopN 20000)
                            , bench "N25000" $ whnf I.run (whileLoopN 25000)
                            , bench "N30000" $ whnf I.run (whileLoopN 30000)
                            ]
       ]
