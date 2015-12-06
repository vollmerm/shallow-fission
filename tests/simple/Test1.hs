module Main where

import           Criterion.Main
import           Prelude                           as P hiding (concat)

import           Data.Array.Accelerate             ((:.) (..), Array, Elt, Shape)
import           Data.Array.Accelerate.Fission     as F
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Interpreter as I

arrSize = 50000
bigNum  = A.constant $ 100000.0 :: A.Exp Double
steps   = A.constant $ 5000

tarr1 :: Acc (A.Vector Double)
tarr1 = liftAcc $ A.use (A.fromList (A.Z :. arrSize) [0..])
ta1 = F.map (+ 1) tarr1
ta2 = F.map (* 2) ta1
-- ta3 = do { a2' <- ta2; F.fold1 (+) a2' }
ta4 = F.zipWith (+) ta1 ta2

tfe x = A.iterate steps sqrt $ bigNum + x
tfea = F.map tfe ta4

rarr1 :: A.Acc (A.Vector Double)
rarr1 = A.use $ A.fromList (A.Z :. arrSize) [0..]

ra1 = A.map (+ 1) rarr1
ra2 = A.map (* 2) ra1
ra4 = A.zipWith (+) ra1 ra2

rfea = A.map tfe ra4

main = do
  ta4''  <- runTune2 $ F.combine ta4
  tfea'' <- runTune2 $ F.combine tfea

  defaultMain [
      bgroup "Test1" [ bench "run fission map"  $ whnf I.run ta4''
                     , bench "run fission map + while" $ whnf I.run tfea''
                     , bench "run regular map" $ whnf I.run ra4
                     , bench "run regular map + while" $ whnf I.run rfea
--                     , bench "runMulti fission map" $ whnf I.runMulti ta4''
--                     , bench "runMulti fission map + while" $ whnf I.runMulti tfea''
                     ]
     ]
