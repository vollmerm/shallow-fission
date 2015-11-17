module Main where
import           Criterion.Main
import           Data.Array.Accelerate             ((:.) (..), Array, Elt,
                                                    Shape)
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Interpreter as C
import           Fission1                          as F
import           Prelude                           as P hiding (concat)

arrSize = 50000
bigNum  = A.constant $ 100000.0 :: A.Exp Double
steps   = A.constant $ 5000

tarr1 :: Acc (A.Vector Double)
tarr1 = mkacc $ A.use (A.fromList (A.Z :. arrSize) [0..])
ta1 = F.map (+ 1) tarr1
ta2 = do { a1' <- ta1; F.map (* 2) a1' }
ta3 = do { a2' <- ta2; F.fold1 (+) a2' }
ta4 = do { a1' <- ta1; a2' <- ta2; F.zipWith (+) a1' a2' }

tfe x = A.iterate steps sqrt $ bigNum + x
tfea = do { a4' <- ta4; F.map tfe a4' }

rarr1 :: A.Acc (A.Vector Double)
rarr1 = A.use $ A.fromList (A.Z :. arrSize) [0..]

ra1 = A.map (+ 1) rarr1
ra2 = A.map (* 2) ra1
ra4 = A.zipWith (+) ra1 ra2

rfea = A.map tfe ra4

main = do
  ta4' <- ta4
  tfea' <- tfea
  let ta4''  = F.combine ta4'
      tfea'' = F.combine tfea'

  defaultMain [
      bgroup "Test1" [ bench "run fission map"  $ whnf C.run ta4''
                     , bench "run fission map + while" $ whnf C.run tfea''
                     , bench "run regular map" $ whnf C.run ra4
                     , bench "run regular map + while" $ whnf C.run rfea
--                     , bench "runMulti fission map" $ whnf C.runMulti ta4''
--                     , bench "runMulti fission map + while" $ whnf C.runMulti tfea''
                     ]
     ]
