module Main where
import           Criterion.Main
import           Data.Array.Accelerate      ((:.) (..), Array, Elt, Shape)
import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.CUDA as C
import           Fission1                   as F
import           Prelude                    as P hiding (concat)

arrSize = 50000
bigNum  = A.constant $ 100000.0 :: A.Exp Double
steps   = A.constant $ 1000

tarr1 :: Acc (A.Vector Double)
tarr1 = mkacc $ A.use (A.fromList (A.Z :. arrSize) [0..])
ta1 = F.map (+ 1) tarr1
ta2 = do { a1' <- ta1; F.map (* 2) a1' }
ta3 = do { a2' <- ta2; F.fold1 (+) a2' }
ta4 = do { a1' <- ta1; a2' <- ta2; F.zipWith (+) a1' a2' }

tfe x = A.iterate steps sqrt $ bigNum + x
tfea = do { a4' <- ta4; F.map tfe a4' }

main = do
  ta4' <- ta4
  tfea' <- tfea
  let ta4''  = F.combine ta4'
      tfea'' = F.combine tfea'
  
  defaultMain [
      bgroup "Test1" [ bench "ta4"  $ whnf C.run ta4''
                     , bench "tfea" $ whnf C.run tfea''
                     ]
     ]
