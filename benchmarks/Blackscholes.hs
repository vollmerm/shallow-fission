{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE ParallelListComp          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

import Data.Array.Accelerate                        as A hiding ( Split )
import qualified Data.Array.Accelerate              as A
import Data.Array.Accelerate.LLVM.Native            as A
import qualified Data.Array.Accelerate.LLVM.PTX     as PTX
import Prelude                                      as P

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Parallel
import Data.Monoid
import Data.Typeable
import System.Random.MWC                            as MWC
import Text.Printf

import Data.Array.Accelerate.Fission
import Criterion.Main
import Data.Array.Accelerate.System.Random.SFMT     as A


bs1 :: Vector (Float,Float,Float) -> Double -> IO (E (Array DIM1 (Float,Float)))
bs1 d1 n = do let s = stune $ schedulen blackscholes d1 n
              return $ exec1 s False

bs0 :: Vector (Float,Float,Float) -> IO (E (Array DIM1 (Float,Float)))
bs0 d1 = do let s = stune $ schedule $ blackscholes (puse d1)
            return $ exec s 

bs2 :: Vector (Float,Float,Float) -> IO (E (Array DIM1 (Float,Float)))
bs2 d1 = do let s = stune $ schedule $ blackscholes (puse d1)
            return $ exec2 s

bsboth n = do
  d <- mkData (n * 1000000)
  bs0' <- bs0 d
  bs1' <- bs1 d 0.8
  bs2' <- bs2 d
  defaultMain
    [ bench ("GPU " P.++ (show n) P.++ " (early)") $ whnf eval bs2'
    , bench ("CPU " P.++ (show n) P.++ " (early)") $ whnf eval bs0'
    , bench ("CPU+GPU " P.++ (show n) P.++ " (early)") $ whnf eval bs1'
    ]

main = do
  bsboth 5
  bsboth 7
  bsboth 9
  bsboth 11
  bsboth 13
  bsboth 15
  bsboth 20
  bsboth 25
  bsboth 30
  bsboth 40
  bsboth 50


-- bsbench n1 n2 = do
--   d <- mkData (n1 * 1000000)
--   bs1' <- bs1 d n2
--   defaultMain
--     [ bgroup ("N=" P.++ (show n1))
--       [ bench ("CPU+GPU " P.++ (show n2) P.++ " (early)") $ whnf eval bs1'
--       ]
--     ]

-- tryall n = do
--   bsbench n 0.05
--   bsbench n 0.1
--   bsbench n 0.2
--   bsbench n 0.3
--   bsbench n 0.5
--   bsbench n 0.7
--   bsbench n 0.8
--   bsbench n 0.9
--   bsbench n 0.95


--
-- Black-Scholes option pricing ------------------------------------------------
--

riskfree, volatility :: Floating a => a
riskfree   = 0.02
volatility = 0.30

horner :: Num a => [a] -> a -> a
horner coeff x = x * foldr1 madd coeff
  where
    madd a b = a + x*b

cnd' :: Floating a => a -> a
cnd' d =
  let poly     = horner coeff
      coeff    = [0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]
      rsqrt2pi = 0.39894228040143267793994605993438
      k        = 1.0 / (1.0 + 0.2316419 * abs d)
  in
  rsqrt2pi * exp (-0.5*d*d) * poly k

mkData :: (Floating a, A.Variate a, Elt a) => Int -> IO (Vector (a,a,a))
mkData n =
  randomArray (A.uniformR ((5,1,0.25), (30,100,10))) (Z :. n)

blackscholes
  :: (IsFloating t, Shape sh, Elt t) =>
     Arr (Acc (Array sh (t, t, t))) -> Arr (Acc (Array sh (t, t)))
blackscholes = pmap go
  where
    go x =
        let (price, strike, years) = A.unlift x
            r       = A.constant riskfree
            v       = A.constant volatility
            v_sqrtT = v * sqrt years
            d1      = (log (price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
            d2      = d1 - v_sqrtT
            cnd d   = let c = cnd' d in d >* 0 ? (1.0 - c, c)
            cndD1   = cnd d1
            cndD2   = cnd d2
            x_expRT = strike * exp (-r * years)
        in
          A.lift ( price * cndD1 - x_expRT * cndD2
                 , x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1))

