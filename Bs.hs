{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Criterion.Main
import           Data.Array.Accelerate             as A
import           Data.Array.Accelerate.Interpreter as C
import qualified Fission1                          as F
import           Prelude                           as P
import           Random
import           System.Environment
import           System.Environment

main = do
  n' <- getEnv "N"
  b' <- getEnv "BACKEND"
  let n = read n' :: Int
      r = options n
  acc <- F.runTune2 $ blackscholes r
  if b' == "multi"
  then undefined
  -- then defaultMain [ bgroup "Blackscholes" [ bench ("multi: n = " ++ (show n)) $ whnf C.runMulti acc
  --                                          ]
  --                  ]
  else defaultMain [ bgroup "Blackscholes" [ bench ("normal: n = " P.++ (show n)) $ whnf C.run (blackscholes' r)
                                           ]
                   ]


options :: Int -> Acc (Vector (Float,Float,Float))
options n = A.use $ randomArray (uniformR ((5,1,0.25),(30,100,10))) (Z :. n)

blackscholes :: (Elt a, IsFloating a) => Acc (Vector (a, a, a)) -> F.TuneM (Acc (Vector (a, a)))
blackscholes arr = do let arr' = F.mkacc arr
                      r <- F.map go arr'
                      return $ F.combine r
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


blackscholes' :: (Elt a, IsFloating a) => Acc (Vector (a, a, a)) -> Acc (Vector (a, a))
blackscholes' = A.map go
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
