{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where
import           Data.Array.Accelerate             as A
import           Data.Array.Accelerate.Interpreter as C
import           Prelude                           (div, max, quotRem,
                                                    undefined, ($), (*), (+),
                                                    (-), (/), (/=), (<), (<=),
                                                    (==), (>), (>=))
import qualified Prelude                           as P

type Matrix a = Array DIM2 a

add = A.zipWith (+)
sub = A.zipWith (-)

-- Divide and conquer algorithm for potentially non-square matrices.
-- Bottoms out to `mul'` which could be CUBLAS gemm or naive Accelerate
-- implementation.
mul :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
mul a b =
    let v = max' n m1 p
    in acond (v ==* n)
       (let (a1,a2) = splitHoriz a
            a1b     = mul' a1 b
            a2b     = mul' a2 b
        in concatHoriz a1b a2b)
       (acond (v ==* p)
        (let (b1,b2) = splitVert b
             ab1 = mul' a b1
             ab2 = mul' a b2
         in concatVert ab1 ab2)
        (let (a1,a2) = splitVert a
             (b1,b2) = splitHoriz b
             a1b1 = mul' a1 b1
             a2b2 = mul' a2 b2
         in add a1b1 a2b2))
    where Z :. n  :. m1 = unlift (shape a)
          Z :. (m2 :: Exp Int) :. p  = unlift (shape b)

-- More sophisticated algorithm, assumes square power-of-2 matrices.
strassen :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
strassen a b = concatFour c11 c12 c21 c22
    where (a11,a12,a21,a22) = splitFour a
          (b11,b12,b21,b22) = splitFour b
          m1 = mul' (a11 `add` a22) (b11 `add` b22)
          m2 = mul' (a21 `add` a22) b11
          m3 = mul' a11 (b12 `sub` b22)
          m4 = mul' a22 (b21 `sub` b11)
          m5 = mul' (a11 `add` a12) b22
          m6 = mul' (a21 `sub` a11) (b11 `add` b12)
          m7 = mul' (a12 `sub` a22) (b21 `add` b22)
          c11 = m1 `add` m4 `sub` m5 `add` m7
          c12 = m3 `add` m5
          c21 = m2 `add` m4
          c22 = m1 `sub` m2 `add` m3 `add` m6


max' :: Exp Int -> Exp Int -> Exp Int -> Exp Int
max' a b c =
    cond (a >* b)
             (cond (a >* c) a c)
             (cond (b >* c) b c)

splitHoriz :: Acc (Matrix Double) -> (Acc (Matrix Double), Acc (Matrix Double))
splitHoriz arr = (generate sh1 f1, generate sh2 f2)
    where f1 i = arr ! i
          f2 i = arr ! adjust i
          adjust i =
              let Z :. (m1 :: Exp Int) :. (n1 :: Exp Int) = unlift (shape arr)
                  Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                  m = mi + (m1 `div` (constant 2))
              in lift $ Z :. m :. ni
          sh1 = let Z :. m1 :. n1 = unlift (shape arr)
                in lift $ Z :. (m1 `div` ((constant 2) :: Exp Int)) :. (n1 :: Exp Int)
          sh2 = let Z :. m2 :. n2 = unlift (shape arr)
                in lift $ Z :. (m2 `div` ((constant 2) :: Exp Int)) :. (n2 :: Exp Int)

splitVert :: Acc (Matrix Double) -> (Acc (Matrix Double), Acc (Matrix Double))
splitVert arr = (generate sh1 f1, generate sh2 f2)
    where f1 i = arr ! i
          f2 i = arr ! adjust i
          adjust i =
              let Z :. (m1 :: Exp Int) :. (n1 :: Exp Int) = unlift (shape arr)
                  Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                  n = ni + (n1 `div` (constant 2))
              in lift $ Z :. mi :. n
          sh1 = let Z :. m1 :. n1 = unlift (shape arr)
                in lift $ Z :. (m1 :: Exp Int) :. (n1 `div` ((constant 2) :: Exp Int))
          sh2 = let Z :. m2 :. n2 = unlift (shape arr)
                in lift $ Z :. (m2 :: Exp Int) :. (n2 `div` ((constant 2) :: Exp Int))

splitFour :: Acc (Matrix Double)
          -> (Acc (Matrix Double), Acc (Matrix Double), Acc (Matrix Double), Acc (Matrix Double))
splitFour arr = (generate sh f1, generate sh f2, generate sh f3, generate sh f4)
    where f1 i = arr ! i
          f2 i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                     i' = lift $ Z :. mi :. ni + (n `div` (constant 2))
                 in arr ! i'
          f3 i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                     i' = lift $ Z :. mi + (m `div` (constant 2)) :. ni
                 in arr ! i'
          f4 i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                     i' = lift $ Z :. mi + (m `div` (constant 2)) :. ni + (n `div` (constant 2))
                 in arr ! i'
          sh = let Z :. m :. n = unlift (shape arr)
               in lift $ Z :. (m `div` (constant 2)) :. (n `div` (constant 2))
          Z :. (m :: Exp Int) :. (n :: Exp Int) = unlift (shape arr)

concatVert :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
concatVert = (A.++)

concatHoriz :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
concatHoriz a b = generate sh f
    where sh = lift $ Z :. (ma + mb) :. na
          f i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                in lift (mi <* ma) ?
                       ((a ! i),
                        (b ! (lift $ Z :. (mi - ma) :. ni)))
          Z :. (ma :: Exp Int) :. (na :: Exp Int) = unlift (shape a)
          Z :. (mb :: Exp Int) :. (nb :: Exp Int) = unlift (shape b)

-- (a b)
-- (c d)
concatFour :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
           -> Acc (Matrix Double)
concatFour a b c d = concatVert (concatHoriz a c) (concatHoriz b d)

main = do
  let xs = use $ fromList (Z :. 12 :. 12) [0.0..]
      ys = use $ fromList (Z :. 12 :. 12) [1.0..]
  P.print $ C.run $ mul' xs ys
  P.print $ C.run $ mul xs ys
  P.print $ C.run $ strassen xs ys

-- bottom out to this or cublas
mul' :: (IsNum e, Elt e) => A.Acc (Matrix e) -> A.Acc (Matrix e)
     -> A.Acc (Matrix e)
mul' arr brr
  = A.fold (+) 0
   $ A.zipWith (*) arrRepl brrRepl
   where
     Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
     Z :. _     :. colsB = unlift (A.shape brr)    :: Z :. Exp Int :. Exp Int

     arrRepl             = A.replicate (lift $ Z :. All   :. colsB :. All) arr
     brrRepl             = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)
