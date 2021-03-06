## Build status

 * Jenkins: [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=accelerate-shallow-fission)](http://tester-lin.soic.indiana.edu:8080/job/accelerate-shallow-fission/)
 * Travis: [![Build Status](https://travis-ci.org/vollmerm/shallow-fission.svg?branch=master)](https://travis-ci.org/vollmerm/shallow-fission)

## Haskell code

```haskell
arr = use (fromList (A.Z :. 10) [0..])
a1 = map (+ 1) arr
a2 = map (* 2) a1
a3 = fold1 (+) a2
a4 = zipWith (+) a1 a2
```

## Generated Accelerate code

For a2:

```
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]) in
let a1 = generate
           (indexTail (shape a0) :. #1 (quotRem (indexHead (shape a0),2))) (\x0 -> 2.0 * (1.0 + (a0!x0))) in
let a2 =
  generate
    (indexTail (shape a0) :. let x0 = quotRem (indexHead (shape a0),2)
                             in (#1 x0) + (#0 x0))
    (\x0 -> 2.0 * (1.0 + (a0!indexTail x0 :. (indexHead x0) + (#1 (quotRem (indexHead (shape a0),2))))))
in generate
     (let x0 = shape a1 in
      let x1 = shape a2
      in intersect (indexTail x0) (indexTail x1) :. (indexHead x0) + (indexHead x1))
     (\x0 -> let x1 = indexHead x0 in
             let x2 = indexHead (shape a1)
             in (x1 <* x2) ? (a1!x0,a2!indexTail x0 :. x1 - x2))
```

For a3:

```
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]) in
let a1 = fold1
           (\x0 x1 -> x0 + x1)
           (Delayed
              indexTail (shape a0) :. #1 (quotRem (indexHead (shape a0),2)) (\x0 -> 2.0 * (1.0 + (a0!x0)))) in
let a2 =
  fold1
    (\x0 x1 -> x0 + x1)
    (Delayed
       indexTail (shape a0) :. let x0 = quotRem (indexHead (shape a0),2)
                               in (#1 x0) + (#0 x0)
       (\x0 -> 2.0
               *
               (1.0 + (a0!indexTail x0 :. (indexHead x0) + (#1 (quotRem (indexHead (shape a0),2)))))))
in generate (intersect (shape a1) (shape a2)) (\x0 -> (a1!x0) + (a2!x0))
```

For a4:

```
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]) in
let a1 = generate
           (indexTail (shape a0) :. #1 (quotRem (indexHead (shape a0),2)))
           (\x0 -> let x1 = 1.0 + (a0!x0)
                   in x1 + (2.0 * x1)) in
let a2 =
  generate
    (indexTail (shape a0) :. let x0 = quotRem (indexHead (shape a0),2)
                             in (#1 x0) + (#0 x0))
    (\x0 -> let x1 = 1.0 + (a0!indexTail x0 :. (indexHead x0) + (#1 (quotRem (indexHead (shape a0),2))))
            in x1 + (2.0 * x1))
in generate
     (let x0 = shape a1 in
      let x1 = shape a2
      in intersect (indexTail x0) (indexTail x1) :. (indexHead x0) + (indexHead x1))
     (\x0 -> let x1 = indexHead x0 in
             let x2 = indexHead (shape a1)
             in (x1 <* x2) ? (a1!x0,a2!indexTail x0 :. x1 - x2))
```
