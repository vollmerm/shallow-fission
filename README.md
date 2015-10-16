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
MkAcc Concat along dim 0 of 2 chunks:
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0])
in transform
     (indexTail (shape a0) :. let x0 = 0 in
                              let x1 = quotRem (indexHead (shape a0),2) in
                              let x2 = #0 x1 in
                              let x3 = #1 x1 in
                              let x4 = (x0 <* x2) ? (x0,x2)
                              in ((1 <* x2) ? (x4 + x3,x3 + x2)) - x4)
     (\x0 -> indexTail x0 :. (indexHead x0)
                             +
                             (let x1 = 0 in
                              let x2 = #0 (quotRem (indexHead (shape a0),2))
                              in (x1 <* x2) ? (x1,x2)))
     (\x0 -> 2.0 * (1.0 + x0))
     a0
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0])
in transform
     (indexTail (shape a0) :. let x0 = 1 in
                              let x1 = quotRem (indexHead (shape a0),2) in
                              let x2 = #0 x1 in
                              let x3 = #1 x1 in
                              let x4 = (x0 <* x2) ? (x0 + x3,x3 + x2)
                              in ((2 <* x2) ? (x4 + x3,(2 * x3) + x2)) - x4)
     (\x0 -> indexTail x0 :. (indexHead x0)
                             +
                             (let x1 = 1 in
                              let x2 = quotRem (indexHead (shape a0),2) in
                              let x3 = #1 x2 in
                              let x4 = #0 x2
                              in (x1 <* x4) ? (x1 + x3,x3 + x4)))
     (\x0 -> 2.0 * (1.0 + x0))
     a0
```

For a3:

```
MkAcc Concat along dim 0 of 1 chunks:
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]) in
let a1 = fold1
           (\x0 x1 -> x0 + x1)
           (Delayed
              indexTail (shape a0) :. let x0 = 0 in
                                      let x1 = quotRem (indexHead (shape a0),2) in
                                      let x2 = #0 x1 in
                                      let x3 = #1 x1 in
                                      let x4 = (x0 <* x2) ? (x0,x2)
                                      in ((1 <* x2) ? (x4 + x3,x3 + x2)) - x4
              (\x0 -> let x1 =
                        let x1 =
                          let x1 =
                            indexTail x0 :. (indexHead x0)
                                            +
                                            (let x1 = 0 in
                                             let x2 = #0 (quotRem (indexHead (shape a0),2))
                                             in (x1 <* x2) ? (x1,x2))
                          in x1
                        in a0!x1
                      in 2.0 * (1.0 + x1))) in
let a2 =
  fold1
    (\x0 x1 -> x0 + x1)
    (Delayed
       indexTail (shape a0) :. let x0 = 1 in
                               let x1 = quotRem (indexHead (shape a0),2) in
                               let x2 = #0 x1 in
                               let x3 = #1 x1 in
                               let x4 = (x0 <* x2) ? (x0 + x3,x3 + x2)
                               in ((2 <* x2) ? (x4 + x3,(2 * x3) + x2)) - x4
       (\x0 -> let x1 =
                 let x1 =
                   let x1 =
                     indexTail x0 :. (indexHead x0)
                                     +
                                     (let x1 = 1 in
                                      let x2 = quotRem (indexHead (shape a0),2) in
                                      let x3 = #1 x2 in
                                      let x4 = #0 x2
                                      in (x1 <* x4) ? (x1 + x3,x3 + x4))
                   in x1
                 in a0!x1
               in 2.0 * (1.0 + x1)))
in generate (intersect (shape a1) (shape a2)) (\x0 -> (a1!x0) + (a2!x0))
```

For a4:

```
MkAcc Concat along dim 0 of 2 chunks:
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0])
in transform
     (indexTail (shape a0) :. let x0 = 0 in
                              let x1 = quotRem (indexHead (shape a0),2) in
                              let x2 = #0 x1 in
                              let x3 = #1 x1 in
                              let x4 = (x0 <* x2) ? (x0,x2)
                              in ((1 <* x2) ? (x4 + x3,x3 + x2)) - x4)
     (\x0 -> indexTail x0 :. (indexHead x0)
                             +
                             (let x1 = 0 in
                              let x2 = #0 (quotRem (indexHead (shape a0),2))
                              in (x1 <* x2) ? (x1,x2)))
     (\x0 -> let x1 = 1.0 + x0
             in x1 + (2.0 * x1))
     a0
let a0 = use (Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0])
in generate
     (intersect (indexTail (shape a0) :. let x0 = 0 in
                                         let x1 = quotRem (indexHead (shape a0),2) in
                                         let x2 = #0 x1 in
                                         let x3 = #1 x1 in
                                         let x4 = (x0 <* x2) ? (x0,x2)
                                         in ((1 <* x2) ? (x4 + x3,x3 + x2)) - x4)
                (indexTail (shape a0) :. let x0 = 1 in
                                         let x1 = quotRem (indexHead (shape a0),2) in
                                         let x2 = #0 x1 in
                                         let x3 = #1 x1 in
                                         let x4 = (x0 <* x2) ? (x0 + x3,x3 + x2)
                                         in ((2 <* x2) ? (x4 + x3,(2 * x3) + x2)) - x4))
     (\x0 -> (2.0
              *
              (1.0
               +
               (a0
                !
                indexTail x0 :. (indexHead x0)
                                +
                                (let x1 = 0 in
                                 let x2 = #0 (quotRem (indexHead (shape a0),2))
                                 in (x1 <* x2) ? (x1,x2)))))
             +
             (2.0
              *
              (1.0
               +
               (a0
                !
                indexTail x0 :. (indexHead x0)
                                +
                                (let x1 = 1 in
                                 let x2 = quotRem (indexHead (shape a0),2) in
                                 let x3 = #1 x2 in
                                 let x4 = #0 x2
                                 in (x1 <* x4) ? (x1 + x3,x3 + x4))))))

```