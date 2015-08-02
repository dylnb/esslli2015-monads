# Putting the Monad to Work

* Let's see what happens when we try division by zero:

    ```haskell
    (unit 4) (map2 +) ((unit 8) safe/ (unit 0)) (map2 =) (unit 3)

    (unit 8) safe/ (unit 0)
    ~~> (Just 8) safe/ (Just 0)
    ~~> Nothing

    (unit 4) (map2 +) ((unit 8) safe/ (unit 0)) (map2 =) (unit 3)
    ~~> (Just 4) (map2 +) ((unit 8) safe/ (unit 0)) (map2 =) (unit 3)
    ~~> (Just 4) >>= (\x. ((unit 8) safe/ (unit 0)) >>= (\y. unit (+ x y)))
    ~~> ((unit 8) safe/ (unit 0)) >>= (\y. unit (+ 4 y)))
    ~~> Nothing >>= (\y. unit (+ 4 y))
    ~~> Nothing                         
    ```

[Draw tree diagram with plumbing around the edge]
