# Generalizing This Combinatorial Strategy

* For the other operators, we write a general lifting function that lifts the
  operators into the monadic computation.

    ```haskell
    map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    map2 f u v == u >>= (\x -> v >>= (\y -> return (f x y)))
    ```


---


