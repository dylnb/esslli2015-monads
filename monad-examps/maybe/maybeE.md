# Introducing the Maybe Monad

* We'll do this with the following monad, which we'll call the Maybe monad:

    ```haskell
    data Maybe a = Just a | Nothing

    instance Monad Maybe where
      return a = Just a
      u >>= f = case u of Nothing -> Nothing
                          Just x -> f x
    ```


---


