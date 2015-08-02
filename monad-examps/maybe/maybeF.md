# Using the Monad

* Our division operator will need to be ready for division by zero. Let `/` be
  the usual division operator. So define 

    ```haskell
    safe/ :: Maybe Int -> Maybe Int -> Maybe Int
    safe/ m n = m >>= \x -> n >>= \y -> test x y
      where test x y = if y == 0 then Nothing else Just (x / y)
    ```

---


