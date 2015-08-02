# Monads

* These structure-generating `a -> m b` functions are called **Kliesli
  arrows**.

* Any type for which there is also a special "trivial" Kliesli arrow is
  called a **monad**.

```haskell
class Bind m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>=)  = flip bind

instance Monad List where
  return x = Cons x Nil

instance Monad Pair where
  return x = Pair (x, x)
```


---


