# Functors

* Like equality, there are other polymorphic functions that are expected to
  behave according to particular *laws*.

```haskell
class Functor m where
  fmap :: (a -> b) -> m a -> m b
  -- note that a and b can be any types whatsoever here; this tells you that
  -- being "mappable" is a property of the type constructor, regardless of what
  -- sorts of values it contains

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Functor Pair where
  fmap (Pair (x, y)) = Pair (f x, f y)
```

* Any specialization of the mapping function `fmap` for a custom type `M`
  should satisfy:
    * `fmap id m` is equivalent to `m`, for any `m :: M`
    * `fmap (g ○ h) m` is equivalent to `fmap g (fmap h m)` , for any `m :: M`

* When it does, we call `M` a **functor** under its definition of `fmap`


---


