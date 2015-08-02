# Bind

* Another commonly overloaded function is a close cousin of `fmap` called
  *bind*, spelled `(>>=)`.

* Where different definitions of `fmap` specify how to map a simple `a -> b`
  function over some structure full of `a`s (to end up with a structure full of
  `b`s), definitions of `bind` specify how to map an *structure-generating
  function* of type `a -> m b` over a structure full of `a`s (to end up with a
  structure full of `b`s.

```haskell
class Functor m where
  fmap :: (a -> b) -> m a -> m b

class Bind m where
  bind :: (a -> m b) -> m a -> m b

instance Bind (List a) where
  bind f Nil = Nil
  bind f (Cons x xs) = concat $ Cons (f x) (bind f xs) 

instance Bind (Pair a) where
  bind f (Pair (x,y)) = Pair (fst (f x), snd (f y))
```


---


