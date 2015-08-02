# Natural (Type) Classes

* Many types represent data that are displayable. For this, we have a single
  `show` function, which we define for each displayable type, including our
  *ad-hoc* `NP` type.

```haskell
instance Show NP where
  show (Trace v n) = "Var: " + show v + show n
  show (Name s)    = "Name: " + s
```

* Likewise, many types of things can be tested for equality. But for parametric
  types, it may depend on whether the type parameter itself is defined for such
  a test.

```haskell
instance Eq a => Eq (List a) where
  Nil == Nil                 = True
  (Cons x xs) == (Cons y ys) = x == y && xs == ys
  _ == _                     = False
```


---


