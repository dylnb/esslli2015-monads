# Monad Laws

* But as with `(==)` and `fmap`, these definitions of `bind` and `return` are
  expected to satisfy certain laws
    * left identity: `return x >>= f` is equivalent to `f x`
    * right identity: `m >>= return` is equivalent to `m`
    * associativity: `(m >>= f) >>= g` is equivalent to `m >>= (\x -> f x >>= g)`

* Loosely speaking, the left identity law guarantees that definitions of
  `return` don't have any side effects.

* The right identity law guarantees that definitions of `return` don't consume
  or modify any side effects.

* The associativity law says that chains of effects should be *linear*

* And that's it. A monad is a singly-parameterized type constructor together
  with a mapping function called **bind** and an injection function
  called **unit** that satisfy the monad laws.
