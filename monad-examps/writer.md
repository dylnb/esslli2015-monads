# Side Content as Side Effect

* Constituents do not always 

```haskell
data Writer w a = Writer (a, w)

instance Monoid w => Monad (Writer w)
  return x = Writer (x, mempty)
  W
