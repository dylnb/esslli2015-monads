# Algebraic Types

* Product Types

```haskell
data Trace = Trace {var :: Char, tag :: Int}

newtrace :: Trace
newtrace = Trace 't' 1

trace2 :: Trace
trace2 = Trace {var = 't', tag = 4}
```

* Sum Types

```haskell
data NP = Trace | Name String

john :: NP
john = Name "John"

x :: NP
x = trace2
```

---


