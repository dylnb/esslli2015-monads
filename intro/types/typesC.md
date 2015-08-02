# Parameterized Types

* **Data constructors** like `Name` convert values from one type to another, in
  this case strings to NPs. They are functions that take values and return
  values

```haskell
:t Name
Name :: String -> NP
```

* Similarly, **type constructors** convert types to new types. They are
  functions that take *types* and return *types*

```haskell
data List a = Empty | Cons a (List a)

domain :: List Entity
domain = Cons John (Cons Mary Nil)

-- domain :: [Entity]
-- domain = [John, Mary]

data Pair a = Pair (a, a)

theSmiths :: Pair Entity
theSmiths = Pair (John, Mary)

point :: Pair Int
point = Pair (3, 4)
```

---


