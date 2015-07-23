# Basic Types

* Char, Int, Bool, ...
* Tuples

    ```haskell
    pos3, neg3 :: (Int, Bool)
    pos3 = (3, True)
    neg3 = (3, False)
    ```

* Functions

    ```haskell
    plus3 :: Int -> Int
    plus3 x = 3 + x
    ```
 

---


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


# *Ad hoc* Polymorphism

* The `show` and `(==)` functions are *overloaded* to work on many different
  types, and what they do with each type is your prerogative.

* This makes sense for `show`, but you might expect `(==)` to be somewhat more
  restricted. For instance, this definition doesn't match anybody's
  expectations about what an equality test should do.

    ```haskell
    instance Eq NP where
      _ == _  = False
    ```

* For things like equality, we say that any specification of the `(==)`
  relation for some particular type should satisfy a few well-known conditions.
    * reflexivity: $$\forall a.\  a = a$$
    * symmetry: $$\forall a,b.\   a = b \Rightarrow b = a$$
    * transitivity: $$\forall a,b,c.\   a = b \wedge b = c \Rightarrow a = c$$

    
<!-- * reflexivity: `∀α. α == α` -->
<!-- * symmetry: `∀α,β. α == β ⇒ β == α` -->
<!-- * transitivity: `∀α,β,γ. α == β && β == γ ⇒ α == γ` -->


---


# Functors

* Like equality, there are other polymorphic functions that are expected to
  behave according to particular *laws*.

```haskell
class Functor m where
  fmap :: (a -> b) -> m a -> m b
  -- note that a and b can be any types whatsoever here; this tells you that
  -- being "mappable" is a property of the type constructor, regardless of what
  -- sorts of values it contains

instance Functor (List a) where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Functor (Pair a) where
  fmap (Pair (x, y)) = Pair (f x, f y)
```

* Any specialization of the mapping function `fmap` for a custom type `M`
  should satisfy:
    * `fmap id m` is equivalent to `m`, for any `m :: M`
    * `fmap (g ○ h) m` is equivalent to `fmap g (fmap h m)` , for any `m :: M`

* When it does, we call `M` a **functor** under its definition of `fmap`


---


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

instance Monad (List a) where
  return x          = Cons x Nil

instance Monad (Pair a) where
  return x           = Pair (x, x)
```


---


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
