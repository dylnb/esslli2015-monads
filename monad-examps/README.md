# The Whirlwind Monad Tour

* There are plenty of phenomena in natural language that correspond to what
  programmers call **side-effects** [Shan2001](../Readings/shan-monads.pdf)

* Today we'll walk through a handful of examples, to get a sense of the breadth
  of the approach and the general pattern
    * Writer monad for denotations with secondary content
    * Reader monad for context-sensitive denotations
    * List/Set monad for constituents with indeterminate denotations
    * State monad for binding
 
* We'll also try to do some demos at the ghc interpreter to convince those to
  you who don't know already what a nice research tool a functional programming
  language can be for natural language semantics


---


# Setting the Stage

* Recall the combinatorial strategy from the maybe monad example

    ```haskell
    map2 :: (a -> b -> c) -> Maybe a -> Mabye b -> Maybe c
    map2 m n = m >>= \x -> n >>= \y -> return (f x y)
    ```

* This combinator maps a *pure* function over some potentially effectful
  arguments


---


# Setting the Stage

* First, notice that there's nothing about this combinator that depends on the
  maybe monad; it is more generally typed

    ```haskell
    map2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
    ```


---


# Setting the Stage

* Second, as a special case of this operation, we might want to map function
  application itself over two arguments: the first a monadic function, the
  second a monadic argument

    ```haskell
    (</>) :: Monad m => m (a -> b) -> m a -> m b
    (</>) = map2 (\f x -> f x)
       -- ~~> m </> n = m >>= \f -> n >>= \x -> return (f x)
    ```

* And backwards

    ```haskell
    (<\>) :: Monad m => m a -> m (a -> b) -> m b
    (<\>) = map2 (\x f -> f x)
       -- ~~> m </> n = m >>= \x -> n >>= \f -> return (f x)
    ```


---


# Setting the Stage

* So `(</>)` and `(<\>)` give effect-sensitive analogs of the traditional
  categorial forward and backward function application combinators

* And they're monad-neutral! This makes the grammar flexible and modular: any
  effect for which $\eta$ and $\star$ are defined can be combined using these
  applicative operations
