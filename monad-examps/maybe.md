# The Maybe Monad: Presupposition and its Failures

* As a first example of a monad in action, we'll need a simple language that we
  need to evaluate.  We'll start with arithmetic, but move quickly to natural
  language.

    ```haskell
    7 == 7
    2 + 3 == 5
    8 / (3 - 1) == 4
    ```

* We want these sentences to evaluate to the boolean True.


---


# Basic Arithmetic

* Start with the usual operations with their usual semantics

    ```haskell
    (+), (-), (*) :: Int -> Int -> Int
    (==) :: Int -> Int -> Bool
    ```


---


# Unsafe Division

* But what about

    8 / (3 - 3) = 4

*  In order to evaluate this sentence, we need to divide 8 by 0, which is
   undefined.  So we need for our division operator to return either a number,
   or some object representing that an error has occurred.

$$\frac{x}{y} = \begin{cases} \iota z.\, z\,y = x \quad&\textbf{if} y \neq 0\\ \# &\textbf{otherwise} \end{cases}$$


---


# Unsafe Division

* But since the result of a division can itself serve as the input to other
  arithmetic operations, as in `4 + (8 / 2)`, we also need to adjust the
  meanings of addition and all of the other arithmetic functions in order to
  prepare them for the possibility that the result of some division somewhere
  might be undefined.
 
* This is unfair --- addition is not part of the problem, yet
  must go out of its normal path in order to contribute to a solution.

* We want a more general solution, one that allows information about the
  presence of an error state to propagate throughout the computation without
  having to adjust the internal workings of operators that don't create error
  conditions.



---


# Introducing the Maybe Monad

* We'll do this with the following monad, which we'll call the Maybe monad:

    ```haskell
    data Maybe a = Just a | Nothing

    instance Monad Maybe where
      return a = Just a
      u >>= f = case u of Nothing -> Nothing
                          Just x -> f x
    ```


---


# Using the Monad

* Our division operator will need to be ready for division by zero. Let `/` be
  the usual division operator. So define 

    ```haskell
    safe/ :: Maybe Int -> Maybe Int -> Maybe Int
    safe/ m n = m >>= \x -> n >>= \y -> test x y
      where test x y = if y == 0 then Nothing else Just (x / y)
    ```

---


# Generalizing This Combinatorial Strategy

* For the other operators, we write a general lifting function that lifts the
  operators into the monadic computation.

    ```haskell
    map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    map2 f u v == u >>= (\x -> v >>= (\y -> return (f x y)))
    ```


---


# Building a Sentence

* Now we can compose our safe-division computation as

                  +----------------------------------+               
                  |                                  |               
                  |                                  |               
          +-------+---------+               +--------+---------+   
          |                 |               |                  |   
          |                 |               |                  |   
       unit 2         +-----+------+     map2 (==)          unit 5   
                      |            |
                      |            |
                   map2 (+)      unit 3
                                                                  

* Smoothly evaluates to `Just True`


---


# Putting the Monad to Work

* Let's see what happens when we try division by zero:

    ```haskell
    (unit 4) (map2 +) ((unit 8) safe/ (unit 0)) (map2 =) (unit 3)

    (unit 8) safe/ (unit 0)
    ~~> (Just 8) safe/ (Just 0)
    ~~> Nothing

    (unit 4) (map2 +) ((unit 8) safe/ (unit 0)) (map2 =) (unit 3)
    ~~> (Just 4) (map2 +) ((unit 8) safe/ (unit 0)) (map2 =) (unit 3)
    ~~> (Just 4) >>= (\x. ((unit 8) safe/ (unit 0)) >>= (\y. unit (+ x y)))
    ~~> ((unit 8) safe/ (unit 0)) >>= (\y. unit (+ 4 y)))
    ~~> Nothing >>= (\y. unit (+ 4 y))
    ~~> Nothing                         
    ```

[Draw tree diagram with plumbing around the edge]
