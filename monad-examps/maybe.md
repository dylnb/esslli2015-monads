<!-- λ ◊ ≠ ∃ Λ ∀ ≡ α β γ ρ ω φ ψ Ω ○ μ η δ ζ ξ ⋆ ★ • ∙ ● ⚫ 𝟎 𝟏 𝟐 𝟘 𝟙 𝟚 𝟬 𝟭 𝟮 ⇧ (U+2e17) ¢ -->
# The Maybe Monad: Presupposition and its Failures

* As a first example of a monad in action, we'll need a simple language that we
  need to evaluate.  We'll start with arithmetic, but move quickly to natural
  language.

  $$7 = 7$$

  $$2 + 3 = 5$$

  $$8 / (3 - 1) = 4$$

* We want these sentences to evaluate to the boolean True.


---


# Basic Arithmetic

* Start with the usual operations with their usual semantics

  $$(+),\ (-),\ (\times) :: \text{Int} \to \text{Int} \to \text{Int}$$

  $$(=) :: \text{Int} \to \text{Int} \to \text{Bool}$$


---


# Unsafe Division

* But what about

  $$8 / (3 - 3) = 4$$

* In order to evaluate this sentence, we need to divide 8 by 0, which is
  undefined.  So we need for our division operator to return either a number,
  or some object representing that an error has occurred.

  $$
  x / y =
  \begin{cases}
    \iota z.\, z \times y = x \quad & \text{if } y \neq 0\\
    \#                         & \text{otherwise}
  \end{cases}
  $$


---


# Unsafe Division (Cont'd)

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

  $$\text{Maybe}\, a = \text{Just}\, a \mid \text{Nothing}$$

  $$\eta\, x = \text{Just}\,x$$

  $$
  m \star k = 
  \begin{cases}
    \text{Nothing} \quad & \text{if } m = \text{Nothing}\\
    k\,x                 & \text{if } m = \text{Just}\,x
  \end{cases}
  $$

---


# Using the Monad

* Our division operator will need to be ready for division by zero. 

  $$
  \text{safe/} :: \text{Maybe Int} \to \text{Maybe Int} \to \text{Maybe Int}
  $$

  $$
  \text{safe/}\,m\,n =
  m \star \lambda x.\, n \star \lambda y.\,
  \begin{cases}
    \text{Nothing} \quad     & \text{if } x = 0\\
    \text{Just}\,y/x & \text{otherwise}
  \end{cases}
  $$

---


# Generalizing This Combinatorial Strategy

* For the other operators, we write a general lifting function that lifts the
  operators into the monadic computation.

  $$
  \text{map2} ::
  (a \to b \to c) \to \text{Maybe}\,a \to \text{Maybe}\,b \to \text{Maybe}\,c
  $$

  $$
  \text{map2}\,f\,u\,v =
  u \star \lambda x.\, v \star \lambda y.\, \eta\,(f\,x\,y)
  $$


---


# Building a Sentence

* Now we can compose our safe-division computation as

                  +----------------------------------+               
                  |                                  |               
                  |                                  |               
          +-------+---------+               +--------+---------+   
          |                 |               |                  |   
          |                 |               |                  |   
         η 2          +-----+------+     map2 (=)             η 5   
                      |            |
                      |            |
                   map2 (+)       η 3  
                                                                  

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

---

# A natural language application of the Maybe monad

Finding a application for the Maybe monad in natural language is not
hard.

* We already have expressions and sentences.  Instead of 

  $$2 + 3 = 5$$

  We want

  Ann and Bill saw Carl.

* All we need is the lingusitic equivalent of division by zero.

---

# Presuppositions

    * The King of France is bald.  [There is no King of France.]
    * Ann's meerkat is sick.  [Not everyone owns a meerkat.]
    * Ann finally realized that the world is round.
    * etc.

* For now, assume that DPs denote (refer to) objects. 

* Computing reference is itself compositional, but assume we have a
  black box named R.

  * R("Ann") = ann
  * R("The President of the US") = obama
  * R("The King of France") = ____?

[Kaplan on plexy](../Readings/kaplan-plexy.pdf)

---

# Let R map DPs not to objects of type Ent, but to Maybe Ent:

  * R("Ann") = unit ann = Just ann
  * R("The President of the US") = unit obama = Just obama
  * R("The King of France") = Nothing

  Then once we lift into the monad, we have

  * Ann saw Bill = Just True
  * Ann saw the President of the US = Just False.
  * Ann saw the King of France = Nothing

  Just to be clear, evaluating "Ann saw Bill" means evaluating 

  `(R Ann) (map2 saw) (R Bill)`

