# Side content as side effect

## Appositives

> The concept of a monad, which arises from category theory, has been applied
> by Moggi to structure the denotational semantics of programming languages

-- Wadler (*Monads for Functional Programming*)

* How should we represent the meaning of this sentence?

* Two basic empirical facts to attend to:

    * The appositive seems to be invisible to other semantic operators

      *The concept of a monad, which arises from category theory, has not been
      applied to structure the semantics of languages*
      
      *Has the concept of a monad, which arises from category theory, been
      applied to structure the semantics of languages?*

    * The appositive is asserted, not presupposed


---


* Constituents with appositive content seem to be two-dimensional, split into
    * a foreground component which absorbs the semantic blow from the rest of the
      sentence
    * and a background component which just hangs along for the ride

* $$\langle$$ The concept structures the semantics of languages, The concept
  arises from category theory $$\rangle$$


---


* Even though the secondary content is immune to semantic operations, it is
  persistent, and it *accumulates*

  *The concept of a monad, which arises from category theory, has been
  applied to structure the semantics of programming languages, which Montague
  thought were just like English*

* $$\langle$$ The concept structures the semantics of languages, The concept
  arises from category theory **and** Montague thinks programming languages are
  like English$$\rangle$$


---


```haskell
Writer w a = (a, w)

return x = (x, ∅)
m >>= k = (y, w ◇ w')
  where (x, w) = m
        (y, w') = k x
```
