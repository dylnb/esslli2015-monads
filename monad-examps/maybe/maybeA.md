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


