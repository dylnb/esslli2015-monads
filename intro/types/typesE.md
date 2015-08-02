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


