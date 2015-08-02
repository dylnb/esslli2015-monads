# Unsafe Division

* But what about

    8 / (3 - 3) = 4

*  In order to evaluate this sentence, we need to divide 8 by 0, which is
   undefined.  So we need for our division operator to return either a number,
   or some object representing that an error has occurred.

$$
\frac{x}{y} =
\begin{cases}
  \iota z.\, z \times y = x \quad & \text{if} y \neq 0\\
  \#                         & \text{otherwise}
\end{cases}
$$


---


