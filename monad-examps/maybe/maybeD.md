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



