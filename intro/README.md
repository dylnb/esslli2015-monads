# Introduction

* Monads are a technique from functional programming for articulating
  computations into layers

[This discussion inspired by Wadler's "Monads for functional programming".]

For instance, imagine you are building a calculator program that
  evaluates artithmetic expressions.  You write code that performs
  addition, subtraction, multiplication, and division, so that you can
  evaluate expressions such as `(* 1 (+ (/ 4 2) 3))`.  Once that
  works, you decide to add the following features:

* You decide you want to fail gracefully when there is division by
  zero.  The obvious strategy: modify all of the code written so far
  in order to handle an error condition.  This is unsatisfying, since
  there is no reason why the code for addition ought to worry about
  division by zero.

* You decide to add a `let` construction, e.g.,

    `let x = 2 in (* 1 (+ (/ 4 x) 3))`

  But this requires rewriting all the code again in order to deal with
  assigning values to variables.

* You decide to count the number of arithmetic operations performed
  for billing purposes.  But this requires rewriting all of the basic
  operations yet again.

## Or you could use monads

Monads allow adding independent layers to the computation.  The basic
computation performs the nested arithmetic operations.  On top of
that, there is a layer tracking whether division by zero has been
attempted.  Independently, there is a layer tracking the values of
variables.  Finally, there can be a layer tracking metainformation
about the ongoing computation.  Using monadic techniques, the orginal
arithmetic computations can be used without modification.

## The connection with natural language

Evaluating expressions is essentially what a formal grammar for a
natural language does.  Instead of `(* 1 (+ (/ 4 2) 3))`, we might
have `((the man)(fed (his mongoose)))`.  Each of the computational
features imagined above have fairly close analogs in the study of
natural language.

* Presupposition as throwing an error: many expressions require some
  precondition to be met in order to have a well-defined meaning.  For
  instance, possessives such as `his mongoose` presuppose that the
  person referred to owns a mongoose.  If this precondition is not
  met, the larger expression fails to have a well-defined meaning,
  just as if we had tried to divide a number by zero.

* Pronouns in natural language function in many (though not in all)
  ways like variables in a programming language.  So evaluating
  expressions in English require a method for mapping pronouns like
  `his` to referents.

* There are a variety of expressions in natural language that produce
  side effects independent of the main "computation".  Expressives
  such as *damn*, as in *the man fed his damn mongoose*, commit the
  speaker to certain attitudes independently of the truth of the
  statement in which it occurs.
  
So the modular techniques developed for functional programming can be
applied directly to many familiar problems in the interpretation of
natural language.


