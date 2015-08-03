# Introduction

## Chris Barker (chris.barker@nyu.edu)
## Dylan Bumford (dbumford@gmail.edu)
## This course [https://github.com/dylnb/esslli2015-monads](https://github.com/dylnb/esslli2015-monads)

---

* Monads are a technique from functional programming for articulating
  computations into layers

[This discussion inspired by Wadler's "Monads for functional programming".](../Readings/wadler-monads.pdf)

* For instance, imagine you are building a calculator program that
  evaluates artithmetic expressions.  You write code that performs
  addition, subtraction, multiplication, and division, so that you can
  evaluate expressions such as `1 * ((4 / 2) + 3)`.  Once that
  works, you decide to add the following features:

---

## Graceful failure

* You decide you want to fail gracefully when there is division by
  zero.  

  `1 * ((4 / 0) + 3)`

  The obvious strategy: modify all of the code written so far
  in order to handle an error condition.  This is unsatisfying, since
  there is no reason why the code for addition ought to worry about
  division by zero.

---

## Variable binding

* You decide to add a `let` construction, e.g.,

    `let x = 2 in 1 * ((4 / x) + 3)`

  But this requires rewriting all the code again in order to deal with
  assigning values to variables.

---

## Meta computation

* You decide to count the number of arithmetic operations performed
  for billing purposes.  But this requires rewriting all of the basic
  operations yet again.

---

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

Although there is no limit to the variety of monads, several specific
monads have proven to be useful, and have names:

* Maybe monad (Option monad in O'Caml)
* Reader monad
* State monad
* Writer monad
* List monad
* Continuation monad

Here are some of the specific natural language phenomena for which
monadic treatments have been proposed:

* Intensionality (Shan 2002, Ben-avi and Winter 2009) [Reader monad]:
  adding a layer in which the values of expressions can depend on the
  choice of an evaluation parameter (usually, a possible world, or a
  world and a time) 

* Indexicality [Reader monad]

* Binding (Jacobson 1999, Shan 2002, de Groote 2007) [Reader monad,
  State monad]: adding a layer in which the values of expressions can
  both depend on previous expressions, and determine subsequent
  expressions.

* Presupposition failure [Maybe monad]
  * Kaplan

* Expressives [Writer monad]

* Scope-taking (Barker 2002, Barker and Shan 2014) [Continuation monad]

In addition to all of these considerations, another important aspect
of monads is that they allow fine-grained control over the order of
evaluation of expressions.  This has implications in the study of
natural language for theories of weak crossover, negative polarity
licensing, and more.

Charlow's 2014 dissertation, which integrates a number of monads into
a sophisticated medium-coverage grammar will be discussed in some
detail in the course.
