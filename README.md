# Monads and Natural Language

ESSLLI Barcelona
17:00--18:30 M-F, 3-7 August 2015

Instructors: Chris Barker <chris.barker@nyu.edu> and Dylan Bumford <dbumford@gmail.com>,
*New York University, Department of Linguistics*

## Course description:

Monads are a concept from the theory of programming
languages that allows modular separation of main effect from side
effect.  For instance, if the main effect is arithmetic computation,
the side effect might be reading the value of a variable from a store
(a Reader monad), or sending a value to the print queue (a Writer
monad), or changing the value of a variable (a State monad).  Each of
these side effects can be added or removed at will without
disturbing the main computation.

It is now well-established that natural languages expressions can be
viewed as having a main ("at-issue") effect and side effects.  The
goal of this course is to explore how the technique of monadic
programming can provide insight into semantic composition in natural
language.  Examples explored in detail include intensionality as a
Reader monad (Winter and Ben-Avi, presented at SuB 11 at the Pompeu
Fabra, and at ESSLLI in 2007); donkey-anaphora as a State monad;
expressive content, as in *the damn dog*, as a Writer monad
(Giorgolo and Asudeh); scope-taking as a Continuation monad (Barker
and Shan 2006, 2014); and the Set monad for Hamblin-style indefinites
(Charlow 2014).

The course will introduce the basics of monads, including the type
constructor, the unit, the bind operator, and the monad laws by
developing progressively more complex concrete fragments of natural
language in Haskell.  The Haskell language (named for Haskell Curry)
provides extensive built-in support for monads.  The fragments will be
applied to progressively more challenging data sets in natural
language.

The level of the course will be appropriate for any student who either
has experience with a functional programming language (Haskell, OCaml,
Scheme, etc.), or who has experience with formal models of semantic
composition (Heim and Kratzer, Jacobson, Steedman, Moortgat, etc.)
The course will not presume previous familiarity with any specific
theory of meaning, and programming techniques will be explained from
first principles; nevertheless, the course will be taught at an
advanced level, and is not appropriate for students who have neither
programming experience nor experience with formal semantic models.

That said, the course is intended to be a way for experienced
programmers to leverage their understanding of functional programming
to appreciate the beauty and intricacy of natural language meaning, or
else to be a way for experienced natural language semanticists to
appreciate the value of functional programming techniques for gaining
new insights into semantic composition.

There will be simple programming exercises, as well as readings from
the linguistics literature.  Unger and van Eijck provides useful
background on Haskell.


## Tentative schedule:

* Day 1:
    * Introduction
        * Computational goals served by monads: structured computation
        * Linguistic goals served by monads: multi-dimensional meaning composition
        * Survey of applications in linguistics
    * Basics
        * Types and Kleisli arrows
        * Monad laws
    * First example, the Maybe monad: division by zero, presupposition projection
    * Haskell orientation

[put in sidebar: wiki, Hankin, Rooth, Shan, Wadler]

* Day 2:
    * Reader monad
        * Intensionality
        * Binding
        * Indexicals
    * Writer monad
        * Expressives and appositives
        * Postsuppositions
    * Re-engineering Groenendijk, Stokhof, and Veltman's 1996 fragment (part I of II)

[Winter and Ben-avi, Giorgolo and Asudeh, GSV]

* Day 3:
    * Re-engineering GSV (part II of II)
    * State monad
        * Binding
        * Donkeys
        * Ellipsis

[Bumford S&P, Charlow]

* Day 4:
    * Combinations of monads
        * Monad trasformers
        * Charlow 2015

* Day 5:
    * Continuations and scope
    * Monads and category theory
    * Comonads

[Barker and Shan, Bekki et al.]
