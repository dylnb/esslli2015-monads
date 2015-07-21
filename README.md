# Monads and Natural Language

## ESSLLI Barcelona

17:00--18:30 M-F, 3-7 August 2015

Instructors: Chris Barker <chris.barker@nyu.edu> and Dylan Bumford
<dbumford@gmail.com>


Monads are a concept from the theory of programming
languages that allows modular separation of main effect from side
effect.  For instance, if the main effect is arithmetic computation,
the side effect might be reading the value of a variable from a store
(a Reader monad), or sending a value to the print queue (a Writer
monad), or changing the value of a variable (a State monad).  Each of
these side effects can be added or subtracted at will without
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
has experience with a functional programming langauge (Haskell, OCaml,
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

Tentative schedule:

* Day 1. Monads, the monad laws, Haskell; survey of applications in linguistics [wikis]
* Day 2. Reader monad for intensionality [Winter and Ben-avi], writer monad for expressives [Giorgolo and Asudeh]
* Day 3. State monad for binding, donkey anaphora, ellipsis [Charlow 2014]
* Day 4. Set monad for indefinites; monad transformers [Charlow 2014]
* Day 5. Continuation monad for scope-taking; the mother of all monads [Barker and Shan 2014]

There will be simple programming excerises, as well as readings from
the linguistics literature.  Unger and van Eijck provides useful
background on Haskell.
