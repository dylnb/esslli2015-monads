Building An Effectful Fragment, Modularly
=========================================

Setting things up
-----------------

* First we need to bring the monad instances into scope
* These instances contain the definitions of `return` and `bind` for the various notions of "effect" that we'll be considering

> {-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
> 
> import Prelude hiding (log)
> import Control.Monad.Identity
> import Control.Monad.Except
> import Control.Monad.Reader
> import Control.Monad.Writer
> import Control.Monad.List
> import Control.Monad.State
> import Control.Monad.RWS
> import Control.Monad.Cont
> import DemoUtils


* Then we need to define the types we'll need for denotations, and set up a model
  in which to interpret those types

* First up, we declare a type `E` and populate it with two individuals, `John` and
  `Mary`. Right now, these objects are just dots in the Haskell universe. The only
  thing we know about them is that `John == John` and `Mary == Mary`.

> data E = John | Mary deriving (Eq, Show)


* Next we define a type for Booleans, which, fortunately Haskell already knows about.
* Haskell also already knows about functional types, like `E -> T`. Assuming we restrict
  ourselves to the exponential closure of `E` and `T`, we've embedded the usual simple
  theory of types.
* And finally to get things going, we'll add a single relation to the model. Everybody
  likes Mary; nobody likes John.

> type T = Bool
> 
> likes' :: E -> E -> T
> likes' x _ = x == Mary


* And that's it for the model theory. Onto the language!


Lexicon
-------

* The lexicon is basically trivial: names and predicates are directly referential
  ("john" means `John`; "likes" means `likes'`, etc.)

* But with one twist: we inject these simple values into an effect-less structure

* We do this in anticipation of the fact that these pieces might eventually compose
  with effect-**ful** language, and also because later we might want to imbue these
  words with effects themselves

> john, mary :: Monad m => m E
> john = return John
> mary = return Mary
> 
> likes :: Monad m => m (E -> E -> Bool)
> likes = return likes'


* The cool thing about this is that because *every* monad defines its own well-behaved
  notion of a "pure" computation (its own `return`), we don't have to specify which
  monadic type these words denote in
  
* We can specify it later at any point, and the Haskell type-inference engine will
  resolve the ambiguous computation according to the relevant definition:

> -- print (john :: Identity E)
> -- print (john :: Pair E)
> -- print (john :: List E)


* For convenience, we can also exploit a family of "run" commands,
  which are basically identity functions with specialized types that effectively
  force the interpreter's hand in disambiguating a monad

> -- :type runIdentity
> -- :type runPair
> -- :type runList



> -- runIdentity john
> -- runPair john
> -- runList john


Grammar: Monadic Application
----------------------------

* Here we lift forward and backward function application through an arbitrary monad

* Remember, every instance of a monad will provide law-abiding definitions for the
  functions `return` and `(>>=)`

* So what's going on here is that we don't directly apply functions to their arguments,
  because one or both of those things could have side effects, which need to be dealt
  with
  
* Instead, we use the monadic operations to extract the values of the two constituents
  before applying one to the other, in the process *sequencing* their effects from left
  to right

> -- Forward function application
> (</>) :: Monad m => m (a -> b) -> m a -> m b
> mf </> mx = mf >>= \f ->
>               mx >>= \x ->
>                 return (f x)
>                 
> -- Backward function application     
> (<\>) :: Monad m => m a -> m (a -> b) -> m b
> mx <\> mf = mx >>= \x ->
>               mf >>= \f ->
>                 return (f x)


Making Sentences
----------------

* With these slashes as our "modes of composition", we build the meanings of sentences
  up in the usual type-driven way; that is, we choose the slash that makes the type fit

> sen1a, sen1b :: Monad m => m Bool
> sen1a = john <\> (likes </> mary)
> sen1b = mary <\> (likes </> john)


* Notice that Haskell is actually inferring the types of the larger constituents,
  based on the types of the parts and the mode of composition

* So at this point `sen1a` and `sen1b` are monad-neutral

* They represent computations that first extract a value from the denotation of their
  subject, then a value from that of the predicate, and then one from the object, in
  that order, accruing side effects as they go (though in this case, there are no
  effects to accrue, since everything is pure)
  
* With those values in hand, it uses the direction of the slashes (really, the
  types) to apply the pieces to one another
  
* To inspect the result, we need only specify what monad we want to see it in

> -- runIdentity sen1a
> -- runIdentity sen1b



> -- runPair sen1a
> -- runList sen1a


Adding An Effect: Context-Sensitivity
-------------------------------------

* To model context-sensitive language, we'll first need to specify some representation
  of context
  
* For simplicity, we model the context as an (entity, integer) pair representing the
  current speaker and the time of utterance, and we define a particular context for
  testing in which John is the speaker and the time is 0 (whatever that means)

> data Context = Context {speaker :: E, time :: Int}
> 
> thisContext :: Context
> thisContext = Context {speaker = John, time = 0}


* Now we define the context-sensitive lexical items

* Haskell defines a certain interface class called `MonadReader` for monads that
  pass around some sort of shared environment. That interface includes the `asks`
  function for retrieving and processing the context.

* In its simplest form --- in the bare reader monad --- `asks` is just a gussied up
  identity function
  
  $\texttt{asks} = \lambda f\lambda r.\, f\,r$

> me :: MonadReader Context m => m E
> me = asks speaker
>      -- Reader (\context -> speaker context)
> 
> now :: MonadReader Context m => m Int
> now = asks time
>      -- Reader (\context -> time context)


* Crucially, once we've encapsulated the context-sensitive denotation in a monadic type,
  we don't need to do anything else to effect successful composition

* That is, we don't need to worry about how the pieces fit together; the context
  just slides through the computation until it hits the piece of the sentence that needs
  it

> sen2 :: MonadReader Context m => m Bool
> sen2 = mary <\> (likes </> me)

> -- runReader sen2 thisContext
> -- thisContext = {speaker = John, time = 0}


Another Effect: Logging
-----------------------

* The flipside of context-sensitivity is context-generativity. Some lexical items seem to
  contribute content in a separate "dimension" of meaning, like the apposatives and 
  expressives
  
* For this, it is useful to imagine them as "writing" out their not-at-issue meanings
  into some separate denotational coordinate that doesn't get targeted by other semantic
  operations don't touch
  
* More simply, it can be useful as researchers to *trace* the thread of composition, to 
  make sure we have a handle on what's happening when
  
* This is what we'll do now, with a `log` function that simply writes out the content of
  some constituent to a string that we pass alongside semantic evaluation

> log :: (Show a, MonadWriter String m) => m a -> m a
> log m = m >>= (\x -> writer (x, "Log " ++ show x ++ ". "))


* As with the reader, there's another interface class called `MonadWriter` that
  side-content-generating monads implement. It comes with a few functions, one of which
  is `writer`, which lifts a pair containing a value and a side-note into the relevant
  monad.
  
* In its simplest form --- the bare writer monad --- it is again an identity function
  
  $\texttt{writer} = \lambda m.\, \langle m_1, m_2 \rangle$
  
  
* And that's it. We can now log nodes in the computation to inspect their values without
  mucking up any of the combinatorics

> sen3 :: MonadWriter String m => m Bool
> sen3 = log mary <\> (likes </> log john)

> -- runWriter sen3


Combining Effects: Or, Look Who's Talking
-----------------------------------------

* We *could* have implemented `me` and `log` directly in the reader and writer monads,
  respectively. But we instead took advantage of Haskell's monad classes `MonadReader`
  and `MonadWriter` using the more generic context-querying and context-writing functions
  `ask` and `writer`. Now we're ready for the payoff.
  
* There is no obstacle to doing both of these things at the same time!

> sen4 :: (MonadReader Context m, MonadWriter String m) => m Bool
> sen4 = mary <\> (likes </> log me)


* As the type signature says, we just need a monad `m` that implements *both* the
  reader and writer interfaces
  
* The simplest such monad is called `ReaderT Context (Writer String)`. In
  pseudo-Haskell, that looks like this:
  
  $\texttt{ReaderT}\,r\,(\texttt{Writer}\,w)\,a := r \to a \ast w$
  
  $
  \begin{align*}
  \texttt{return}\,x &= \lambda r.\, \langle x,\,\emptyset\rangle\\
  m \gg\!= k &= \lambda r.\, \langle y,\, w \diamond w'\rangle,\\
  &\hphantom{{}={}}\textsf{where }
  \langle x, w\rangle = m\,r\\
  &\hphantom{{}=\textsf{where }}\langle y, w'\rangle = k\,x\,r\\
  \end{align*}
  $
  
  
* To extract our results and effects from this type, we unwrap it in layers.
  (We'll try and make more sense out of this pattern later on when we get to
  monad transformers)


> -- runWriter (runReaderT sen4 thisContext)


Alternatives
------------

* To handle denotations that encode disjunction, we call on another standard
  interface class, `MonadPlus`, which subcategorizes for monads that implement
  something like a union operator
  
* The generic operator is called `mplus`, and the canonical `MonadPlus` instance
  is the list monad. It implements `mplus`, unsurprisingly, as concatenation
  
  $\texttt{mplus} = \lambda m\lambda n.\, m +\!\!\!\!+\, n$
  
  
* From there, it is easy to give a denotation for indefinite DPs in terms
  of this monadic disjunction

> someone :: MonadPlus m => m E
> someone = john `mplus` mary


* Remember that `john` here is the denotation of the name "John". It
  names the ambiguous function `return John` (in the list monad, `[John]`)

> sen5a :: MonadPlus m => m Bool
> sen5a = someone <\> (likes </> mary)

> -- runList sen5a


* As before, it is painless to combine this nondeterminism with other
  effects
  
* Here we'll write out the name of each individual that "someone" generates,
  in effect tagging each different thread of the computation.

> sen5b :: (MonadPlus m, MonadWriter String m) => m Bool
> sen5b = log someone <\> (likes </> john)


* To execute this program, we need a monad that implements both the writer
  and disjunction interface. The simplest is called `WriterT w List`:
   
  $\texttt{WriterT}\,w\,\texttt{List}\,a := \texttt{List}\,(a \ast w)$
  
  $
  \begin{align*}
  \texttt{return}\,x &= [ \langle x, \emptyset\rangle ]\\
  m \gg\!= k &=
  [%
    \langle y, w \diamond w'\rangle
  \mid
    \langle x, w\rangle \in m,\ \ \langle y, w'\rangle \in k\,x
  ]
  \end{align*}
  $
  
  
* Again, we unwrap this in layers

> -- runList (runWriterT sen5b)


Anaphora
--------

* The strategy by now is familiar; introducing and picking up discourse referents is
  a side effect of evaluation
  
* First, we'll create a space for discourse referents to live, and a help ourselves
  to a couple of possible discourse states for testing

> type Stack = List E
> 
> discourseInitial, justMentionedMary :: Stack
> discourseInitial = []
> justMentionedMary = [Mary]


* Then we define the dynamic-sensitive operations

* As before, there's an interface for state-manipulating monads called `MonadState`,
  which allocates a pair of functions for creating stateful functions that (i) retrieve
  and process the current state, and (ii) pushing out a new, or modified state
  
* In their canonical forms, in the state monad, they are:
  
  $\texttt{gets} = \lambda f\lambda s.\, \langle f\,s, s\rangle$
  
  $\texttt{modify} = \lambda f\lambda s.\, \langle (), f\,s\rangle$
  

* `pro` here represents the simplest possible anaphoric dependency; it retrieves
  the first (most recent) discourse referent from the state

> pro :: MonadState Stack m => m E
> pro = gets head
> 
> sen6a :: MonadState Stack m => m Bool
> sen6a = mary <\> (likes </> pro)


> -- runState sen6a justMentionedMary
> -- justMentionedMary = [Mary]


* Running the sentence in a discourse without any obvious shared referents
  results in presupposition failure

> -- runState sen6a discourseInitial
> -- discourseInitial = []


* Since it came up in class, here's a way to slip something like `Maybe` into something that's stateful

* `throwError` is a lot like `Nothing` except instead of encoding, well, nothing, it provides a little message

> proM :: (MonadError String m, MonadState Stack m) => m E
> proM = get >>= safeLookup
>   where safeLookup s =
>           if null s
>               then throwError "Who are we talking about here?"
>               else return (head s)



> sen6b :: (MonadError String m, MonadState Stack m) => m Bool
> sen6b = mary <\> (likes </> proM)


* A good monad to run this in is `ExceptT String (State Stack)`


  $\texttt{ExceptT}\,e\,(\texttt{State}\,s)\,a := s \to (a|e) \ast w$
  
  $
  \begin{align*}
  \texttt{return}\,x &= \lambda s.\, \langle \texttt{R}\,x, s\rangle \\
  m \gg\!= k &= \lambda s.\,
    \begin{cases}
      k\,x\,s' \quad & \textbf{if } m\,s =
                       \langle \texttt{R}\,x, s'\rangle \\
      m\,s           & \textbf{if } m\,s =
                       \langle \texttt{L}\,x, s'\rangle
    \end{cases}
  \end{align*}
  $


* In this monad, `throwError` just "returns" a trivially dynamic
  thing whose value is failure

  $\texttt{throwError}\, e =
   \texttt{return}\,(\texttt{L}\,e) =
   \lambda s.\, \langle \texttt{L}\,e, s\rangle$

> -- runState (runExceptT sen6b) justMentionedMary
> -- runState (runExceptT sen6b) discourseInitial


* Of course, we can *modify* the discourse state as well as read it in

> push :: MonadState Stack m => m E -> m E
> push m = m >>= \x -> state (\s -> (x, x:s))
>     -- = m >>= \x -> modify (x:) >>= \_ return x
> 
> sen7a :: MonadState Stack m => m Bool
> sen7a = push mary <\> (likes </> pro)

> -- runState sen7a justMentionedMary


* And as usual, we can log out any component of the sentence to an independent
  store to see what values our lexical items are taking in context

> sen7b :: (MonadState Stack m, MonadWriter String m) => m Bool
> sen7b = mary <\> (likes </> log pro)

> -- runWriter (runStateT sen7b justMentionedMary)


Dynamic Semantics: Alternatives + Anaphora
------------------------------------------

* Natural language "dynamic semantics" comes in a few different flavors, one of which
  treats sentence meanings as relations between discourse states
  $:: \texttt{List}\,(s \ast s) \quad = \quad s \to \texttt{List}\, s$
  
* If we generalize this type to cover constituents of any size, rather than just sentences,
  we get something like
  $:: s \to \texttt{List}\, (a \ast s)$
  
* This suggests that what we really need to model dynamic phenomena is a monad that
  trades in two types of effects: state-manipulation and nondeterminism. Let's try it.

> sen8a :: (MonadState Stack m, MonadPlus m) => m Bool
> sen8a = push someone <\> (likes </> pro)


* Indeed, we see that we can already apply operations from our growing fragment
  to handle indeterminate binding, a staple of dynamic systems
  
* And as we suspected, it requires a monad that implements both the stateful and
  disjunctive interfaces. Here's the prototype:
  
  $\texttt{StateT}\,s\,\texttt{List}\,a := s \to \texttt{List}\,(a \ast s)$
  
  $
  \begin{align*}
  \texttt{return}\,x &= \lambda s.\, [ \langle x, s\rangle ]\\
  m \gg\!= k &= \lambda s.\,
  [%
    \langle y, s''\rangle
  \mid
    \langle x, s'\rangle \in m\,s,\ \ \langle y, s''\rangle \in k\,x\,s'
  ]
  \end{align*}
  $

> -- runList (runStateT sen8a discourseInitial)


* Checking to make sure we're really binding that pronoun

> sen8b :: (MonadState Stack m, MonadPlus m, MonadWriter String m) => m Bool
> sen8b = push someone <\> (likes </> log pro)

> -- runList (runRWST sen8b thisContext discourseInitial)


* Adding back in the indexicals, just for the hell of it

* For the simultaneous evaluation of constituents with read-only, write-only,
  and state-manipulating side effects, we have the octopodoid `RWS` monad,
  which we can layer over nondeterministic output
  
  $\texttt{RWST}\,\,r\,w\,s\,\texttt{List}\,a :=
    r \to s \to \texttt{List}\,(a \ast s \ast w)$
  
  $
  \begin{align*}
  \texttt{return}\,x &= \lambda r\lambda s.\,
  \big[%
    \langle x, s, \emptyset\rangle
  \big]\\
  m \gg\!= k &= \lambda r\lambda s.\,
  \big[%
    \langle y, s'', w \diamond w'\rangle
  \mid
    \langle x, s', w\rangle \in m\,r\,s,\ \ \langle y, s'', w'\rangle \in k\,x\,r\,s'
  \big]
  \end{align*}
  $

> sen8c :: (MonadPlus m, MonadRWS Context String Stack m) => m Bool
> sen8c = push someone <\> (likes </> log me)



> -- runList (runRWST sen8c thisContext discourseInitial)


Quantification As Side Effect
-----------------------------

* This one is cool. It's possible to think about every lexical item as contributing
  a "local" value of some basic functional or argumental type, something that
  encodes its essential argument-structure role in the sentence
  
* However, some items --- in addition to contributing this basic *trace* --- take control
  of the computation that uses that value, most often by *quantifying* over the possible
  values that that trace may assume
  
* That control-manipulation is modeled by the continuation monad, which passes traces
  around like state, effectively bringing everything into surface order scope
  

  $\texttt{ContT}\,\,r\,\,m\,a := (a \to m\,r) \to m\,r$
  
  $
  \begin{align*}
  \texttt{return}\,x &= \lambda k.\, k\,x \\
  m \gg\!= f &= \lambda k.\, m\,(\lambda x.\, f\,x\,k)
  \end{align*}
  $

> everyone :: Monad m => ContT Bool m E
> everyone = ContT (\k -> k John `andM` k Mary)
>   where m `andM` n = m <\> (return (&&) </> n)
> 
> lower :: Monad m => ContT a m a -> m a
> lower t = runContT t return



> sen9a :: Monad m => ContT Bool m Bool
> sen9a = everyone <\> (likes </> mary)

> -- runIdentity (lower sen9a)



> sen9b :: Monad m => ContT Bool m Bool
> sen9b = mary <\> (likes </> everyone)


* Notice no need for anything like syntactic QR. The continuation's `return`
  operator "Montague lifts" the non-scopal denotations, and then the
  applicative combinators scope everything into place:
  
  $$
  \textsf{mary}^\uparrow\,(\lambda x.\,
    \textsf{likes}^\uparrow\,(\lambda f.\,
      \textsf{everyone}\,(\lambda y.\,
        f\,y\,x)))
  $$

> -- runIdentity (lower sen9b)


Putting It All Together
-----------------------

> sen10a :: MonadState Stack m => ContT Bool m Bool
> sen10a =  push everyone <\> (likes </> mary)



> -- runState (lower sen10a) discourseInitial



> sen10b :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => ContT Bool m Bool
> sen10b = log someone <\> (likes </> push everyone)



> -- runList (runWriterT (runStateT (lower sen10b) discourseInitial))



> sen10c :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => ContT Bool m Bool
> sen10c = push everyone <\> (likes </> log someone)



> -- runList (runWriterT (runStateT (lower sen10c) discourseInitial))


