<!-- λ ◊ ≠ ∃ Λ ∀ ≡ α β γ ρ ω φ ψ Ω ○ μ η δ ζ ξ ⋆ ★ • ∙ ● ⚫ 𝟎 𝟏 𝟐 𝟘 𝟙 𝟚 𝟬 𝟭 𝟮 ⇧ (U+2e17) ¢ -->
# Groenendijk, Stokhof, and Veltman 1996

## Doing things with monads (an extended application): Groenendijk, Stokhof and Veltman's *Coreference and Modality*

Here is the paper: [GSV](../Readings/GSV-coreference-and-modality.pdf)

GSV are interested in developing and establishing a reasonable theory
of discourse update.  One way of looking at this paper is like this:

> GSV = GS + V, where
        
> GS = Dynamic theories of binding of Groenendijk and Stokhof, e.g.,
> Dynamic Predicate Logic L&P 1991: dynamic binding, donkey anaphora
> Dynamic Montague Grammar 1990: generalized quantifiers and
> discourse referents

> V = a dynamic theory of epistemic modality, e.g., 
> Veltman, Frank. "Data semantics." 
> In Truth, Interpretation and Information, Foris, Dordrecht
> (1984): 43-63, or
> Veltman, Frank. "Defaults in update semantics." Journal of
> philosophical logic 25.3 (1996): 221-261. 

That is, Groenendijk and Stokhof have a well-known theory of dynamic
semantics, and Veltman has a well-known theory of epistemic modality,
and this fragment brings both of those strands together into a single
system.  The key result, as we'll discuss, is that adding modality to
dynamic semantics creates some unexpected and fascinating
interactions.

## Empirical target

Epistemic modality:

* Alice might be hungry.  Alice isn't hungry.
* #Alice isn't hungry.  (So) Alice might be hungry.

Pronoun binding:

* A man entered.  He sat.
* He sat. A man entered.

Interaction of modality with binding: [the broken vase scenario...]


## Basics of GSV's fragment

The fragment in this paper is unusually elegant.  We'll present it on
its own terms, with the exception that we will not use GSV's "pegs".
See the discussion below below concerning pegs for an explanation.
After presenting the paper, we'll re-engineer the fragment using
explicit monads.  So think about where you would put monadic layers.

* an evaluation point, "a possibility": (world, assignment function)

* "information state": set of possibilities

* Combines info about the facts, info about the discourse

* Predicate Calculus with equality, existential and universal
  quantification, and one unary modality 

* Terms: Alice, Bob, Carl, or a variable.

* ref ((w,g), t) = t if t is Alice or Bob or Carl, and g(t) if t is a var

GSV's definitions of update:

* s[P(t)] = {(w,g) in s | extension w P (ref((w,g),t))}

So `man(x)` is the set of live possibilities `(w,g)` in s such that
the set of men in `w` given by `extension w "man"` maps the object
referred to by `x`, namely, `g("x")`, to `true`.  That is, update with
"man(x)" discards all possibilities in which "x" fails to refer to a
man.

* s[t1 = t2] = {i in s | ref(i,t1) == ref(i,t2)}

* s[φ and ψ] = s[φ][ψ]

*  s[neg φ] =  {i | {i}[φ] = {}}

Existential quantification is somewhat intricate.

* s[∃xφ] = Union {{(w, g[x->a]) | (w,g) in s}[φ] | a in ent} 

Here's the recipe: 

* given a starting infostate s, choose an object a
from the domain of discourse.  

* Construct a modified infostate s' by
adjusting the assignment function of each possibility so as to map the
variable x to a.  

* Then update s' with φ.  

* Finally, take the union over
the results of doing this for every object a in the domain of
discourse.  

And here is the definition of truth:

* Truth: if updating φ with the information state that contains only
  the possibility i returns the empty information state, then not φ is
  true with respect to i.

Disjunction, the conditional, and the universals are defined
in terms of negation and the other connectives

Exercise: assume that there are three entities in the domain of
discourse, Alice, Bob, and Carl.  Assume that Alice is a woman, and
Bob and Carl are men.

Compute the following:

    1. {(w,g)}[∃x.man(x)]

       = {(w,g[x->a])}[man(x)] ++ {(w,g[x->b])}[man(x)] 
                               ++ {(w,g[x->c])}[man(x)] 
       = {} ++ {(w,g[x->b])} ++ {(w,g[x->c])}
       = {(w,g[x->a]),(w,g[x->b]),(w,g[x->c])}
       -- Bob and Carl are men

## Order and modality

The final remaining update rule concerns modality:

* s[◊φ] = {i in s | s[φ] ≠ {}}

    1. Alice isn't hungry.  #Alice might be hungry.

We'll start with an infostate containing two possibilities.  In one
possibility, Alice is hungry (call this possibility "hungry"); in the
other, she is not (call it "full").

      {hungry, full}[Alice isn't hungry][Alice might be hungry]
    = {full}[Alice might be hungry]
    = {}

As usual in dynamic theories, a sequence of sentences is treated as if
the sentence were conjoined.  

      {full}[Alice is hungry]
    = {}

    2. Alice might be hungry.  Alice isn't hungry.

We'll start with the same two possibilities.

    = {hungry, full}[Alice might be hungry][Alice isn't hungry]
    = {hungry, full}[Alice isn't hungry]
    = {full}

GSV comment that a single speaker couldn't possibly be in a position
to utter the discourse in (2).

    3. (Based on public evidence,) Alice might be hungry.  
       (But in fact I have private knowledge that) she's not hungry.

The main point to appreciate here is that the update behavior of the
discourses depends on the order in which the sentences are processed.

You might think that asserting *might* requires that the prejacent be
not merely possible, but undecided.

## Order and binding

    6. A man^x entered.  He_x sat.
    7. He_x sat.  A man^x entered.

In order to demonstrate how the fragment treats these discourses, we'll
need an information state whose refsys is defined for at least one
variable.

    8. {(w,g[x->b])}

This infostate contains a refsys and an assignment that maps the
variable x to Bob.  Here are the facts in world w:

    extension w "enter" a = false
    extension w "enter" b = true
    extension w "enter" c = true

    extension w "sit" a = true
    extension w "sit" b = true
    extension w "sit" c = false

We can now consider the discourses in (6) and (7) (after magically
converting them to the Predicate Calculus):

    9. Someone^x entered.  He_x sat.  

         {(w,g[x->b])}[∃x.enter(x)][sit(x)]

       = (   {(w,g[x->b][x->a])}[enter(x)]
          ++ {(w,g[x->b][x->b])}[enter(x)]
          ++ {(w,g[x->b][x->c])}[enter(x)])[sit(x)]

          -- "enter(x)" filters out the possibility in which x refers
          -- to Alice, since Alice didn't enter

       = (   {}
          ++ {(w,g[x->b][x->b])}
          ++ {(w,g[x->b][x->c])})[sit(x)]

          -- "sit(x)" filters out the possibility in which x refers
          -- to Carl, since Carl didn't sit

       =  {(w,g[x->b][x->b])}

One of the key facts here is that even though the existential has
scope only over the first sentence, in effect it binds the pronoun in
the following clause.  This is characteristic of dynamic theories in
the style of Groenendijk and Stokhof, including DPL and DMG. 

The outcome is different if the order of the sentences is reversed.

    10. He_x sat.  Someone^x entered. 

         {(w,g[x->b])}[sit(x)][∃x.enter(x)]

         -- evaluating `sit(x)` rules out nothing, since (coincidentally)
         -- x refers to Bob, and Bob is a sitter

       = {(w,g[x->b])}[∃x.enter(x)]

         -- Just as before, the existential adds a new peg and assigns
         -- it to each object

       =    {(w,g[x->b][x->a])}[enter(x)]
         ++ {(w,g[x->b][x->b])}[enter(x)]
         ++ {(w,g[x->b][x->c])}[enter(x)]

         -- enter(x) eliminates all those possibilities in which x did
         -- not enter

       = {} ++ {(w,g[x->b][x->b])}
            ++ {(w,g[x->b][x->c])}

       = {(w,g[x->b][x->b]), (w,g[x->b][x->c])}

Before, there was only one possibility: that x refered to the only
person who both entered and sat.  Here, there remain two
possibilities: that x refers to Bob, or that x refers to Carl.  This
makes predictions about the interpretation of continuations of the
dialogs:

    11. A man^x entered.  He_x sat.  He_x spoke.
    12. He_x sat.  A man^x entered.  He_x spoke.

The construal of (11) as marked entails that the person who spoke also
entered and sat.  The construal of (12) guarantees only that the
person who spoke also entered.  There is no guarantee that the person
who spoke sat.  

    13. If a woman entered, she sat.

See the paper for details.

## Interactions of binding with modality

* (∃x.enter(x)) and (sit(x)) ≡ ∃x (enter(x) and sit(x))

In words, existentials can bind pronouns in subsequent clauses even if
they don't take syntactic scope over those clauses.

The presence of modal possibility, however, disrupts this
generalization.  GSV illustrate this with the following story.

    The Broken Vase:
    There are three children: Alice, Bob, and Carl.
    One of them broke a vase.  
    Alice is known to be innocent.  
    Someone is hiding in the closet.

* (∃x.closet(x)) and (◊guilty(x)) ≡/≡ ∃x (closet(x) and ◊guilty(x))

To see this, we'll start with the left hand side.  We'll need at least
two worlds.

        in closet        guilty 
        ---------------  ---------------
    w:  a  true          a  false
        b  false         b  true
        c  false         c  false

    w': a  false         a  false
        b  false         b  false
        c  true          c  true

GSV say that (∃x.closet(x)) and (◊guilty(x)) is true if there is at
least one possibility in which a person in the closet is guilty.  In
this scenario, world w' is the verifying world: Carl is in the closet,
and he's guilty.  It remains possible that there are closet hiders who
are not guilty in any world.  Alice fits this bill: she's in the
closet in world w, but she is not guilty in any world.

Let's see how this works out in detail.

    14. Someone^x is in the closet.  They_x might be guilty.

         {(w,g), (w',g}[∃x.closet(x)][◊guilty(x)]

         -- existential introduces new peg

       = (   {(w,g[x->a]), (w',g[x->a])}[closet(x)]
          ++ {(w,g[x->b]), (w',g[x->b])}[closet(x)]
          ++ {(w,g[x->c]), (w',g[x->c])}[closet(x)]
                                                    )[◊guilty(x)]

         -- only possibilities in which x is in the closet survive
         -- the first update

       = {(w,g[x->a]), (w',g[x->c])}[◊guilty(x)]

         -- Is there any possibility in which x is guilty?
         -- yes: for x = Carl, in world w' Carl broke the vase
         -- that's enough for the possiblity modal to allow the entire
         -- infostate to pass through unmodified.

       = {(w,g[x->a]),(w',g[x->c])}

Now we consider the second half:

    15. Someone^x is in the closet who_x might be guilty.

         {(w,g), (w',g)}[∃x(closet(x) & ◊guilty(x))]
       
       =    {(w,g[x->a]), (w',g[x->a])}[closet(x)][◊guilty(x)]
         ++ {(w,g[x->b]), (w',g[x->b])}[closet(x)][◊guilty(x)]
         ++ {(w,g[x->c]), (w',g[x->c])}[closet(x)][◊guilty(x)]

          -- filter out possibilities in which x is not in the closet
          -- and filter out possibilities in which x is not guilty
          -- the only person who was guilty in the closet was Carl in
          -- world w'

       = {(w',g[x->c])}

The result is different.  Fewer possibilities remain.  We have
eliminated one of the possible worlds (w is ruled out), and we have
eliminated one of the possible discourses (x cannot refer to Alice).
So the second formula is more informative.

One of main conclusions of GSV is that in the presence of modality,
the hallmark of dynamic treatments--that existentials bind outside of 
their syntactic scope--needs to refined into a more nuanced understanding.
Binding still occurs, but the extent of the syntactic scope of an existential
has a detectable effect on truth conditions.
