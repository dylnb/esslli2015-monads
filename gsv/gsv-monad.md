<!-- λ ◊ ≠ ∃ Λ ∀ ≡ α β γ ρ ω φ ψ Ω ○ μ η δ ζ ξ ⋆ ★ • ∙ ● ⚫ 𝟎 𝟏 𝟐 𝟘 𝟙 𝟚 𝟬 𝟭 𝟮 ⇧ (U+2e17) ¢ -->
# GSV Monadized

* There are a few ways to go here. In class we presented the following
  fragment

    * `[Pt] = λg.{(w,g) | [P](g(t))(w)}`
    * `[∃x.φ] = λg.U{[φ](g[x->v]) | v in D_e}`
    * `[◊φ] = λg.` **if** `[φ](g) ≠ ∅` **then** `{(w,g) | w in W}` **else** ∅
    * `[α ; β] = [α] >>= \w.[β] >>= \w'.` **if** `w == w'` **then** `return w` **else** `λg.∅`

* This uses the same state+set monad we introduced for plain extensional
  dynamic semantics. Binding information is transmitted in the usual fashion,
  through the input and output assignment functions, where it accumulates
  according the order in which clauses are evaluated.

* But the nondeterminism now does double duty. As before, in the presence of an
  existential, the computation branches into a set of outputs, each of which
  will push through whatever comes next independently. But it also encodes
  uncertainty about the world of evaluation. For instance, atomic sentences
  denote the update that returns every world at which the predicate holds of
  its argument *in a different branch*. Effectively, it treats clauses as
  indefinites over worlds: `[Pt] ≈ "some world in which P(t)"`.

* Conjunction encodes an order-sensitive version of set intersection. Any
  binding side effects from the first conjunct are allowed to influence the
  second, and any additional effects in the second survive to influence
  whatever comes next.


---


* However, as pointed out in class, while this formulation does capture the way
  that existentials can bind pronouns beyond their local scope, it doesn't
  quite capture the modal dynamicity at the heart of the V part of GSV.

* You can see that the entry for `◊φ` has no access to the modal state of play
  prior to its evaluation. The only thing it reads in is `g`, the current,
  local state of how discourse referents are allocated to variables. So there
  will be no way for previous information about what the set of current
  *possibilities* is like to influence the denotation of `◊φ`. In particular,
  "Alice isn't hungry; she might be hungry" will be felicitous just in case
  right now, Alice both might be hungry and isn't hungry. The second conjunct
  doesn't take into account the information contributed by the first.


---


* The problem is exactly the way that modal possibilities are conflated with
  binding possibilities. In effect we've sublimated the "information" in each
  clause into the nondynamic side effects of its denotation. But that's a
  mistake; information-accrual is certainly dynamic, and moreover probably not
  a side effect. 

* This suggests the following revision

    * `[Pt] = λpλg.{(p ∩ q, g) | q = {w | [P](g(t))(w)}}`

    * `[∃x.φ] = λpλg.U{(p ∩ q, g') | v in D_e, (q,g') in [φ](g[x->v])}`

    * `[◊φ] = λpλg.` **if** `∃(q,g') in [φ](g)(p). q ≠ ∅` **then** `{(p,g)}` **else** `∅`

    * `[α ; β] = α >=> β` (see below for `>=>`)

* Now our three types of clauses are all functions from plain propositions to
  propositions in the dynamic monad:: `st -> M (st)`, where `M` is our
  state+set contraption:: `M a = g -> {(a,g)}` (pardon the reuse of `g` as the
  *type* of assignment functions)

* The first two clauses take the incoming proposition and intersect it with
  their own (this can actually be factored out into a `map2 (∩)` operation
  applied to something more like their essential contribution). In the case of
  the existential, its own information contribution is split out into a
  different proposition for each potential value of `x`.

* The modal, however, tests its prejacent against its incoming proposition. If
  the prejacent and the input are not incompatible, it simple returns the input
  proposition (and binding state)

* Conjunction is a little interesting. All of our clauses have type `a ->
  M a`, where `a` is `st` and `M` is the state+set monad constructor. So
  they're all *Kliesli arrows* ([first class](../intro/types.md)), or
  continuations if you prefer, or the sort of thing that comes on the right
  hand side of `>>=`. Now, this is neat. There is a natural notion of
  *composition* for these effectful continuations: `k >=> h = λp. k(p) >>= h`.
  That is, given a function `a -> M b` and a function `b -> M c`, we can form
  the composition of type `a -> M c` that, when given an argument of type `a`,
  essentially runs the two computations in sequence, passing the results of the
  first into the second. This is exactly what we want conjunction to do.

* It's fitting that conjunction should be effect-respecting function
  composition, given that conjunction in GSV (and most dynamic systems) is
  (effectless) relation composition. It's also perhaps worth pointing out that
  this isn't the only dynamic fragment that uses a version of `>=>` for
  conjunction. Philippe de Groote's 2006 semantics, in a very different type
  system, actually proposes the Kliesli compositor (`>=>`) of the continuation
  monad as a way to join dynamic sentences together (though he doesn't mention
  it).
