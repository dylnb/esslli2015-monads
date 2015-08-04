# Day 2: Examples of monads

## Last time:

  Every time you want to add a new side effect to your computation,
  you could modify each operations in order to handle the new side
  effect...

  ... or you could use a monad.

## First monad: Maybe monad

  If `a` is a type, then `Maybe a` is the type `Just a | Nothing`

  So if `a` is `Int`, then `Nothing` has type `Maybe Int`.

  So if `a` is `Bool`, then `Just True` has type `Maybe Bool`.

  So `Maybe` is a ***type function*** (pronounced in Haskell as "type constructor"): for any type `a`, `Maybe a` returns a
  type (based on `a`).

    return :: a -> Maybe a
    return x = Just x

    bind :: (Maybe a) -> (a -> Maybe b) -> (Maybe b)
    bind m k = if m == Nothing, Nothing
               if m == Just x, k x

We'll write `bind m k` as `m >>= k`

    
## Who says it's a monad?

    Law 1: return x >>= k == k x
    Law 2: m >>= return == m

     case 1: m is Nothing, then m >>= return == Nothing
     case 2: m is Just x, then m >>= return == return x == Just x

    Law 3: (m >>= f) >>= g == m >>= (\x. f x >>= g)

     case 1: m or f or g is Nothing: both sides evalute to Nothing
     case 2: m is Just x: (m >>= f) >>= g == f x >>= g == g x
                          m >>= (\x. f x >>= g) == f x >>= g == g x

Yes, it's a monad!

[draw tree with plumbing]        

---

Gilad Ben-Avi and Yoad Winter. 2007.
The Semantics of Intensionalization.
Workshop on New Directions in Type-theoretic
Grammars. Muskens (ed). ESSLLI (Dublin)

  In this paper, expressions like the verbs *seek*, *need* and
  *believe* and the adjective *fake*, which create an intensional
  context are called *intension-sensitive*. Expressions that do not
  create an intensional context, such as the verb kiss or the
  adjective red, are called *intension-insensitive*. We assume that an
  extensional semantics is sufficiently adequate for expressions that
  consist solely of intension-insensitive lexical items, while an
  intensional semantics is only needed for expressions with
  intension-sensitive lexical items.

  In this paper we propose a modular approach to the architecture of
  intensional systems that is based on this assumption. We start out
  by introducing a simple grammatical framework with a standard
  extensional semantics, and then add intension-sensitive words to the
  lexicon. Since the intension-sensitive lexical items (semantically)
  select intensional objects, the extensional types and meanings of
  the intension-insensitive lexical items need to be shifted to inten-
  sional types and meanings.

---

## Toy fragment

```haskell
Reader a ==> s -> a
return x = \w.x
m >>= k = \w.k(m w)w

-- Types: World, Ent, and Bool

w1, w2 :: World
Ann, Bill, Carl :: Ent

president :: World -> Ent
president w1 = Obama
president w2 = Bush

hit :: Ent -> Ent -> Bool
(map2 hit) (return Ann) (return Obama) w1 == True
(map2 hit) (return Ann) president w1 == True
(map2 hit) (return Ann) president w2 == False

seek :: Ent -> (World -> Ent) -> Bool
seek Ann president == False
seek Ann (return Obama) == True
```
