# Context-sensitivity as side effect

* Appositives and expressives are examples of expressions that *generate*
  additional data alongside the computation of their primary, at-issue content

* But there are also plenty of examples of the obverse phenomenon: expressions
  that *consume* additional data as they compute their at-issue content

* You might think of this as the denotation changing as a side-effect of begin
  evaluated in a different environment


---


* Examples of context sharing in linguistics are easy to come by

    * indexicality
    * intensionality
    * assignment-sensitivity (anaphora, binding)
    * modal bases
    * negotiated vagueness thresholds
    * the meanings of words 

* Basically anything that you've ever seen grafted onto a pair of denotation
  brackets as a "parameter of evaluation"


---


```haskell
Reader r a = r -> a

return x = \r -> x
m >>= k = \r -> k (m r) r
```

---


* This looks pretty familiar!

> If \alpha is a terminal node occupied by a lexical item, then
> for any assignment g, [[\alpha]]^g = [[\alpha]]
> 
> If \alpha is a branching node and \{\beta, \gamma\} the set of its
> daughters, then, for any assignment g, [...]
> [[\alpha]]^g = [[\beta]]^g([[\gamma]]^g)

-- Heim and Kratzer, *Semantics in Generative Grammar*


* In other words, for lexical nodes \alpha, [[\alpha]] =  `return`
  [[\alpha]], and for nonterminal nodes \alpha, [[\alpha]] =
  [[\beta]]`>>=` \lambda y\lambda g.[[\gamma]] g y
