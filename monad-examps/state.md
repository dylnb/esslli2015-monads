# Discourse referents as side effects

* Here's a famous fact in linguistics: expressions in one sentence can *bind*
  pronouns in completely separate clauses

  * **John** left. **He** was whistling.
  * John **left**. So **did** Mary


* One old intuition about what's happening here is that sentences manipulate a
  kind of contextual record on which we keep track of what's been talked about
  and when

* In fact, in one tradition of *dynamic semantics*, sentences are actually
  nothing more than instructions for updating these sorts of records; truth and
  falsity are derivative notions


---


* But the classic binding data doesn't by itself motivate abandoning good old
  truth-conditional content

* We just need some way of tracking updates to the discourse state, *alongside*
  compositional evaluation

* I.e., the generation of and sensitivity to discourse referents is a side
  effect of evaluating the meaning of a sentence


---


* So we want a dynamic monad; it'll need to read in some sort of environment
  (the current state of the discourse), and write out some sort of new
  environment for future expressions that might depend on it

* So we'll just glue our reader to our writer

```
State s a = s -> (a, s)

return x = \s -> (x, s)
m >>= k = \s -> k x s'
  where (x, s') = m s
```


---


* But as Hans can tell us
