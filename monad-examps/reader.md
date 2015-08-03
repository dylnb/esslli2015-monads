# Reader

$$\text{Reader}\,r\,a = r \to a$$

$$\eta\,x = \lambda r.\, x$$

$$m \star k = \lambda r \to k\,(m\,r)\,r$$


---


```haskell
type Context = {speaker :: Entity, time :: Int}

type CR a = Reader Context a
     -- = Context -> a
```


---


```haskell
left' :: Entity -> Bool
left' = \x -> x == John

john :: CR Entity
john = return John
  -- = \r -> John

left :: CR (Entity -> Bool)
left = return left'
  -- = \r -> left'

john >>= \x -> left >>= \f -> return (left' x)
<~~~>
left >>= \f -> return (left' john)
<~~~>
return (left' john)
-- = \r -> left' john
```


---



