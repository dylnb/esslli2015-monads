> {-# LANGUAGE NoImplicitPrelude #-}
>
> module Indexicals where
>
> import Prelude hiding ((>>=), return)
>
> type Context = {speaker :: Entity, addressee :: Entity, time :: Int}
> type S = { one-place :: [(String, Ent -> Bool)]
>          , two-place :: [(String, Ent -> Ent -> Bool)]
>          }
> type Prop = S -> Bool
> type Entity = John | Mary | Bill
>
> 
>
> class Monad m where
>   return :: a -> m a
>   (>>=) :: m a -> (a -> m b) -> m b
>
> instance Monad ((->) r) where
>   return = const
>   m >>= k = \w -> k (m w) w
>
>
> left :: Entity -> Prop
> left x = \w -> fromJust (lookup "left" (one-place w)) x
