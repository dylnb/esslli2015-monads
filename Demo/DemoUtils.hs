{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module DemoUtils where

import Control.Monad.Writer
import Control.Monad.Cont
import Control.Applicative

type List a = [a]

runList :: List a -> [a]
runList = id

runMaybe :: Maybe a -> Maybe a
runMaybe = id

data Pair a = Pair {runPair :: (a, a)}

instance Show a => Show (Pair a) where
  show (Pair (x, y)) = "Pair " ++ show (x, y)

instance Functor Pair where
  fmap f (Pair (x, y)) = Pair (f x, f y)

instance Applicative Pair where
  pure x = Pair (x, x)
  (Pair (uf, vf)) <*> (Pair (ux, vx)) = Pair (uf ux, vf vx)

instance Monad Pair where
  return x = Pair (x, x)
  (Pair (x, y)) >>= k = Pair (fstP $ k x, sndP $ k y)
    where fstP (Pair (u, v)) = u
          sndP (Pair (u, v)) = v

instance Monoid w => Monad ((,) w) where
  return x = (mempty, x)
  (w, x) >>= k = (w <> fst (k x), snd (k x))

instance Monoid w => MonadWriter w ((,) w) where
  writer (x, w) = (w, x)
  listen (w, x) = (w, (x, w))
  pass (w, (x, f)) = (f w, x)

instance Alternative m => Alternative (ContT r m) where
  empty = ContT $ const empty
  m <|> n = ContT $ \k -> runContT m k <|> runContT n k

instance MonadPlus m => MonadPlus (ContT r m) where
  mzero = ContT $ const mzero
  m `mplus` n = ContT $ \k -> runContT m k `mplus` runContT n k

instance MonadWriter w m => MonadWriter w (ContT r m) where
  tell w = lift (tell w)
  listen = undefined
  pass = undefined

class Apply f g ret | f g -> ret where
  app :: f -> g -> ret
instance Apply (a -> b) a b where
  f `app` x = f x
instance Apply a (a -> b) b where
  x `app` f = f x
instance (Monad m, Apply s t ret) => Apply (m s) (m t) (m ret) where
  app = liftM2 app
