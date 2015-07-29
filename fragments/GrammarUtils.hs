{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module GrammarUtils where

import Control.Monad.Writer
import Control.Monad.Cont
import Control.Applicative

runList :: [a] -> [a]
runList = id

runMaybe :: Maybe a -> Maybe a
runMaybe = id

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
