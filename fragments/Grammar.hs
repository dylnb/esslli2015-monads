{-# LANGUAGE FlexibleContexts #-}

module Grammar where

import Prelude hiding (log)
--import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Cont
import GrammarUtils

data Entity = John | Mary deriving (Eq, Show)
data Context = Context {speaker :: Entity, time :: Int}
type Stack = [Entity]

-- "<\>" is the categorial slash lifted into monad m
(<\>) :: Monad m => m a -> m (a -> b) -> m b
mx <\> mf = mx >>= (\x -> mf >>= (\f -> return (f x)))

(</>) :: Monad m => m (a -> b) -> m a -> m b
mf </> mx = mf >>= (\f -> mx >>= (\x -> return (f x)))



john, mary :: Monad m => m Entity
john = return John
mary = return Mary

likes :: Monad m => m (Entity -> Entity -> Bool)
likes = return (\x y -> x == John)

defCon :: Context
defCon = Context {speaker = John, time = 0}

sen1 :: Monad m => m Bool
sen1 = mary <\> (likes </> john)
-- runIdentity sen1
-- runMaybe sen1
-- runReader sen1 []

log :: (Show a, MonadWriter String m) => m a -> m a
log m = m >>= (\x -> writer (x, "Logging " ++ show x ++ "; "))

sen2 :: MonadWriter String m => m Bool
sen2 = log mary <\> (likes </> log john)
-- runWriter sen2 

someone :: MonadPlus m => m Entity
someone = john `mplus` mary

sen3 :: MonadPlus m => m Bool
sen3 = someone <\> (likes </> john)
-- runListT sen3

sen4 :: (MonadPlus m, MonadWriter String m) => m Bool
sen4 = log someone <\> (likes </> john)
-- runListT (runWriterT sen4)

me :: MonadReader Context m => m Entity
me = asks speaker

sen5 :: MonadReader Context m => m Bool
sen5 = mary <\> (likes </> me)
-- runReader sen5 defCon

pro :: MonadState Stack m => m Entity
pro = gets head

sen6 :: MonadState Stack m => m Bool
sen6 = mary <\> (likes </> pro)
-- runState sen6 [John]

push :: MonadState Stack m => m Entity -> m Entity
push m = m >>= \x -> state (\s -> (x, x:s))

sen7 :: MonadState Stack m => m Bool
sen7 = push mary <\> (likes </> pro)
-- runState sen7 [John]

sen8 :: (MonadPlus m, MonadState Stack m) => m Bool
sen8 = push someone <\> (likes </> pro)
-- runListT (runStateT sen8 [])

sen9 :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => m Bool
sen9 = push someone <\> (likes </> log pro)
-- runListT (runWriterT (runStateT sen9 []))
-- runListT (runStateT (runWriterT sen9) [])
-- runListT (runRWST sen9 defCon [])

sen10 :: (MonadPlus m, MonadRWS Context String Stack m) => m Bool
sen10 = push someone <\> (likes </> log me)
-- runListT (runRWST sen10 defCon [])

everyone :: Monad m => ContT Bool m Entity
everyone = ContT $ \k -> k John `andM` k Mary
  where andM = liftM2 (&&)

lower :: Monad m => ContT a m a -> m a
lower t = runContT t return

sen11 :: Monad m => ContT Bool m Bool
sen11 = mary <\> (likes </> everyone)
-- runIdentity (lower sen11)

sen12 :: MonadState Stack m => ContT Bool m Bool
sen12 =  mary <\> (likes </> push everyone)
-- runState (lower sen12) []

sen13 :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => ContT Bool m Bool
sen13 = log someone <\> (likes </> push everyone)
-- runListT (runWriterT (runStateT (lower sen13) []))
