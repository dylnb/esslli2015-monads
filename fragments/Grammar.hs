{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}

module Grammar where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS


data Entity = John | Mary deriving (Eq, Show)
data Context = Context {speaker :: Entity, time :: Int}
type Stack = [Entity]


(<\>) :: Monad m => m a -> m (a -> b) -> m b
mx <\> mf = mx >>= (\x -> mf >>= (\f -> return $ f x))

(</>) :: Monad m => m (a -> b) -> m a -> m b
mf </> mx = mf >>= (\f -> mx >>= (\x -> return $ f x))



john, mary :: Monad m => m Entity
john = return John
mary = return Mary

likes :: Monad m => m (Entity -> Entity -> Bool)
likes = return (\x y -> x == John && y == Mary)

defCon :: Context
defCon = Context {speaker = John, time = 0}

sen1 :: Monad m => m Bool
sen1 = mary <\> (likes </> john)
-- sen1 :: Identity Bool
-- sen1 :: Maybe Bool
-- sen1 :: [Bool]
-- (sen1 :: Reader Stack Bool) []

log :: (Show a, MonadWriter String m) => m a -> m a
log m = m >>= (\x -> writer (x, "Log: " ++ show x ++ "; "))

sen2 :: MonadWriter String m => m Bool
sen2 = log mary <\> (likes </> log john)
-- sen2 :: Writer String Bool

someone :: MonadPlus m => m Entity
someone = john `mplus` mary

sen3 :: MonadPlus m => m Bool
sen3 = someone <\> (likes </> john)
-- sen3 :: [Bool]

sen4 :: (MonadPlus m, MonadWriter String m) => m Bool
sen4 = log someone <\> (likes </> john)
-- sen4 :: WriterT String [] Bool

me :: MonadReader Context m => m Entity
me = asks speaker

sen5 :: MonadReader Context m => m Bool
sen5 = mary <\> (likes </> me)
-- (sen4 :: Context -> Bool) defCon

pro :: MonadState Stack m => m Entity
pro = gets head

sen6 :: MonadState Stack m => m Bool
sen6 = mary <\> (likes </> pro)
-- runState (sen6 :: State Stack Bool) [John]

push :: MonadState Stack m => m Entity -> m Entity
push m = m >>= \x -> state (\s -> (x, x:s))

sen7 :: MonadState Stack m => m Bool
sen7 = push mary <\> (likes </> pro)
-- runState (sen7 :: State Stack Bool) [John]

sen8 :: (MonadPlus m, MonadState Stack m) => m Bool
sen8 = push someone <\> (likes </> pro)
-- runStateT (sen8 :: StateT Stack [] Bool) []

sen9 :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => m Bool
sen9 = push someone <\> (likes </> log pro)
-- runWriterT (runStateT (sen9 :: StateT Stack (WriterT String []) Bool) [])
-- runStateT (runWriterT (sen9 :: WriterT String (StateT Stack []) Bool)) []
-- runRWST (sen9 :: RWST Context String Stack [] Bool) defCon []

sen10 :: (MonadPlus m, MonadState Stack m, MonadWriter String m, MonadReader Context m) => m Bool
sen10 = push someone <\> (likes </> log me)
-- runRWST (sen10 :: RWST Context String Stack [] Bool) defCon []
