
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

import Prelude hiding (log)
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.List
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Cont
:load DemoUtils

data E = John | Mary deriving (Eq, Show)

type T = Bool

likes' :: E -> E -> T
likes' x _ = x == Mary

john, mary :: Monad m => m E
john = return John
mary = return Mary

likes :: Monad m => m (E -> E -> Bool)
likes = return likes'

print (john :: Identity E)
print (john :: Pair E)
print (john :: List E)

:type runIdentity
:type runPair
:type runList

runIdentity john
runPair john
runList john

-- Forward function application
(</>) :: Monad m => m (a -> b) -> m a -> m b
mf </> mx = mf >>= \f ->
              mx >>= \x ->
                return (f x)
                
-- Backward function application     
(<\>) :: Monad m => m a -> m (a -> b) -> m b
mx <\> mf = mx >>= \x ->
              mf >>= \f ->
                return (f x)

sen1a = john <\> (likes </> mary)
sen1b = mary <\> (likes </> john)

:type sen1a
:type sen1b

runIdentity sen1a
runIdentity sen1b

runPair sen1a
runList sen1a

data Context = Context {speaker :: E, time :: Int}

thisContext :: Context
thisContext = Context {speaker = John, time = 0}

me :: MonadReader Context m => m E
me = asks speaker
     -- Reader (\context -> speaker context)

now :: MonadReader Context m => m Int
now = asks time
     -- Reader (\context -> time context)

sen2 = mary <\> (likes </> me)

:type sen2

runReader sen2 thisContext
-- thisContext = {speaker = John, time = 0}

log :: (Show a, MonadWriter String m) => m a -> m a
log m = m >>= (\x -> writer (x, "Log " ++ show x ++ ". "))

sen3 = log mary <\> (likes </> log john)

:type sen3

runWriter sen3

sen4 = mary <\> (likes </> log me)

:type sen4

runWriter (runReaderT sen4 thisContext)

someone :: MonadPlus m => m E
someone = john `mplus` mary

sen5a = someone <\> (likes </> mary)

:type sen5a

runList sen5a

sen5b = log someone <\> (likes </> john)

:type sen5b

runList (runWriterT sen5b)

type Stack = List E

discourseInitial, justMentionedMary :: Stack
discourseInitial = []
justMentionedMary = [Mary]

pro :: MonadState Stack m => m E
pro = gets head

sen6a = mary <\> (likes </> pro)

runState sen6a justMentionedMary
-- justMentionedMary = [Mary]

runState sen6a discourseInitial
-- discourseInitial = []

proM :: (MonadError String m, MonadState Stack m) => m E
proM = get >>= safeLookup
  where safeLookup s =
          if null s
              then throwError "Who are we talking about here?"
              else return (head s)

sen6b = mary <\> (likes </> proM)

:type sen6b

runState (runExceptT sen6b) justMentionedMary
runState (runExceptT sen6b) discourseInitial

push :: MonadState Stack m => m E -> m E
push m = m >>= \x -> state (\s -> (x, x:s))
    -- = m >>= \x -> modify (x:) >>= \_ return x

sen7a = push mary <\> (likes </> pro)

:type sen7a

runState sen7a justMentionedMary

sen7b = mary <\> (likes </> log pro)

:type sen7b

runWriter (runStateT sen7b justMentionedMary)

sen8a = push someone <\> (likes </> pro)

:type sen8a

runList (runStateT sen8a discourseInitial)

sen8b = push someone <\> (likes </> log pro)

:type sen8b

runList (runRWST sen8b thisContext discourseInitial)

sen8c :: (MonadPlus m, MonadRWS Context String Stack m) => m Bool
sen8c = push someone <\> (likes </> log me)

runList (runRWST sen8c thisContext discourseInitial)

everyone :: Monad m => ContT Bool m E
everyone = ContT (\k -> k John `andM` k Mary)
  where m `andM` n = m <\> (return (&&) </> n)

lower :: Monad m => ContT a m a -> m a
lower t = runContT t return

sen9a = everyone <\> (likes </> mary)

:type sen9a

runIdentity (lower sen9a)

sen9b = mary <\> (likes </> everyone)

:type sen9b

runIdentity (lower sen9b)

sen10a :: MonadState Stack m => ContT Bool m Bool
sen10a =  push everyone <\> (likes </> mary)

runState (lower sen10a) discourseInitial

sen10b :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => ContT Bool m Bool
sen10b = log someone <\> (likes </> push everyone)

runList (runWriterT (runStateT (lower sen10b) discourseInitial))

sen10c :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => ContT Bool m Bool
sen10c = push everyone <\> (likes </> log someone)

runList (runWriterT (runStateT (lower sen10c) discourseInitial))
