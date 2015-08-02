
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

import Prelude hiding (log)
import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Writer
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

sen1 = john <\> (likes </> mary)
sen2 = mary <\> (likes </> john)

:type sen1
:type sen2

runIdentity sen1
runIdentity sen2

runPair sen1
runList sen1

data Context = Context {speaker :: E, time :: Int}

thisContext :: Context
thisContext = Context {speaker = John, time = 0}

me :: MonadReader Context m => m E
me = asks speaker
     -- Reader (\context -> speaker context)

now :: MonadReader Context m => m Int
now = asks time
     -- Reader (\context -> time context)

sen5 = mary <\> (likes </> me)

:type sen5

runReader sen5 thisContext
-- thisContext = {speaker = John, time = 0}

log :: (Show a, MonadWriter String m) => m a -> m a
log m = m >>= (\x -> writer (x, "Log " ++ show x ++ ". "))

sen2 = log mary <\> (likes </> log john)

:type sen2

runWriter sen2

senBlah = mary <\> (likes </> log me)

:type senBlah

runWriter (runReaderT senBlah thisContext)

someone :: MonadPlus m => m E
someone = john `mplus` mary

sen3 = someone <\> (likes </> mary)

:type sen3

runList sen3

sen4 = log someone <\> (likes </> john)

:type sen4

runList (runWriterT sen4)

type Stack = List E

discourseInitial :: Stack
discourseInitial = []

justMentionedJohn :: Stack
justMentionedJohn = [John]

pro :: MonadState Stack m => m E
pro = gets head

sen6 = mary <\> (likes </> pro)

runState sen6 justMentionedJohn
-- justMentionedJohn = [John]

runState sen6 discourseInitial
-- discourseInitial = []

push :: MonadState Stack m => m E -> m E
push m = m >>= \x -> state (\s -> (x, x:s))
    -- = m >>= \x -> modify (x:) >>= \_ return x

sen7 = push mary <\> (likes </> pro)

:type sen7

runState sen7 justMentionedJohn

sen65 = mary <\> (likes </> log pro)

:type sen65

runWriter (runStateT sen65 justMentionedJohn)

sen8 = push someone <\> (likes </> pro)

:type sen8

runList (runStateT sen8 discourseInitial)

sen9 = push someone <\> (likes </> log pro)

:type sen9

runList (runRWST sen9 thisContext discourseInitial)

sen10 :: (MonadPlus m, MonadRWS Context String Stack m) => m Bool
sen10 = push someone <\> (likes </> log me)

runList (runRWST sen10 thisContext discourseInitial)

everyone :: Monad m => ContT Bool m E
everyone = ContT (\k -> k John `andM` k Mary)
  where m `andM` n = m <\> (return (&&) </> n)

lower :: Monad m => ContT a m a -> m a
lower t = runContT t return

sen11 = everyone <\> (likes </> mary)

:type sen11

runIdentity (lower sen11)

sen111 = mary <\> (likes </> everyone)

:type sen111

runIdentity (lower sen111)

sen12 :: MonadState Stack m => ContT Bool m Bool
sen12 =  push everyone <\> (likes </> mary)

runState (lower sen12) discourseInitial

sen13 :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => ContT Bool m Bool
sen13 = log someone <\> (likes </> push everyone)

runList (runWriterT (runStateT (lower sen13) discourseInitial))

sen14 :: (MonadPlus m, MonadState Stack m, MonadWriter String m) => ContT Bool m Bool
sen14 = push everyone <\> (likes </> log someone)

runList (runWriterT (runStateT (lower sen14) discourseInitial))
