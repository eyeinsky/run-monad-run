{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Control.Monad.Run where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except

-- wip

class Run' ma m | ma -> m where
  run' :: ma

instance Run' (r -> ReaderT r m a -> m a) (ReaderT r m) where
  run' = undefined

-- | Make the following work without needing an explicit type
-- signature on the result:

-- hot :: StateT () IO ()
hot = run' () m
  where
    m :: ReaderT () (StateT () IO) ()
    m = undefined

-- /wip

class (Monad m) => Run m where
  type Result m a
  run :: m a -> Result m a

instance Run IO where
  type Result IO a = IO a
  run io = io

instance Run Identity where
  type Result Identity a = a
  run = runIdentity

instance (Monad m) => Run (ReaderT r m) where
  type Result (ReaderT r m) a = r -> m a
  run = runReaderT

instance (Monad m, Monoid w) => Run (WriterT w m) where
  type Result (WriterT w m) a = m (a, w)
  run = runWriterT

instance (Monad m, Monoid w) => Run (RWST r w s m) where
  type Result (RWST r w s m) a = r -> s -> m (a, s, w)
  run = runRWST

instance (Monad m) => Run (ExceptT e m) where
  type Result (ExceptT e m) a = m (Either e a)
  run = runExceptT

instance (Monad m) => Run (StateT s m) where
  type Result (StateT s m) a = s -> m (a, s)
  run = runStateT

test :: IO ()
test = void $ flip run () $ run $ flip run () $ m
  where
    m :: ReaderT () (WriterT [Int] (StateT () IO)) ()
    m = liftIO $ print ()
