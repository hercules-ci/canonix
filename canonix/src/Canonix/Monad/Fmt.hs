{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Canonix.Monad.Fmt
  (
    -- * Fmt monad
    --
    -- A monad for tree data flows, streaming output and exceptions.
    Fmt
  , runFmt

    -- * Top-down data flow
    --
    -- "context" or "inherited"
  , askParent
  , asksParent
  , tellChildren

    -- * Bottom-up data flow
    --
    -- Info about subtrees ("synthesized")
  , censorChildren
  , tellParent

    -- * Stream-like output with exceptions
  , write
  , censorWrites
  , throw
  )
where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Control.Monad.Identity
import           Pipes
import qualified Pipes.Prelude                 as PPl
import qualified Pipes.Lift                    as PL

-- | A monad facilitating data flows for tree computations that also produce streams.
newtype Fmt inh syn o e a = Fmt
  { fromFmt :: Producer o (ReaderT inh (StateT syn (ExceptT e Identity))) a
  } deriving (Functor, Applicative, Monad)

runFmt
  :: Monoid syn
  => Fmt inh syn o e a
  -> inh
  -> Producer o Identity (Either e (syn, a))
runFmt (Fmt m) inh =
  fmap (fmap swap) $ PL.runExceptP $ PL.runStateP mempty $ PL.runReaderP inh m
 where
  swap :: (a, b) -> (b, a)
  swap (a, b) = (b, a)

askParent :: Fmt inh syn o e inh
askParent = Fmt $ ask

asksParent :: (inh -> a) -> Fmt inh syn o e a
asksParent f = Fmt $ asks f

tellChildren :: inh -> Fmt inh syn o e a -> Fmt inh syn o e a
tellChildren inh (Fmt m) = Fmt $ local (const inh) m

censorChildren
  :: Monoid syn'
  => Fmt inh syn' o e a
  -> (syn' -> a -> Fmt inh syn o e b)
  -> Fmt inh syn o e b
censorChildren (Fmt m) f = Fmt $ do
  runR      <- asks (flip runReaderT)
  (a, syn') <-
    hoist (lift . lift)
    $ flip runStateT mempty
    $ PL.distribute
    $ runR
    $ PL.distribute m
  fromFmt $ f syn' a

tellParent :: Semigroup syn => syn -> Fmt inh syn o e ()
tellParent syn = Fmt $ modify (<> syn)

write :: o -> Fmt inh syn o e ()
write o = Fmt $ yield o

censorWrites
  :: Fmt inh syn o' e a -> ([o'] -> a -> Fmt inh syn o e b) -> Fmt inh syn o e b
censorWrites (Fmt m) f = Fmt $ do
  (l, a) <- lift $ PPl.toListM' m
  fromFmt $ f l a

  -- Free $ CensorWrites m (\o's a -> fromFmt $ f o's a)

throw :: e -> Fmt inh syn o e a
throw e = Fmt $ throwError e
