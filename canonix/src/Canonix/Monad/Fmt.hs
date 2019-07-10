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

    -- * Start-to-end data flow
  , askPreceding
  , tellSucceeding
  , precedingState

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
import           Data.Function
import           Pipes
import qualified Pipes.Prelude                 as PPl
import qualified Pipes.Lift                    as PL

-- | A monad facilitating data flows for tree computations that also produce streams.
newtype Fmt inh syn pre o e a = Fmt
  { fromFmt :: Producer o (ReaderT inh (StateT (syn, pre) (ExceptT e Identity))) a
  } deriving (Functor, Applicative, Monad)

runFmt
  :: Monoid syn
  => Fmt inh syn pre o e a
  -> inh
  -> pre
  -> Producer o Identity (Either e (syn, pre, a))
runFmt (Fmt m) inh pre0 =
  fmap (fmap swoosh)
    $ PL.runExceptP
    $ PL.runStateP (mempty, pre0)
    $ PL.runReaderP inh m
 where
  swoosh (c, (a, b)) = (a, b, c)

askParent :: Fmt inh syn pre o e inh
askParent = Fmt $ ask

asksParent :: (inh -> a) -> Fmt inh syn pre o e a
asksParent f = Fmt $ asks f

tellChildren :: inh -> Fmt inh syn pre o e a -> Fmt inh syn pre o e a
tellChildren inh (Fmt m) = Fmt $ local (const inh) m

censorChildren
  :: Monoid syn'
  => Fmt inh syn' pre o e a
  -> (syn' -> a -> Fmt inh syn pre o e b)
  -> Fmt inh syn pre o e b
censorChildren (Fmt m) f = Fmt $ do
  runR             <- asks (flip runReaderT)
  pre0             <- gets snd
  (a, (syn', pre)) <-
    hoist (lift . lift)
    $ flip runStateT (mempty, pre0)
    $ PL.distribute
    $ runR
    $ PL.distribute m
  fromFmt $ tellSucceeding (const pre)
  fromFmt $ f syn' a

tellParent :: Semigroup syn => syn -> Fmt inh syn pre o e ()
tellParent syn = Fmt $ modify (\(syn0, pre) -> (syn0 <> syn, pre))

write :: o -> Fmt inh syn pre o e ()
write o = Fmt $ yield o

censorWrites
  :: Fmt inh syn pre o' e a
  -> ([o'] -> a -> Fmt inh syn pre o e b)
  -> Fmt inh syn pre o e b
censorWrites (Fmt m) f = Fmt $ do
  (l, a) <- lift $ PPl.toListM' m
  fromFmt $ f l a

throw :: e -> Fmt inh syn pre o e a
throw e = Fmt $ throwError e

askPreceding :: Fmt inh syn pre o e pre
askPreceding = precedingState $ \pre -> (pre, pre)

tellSucceeding :: (pre -> pre) -> Fmt inh syn pre o e ()
tellSucceeding f = precedingState $ \pre -> ((), f pre)

precedingState :: (pre -> (a, pre)) -> Fmt inh syn pre o e a
precedingState f =
  Fmt $ state $ \(syn, pre) -> f pre & \(a, pre') -> (a, (syn, pre'))
