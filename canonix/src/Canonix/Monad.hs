{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Canonix.Monad
  (
    -- * Fmt monad
    --
    -- A monad for tree data flows, streaming output and exceptions.
    Fmt
  , runFmt
  , Progress(..)
  , Result(..)

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

  ) where

import           Control.Monad.Free

newtype Fmt inh syn o e a = Fmt { fromFmt :: Free (FormatterEffect inh syn () o e) a }
  deriving (Functor, Applicative, Monad)

runFmt :: Monoid syn => Fmt inh syn o e a -> inh -> Progress o e (Result syn () a)
runFmt (Fmt m) inh = runFormatter m inh ()

askParent :: Fmt inh syn o e inh
askParent = Fmt $ Free (AskParent pure)

asksParent :: (inh -> a) -> Fmt inh syn o e a
asksParent f = Fmt $ Free $ AskParent $ pure . f

tellChildren :: inh -> Fmt inh syn o e a -> Fmt inh syn o e a
tellChildren inh (Fmt m) = Fmt $ Free $ TellChildren inh m

censorChildren :: Monoid syn' => Fmt inh syn' o e a -> (syn' -> a -> Fmt inh syn o e b) -> Fmt inh syn o e b
censorChildren (Fmt m) f = Fmt $ Free $ CensorChildren m (\syn' a -> fromFmt $ f syn' a)

tellParent :: syn -> Fmt inh syn o e ()
tellParent syn = Fmt $ Free $ TellParent syn (pure ())

write :: o -> Fmt inh syn o e ()
write o = Fmt $ Free $ Write o (pure ())

censorWrites :: Fmt inh syn o' e a -> ([o'] -> a -> Fmt inh syn o e b) -> Fmt inh syn o e b
censorWrites (Fmt m) f = Fmt $ Free $ CensorWrites m (\o's a -> fromFmt $ f o's a)

throw :: e -> Fmt inh syn o e a
throw e = Fmt $ Free $ Throw e

-- TODO: when needs of formatter are known to be met, fuse with runFormatter,
--       which means writing it as a newtype that resembles the runFormatter
--       type, or use an equivalent monad transformer stack.
--       Eliminating the FormatterEffect indirection should improve performance.
data FormatterEffect inh syn sib o e a
  = AskParent (inh -> a)
  | TellChildren inh a

  | forall b syn'. Monoid syn' => CensorChildren (Free (FormatterEffect inh syn' sib o e) b) (syn' -> b -> a)
  | TellParent syn a

  | Modify (sib -> (sib, a))

  | Write o a
  | forall b o'. CensorWrites (Free (FormatterEffect inh syn sib o' e) b) ([o'] -> b -> a)
  | Throw e

deriving instance Functor (FormatterEffect inh syn sib o e)

data Progress o e a = Step o (Progress o e a) | Exceptional e | Done a
  deriving (Functor, Show)
data Result syn sib a = Result { resultSyn :: syn, resultSib :: sib, resultValue :: a }
  deriving (Functor, Show)

runFormatter :: Monoid syn => Free (FormatterEffect inh syn sib o e) a -> inh -> sib -> Progress o e (Result syn sib a)
runFormatter (Pure a) _inh sib = Done (Result mempty sib a)

 -- top-down
runFormatter (Free (AskParent f)) inh sib = runFormatter (f inh) inh sib
runFormatter (Free (TellChildren inh' f)) _inh sib = runFormatter f inh' sib

 -- bottom-up
runFormatter (Free (CensorChildren sub f)) inh sib =
  let
    subProgress = runFormatter sub inh sib
    go (Step a p) = Step a (go p)
    go (Exceptional e) = Exceptional e
    go (Done t) = runFormatter (f (resultSyn t) (resultValue t)) inh (resultSib t)
  in go subProgress
runFormatter (Free (TellParent syn a)) inh sib = (\r -> r { resultSyn = syn <> resultSyn r }) <$> runFormatter a inh sib

 -- left-to-right (beginning of file to end of file, sort of like preorder but
 --                allowing to 'revisit' the parent before moving on)
 --               (chaining in attribute grammars)
runFormatter (Free Modify {}) _ _ = error "not implemented"

 -- streaming output
runFormatter (Free (Write o a)) inh sib =
  let x = runFormatter a inh sib
  in Step o x
runFormatter (Free (CensorWrites sub f)) inh sib =
  let
    subProgress = runFormatter sub inh sib
    go acc (Step a p) = go (acc . (a:)) p
    go _ac (Exceptional e) = Exceptional e
    go acc (Done t) = runFormatter (Free (TellParent (resultSyn t) $ f (acc []) (resultValue t))) inh (resultSib t)
  in go id subProgress
runFormatter (Free (Throw e)) _inh _sib =
  Exceptional e
