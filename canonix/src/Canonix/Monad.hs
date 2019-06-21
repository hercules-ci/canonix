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

    -- * Stream-like output
  , write

  ) where


import           Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as T
import           Control.Monad
import           Control.Monad.Free
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder        ( Builder )
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Char (ord)
import           Data.Functor.Foldable
import           Data.List ((\\))
import           Data.Semigroup.Generic
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.String
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc          ( malloc )
import           Foreign.Marshal.Array          ( peekArray
                                                , allocaArray
                                                )
import           Foreign.Marshal.Utils          ( with )
import           Foreign.Ptr                    ( nullPtr )
import           Foreign.Storable               ( peek )
import           GHC.Generics
import           System.IO
import           System.IO.Unsafe
import           TreeSitter.Language
import           TreeSitter.Nix
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Tree

newtype Fmt inh syn o a = Fmt { fromFmt :: Free (FormatterEffect inh syn () o) a }
  deriving (Functor, Applicative, Monad)

runFmt :: Monoid syn => Fmt inh syn o a -> inh -> Progress o (Result syn () a)
runFmt (Fmt m) inh = runFormatter m inh ()

askParent :: Fmt inh syn o inh
askParent = Fmt $ Free (AskParent pure)

asksParent :: (inh -> a) -> Fmt inh syn o a
asksParent f = Fmt $ Free $ AskParent $ pure . f

tellChildren :: inh -> Fmt inh syn o a -> Fmt inh syn o a
tellChildren inh (Fmt m) = Fmt $ Free $ TellChildren inh m

censorChildren :: Monoid syn' => Fmt inh syn' o a -> (syn' -> a -> Fmt inh syn o b) -> Fmt inh syn o b
censorChildren (Fmt m) f = Fmt $ Free $ CensorChildren m (\syn' a -> fromFmt $ f syn' a)

tellParent :: syn -> Fmt inh syn o ()
tellParent syn = Fmt $ Free $ TellParent syn (pure ())

write :: o -> Fmt inh syn o ()
write o = Fmt $ Free $ Write o (pure ())

-- TODO: when needs of formatter are known to be met, fuse with runFormatter,
--       which means writing it as a newtype that resembles the runFormatter
--       type, or use an equivalent monad transformer stack.
--       Eliminating the FormatterEffect indirection should improve performance.
data FormatterEffect inh syn sib o a
  = AskParent (inh -> a)
  | TellChildren inh a

  | forall b syn'. Monoid syn' => CensorChildren (Free (FormatterEffect inh syn' sib o) b) (syn' -> b -> a)
  | TellParent syn a

  | Modify (sib -> (sib, a))

  | Write o a

deriving instance Functor (FormatterEffect inh syn sib o)

data Progress o a = Step o (Progress o a) | Done a
  deriving (Functor, Show)
data Result syn sib a = Result { resultSyn :: syn, resultSib :: sib, resultValue :: a }
  deriving (Functor, Show)

runFormatter :: Monoid syn => Free (FormatterEffect inh syn sib o) a -> inh -> sib -> Progress o (Result syn sib a)
runFormatter (Pure a) _inh sib = Done (Result mempty sib a)

 -- top-down
runFormatter (Free (AskParent f)) inh sib = runFormatter (f inh) inh sib
runFormatter (Free (TellChildren inh' f)) _inh sib = runFormatter f inh' sib

 -- bottom-up
runFormatter (Free (CensorChildren sub f)) inh sib =
  let
    subProgress = runFormatter sub inh sib
    go (Step a p) = Step a (go p)
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
