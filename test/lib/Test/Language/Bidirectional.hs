-- Copyright (c) 2017 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
             UndecidableInstances #-}

module Test.Language.Bidirectional(
       Type(..),
       Intro(..),
       Elim(..),
       Literal(..),
       Typed(..),
       FieldName,
       ParamName,

       HSubst(..)
       ) where

import Control.Monad
import Data.Array(Ix(..), Array)
import Data.Traversable
import Subst
import Subst.Bound
import Subst.Free
import Subst.Scope

newtype FieldName = FieldName { fieldName :: Word }
  deriving (Eq, Ord, Ix)

newtype ParamName = ParamName { paramName :: Word }
  deriving (Eq, Ord, Ix)

instance Enum FieldName where
  toEnum = FieldName . toEnum
  fromEnum = fromEnum . fieldName

instance Enum ParamName where
  toEnum = ParamName . toEnum
  fromEnum = fromEnum . paramName

data Type =
    Product {
      productElems :: Array FieldName Type
    }
  | Arrow {
      arrowArgs :: Array ParamName Type,
      arrowRet :: Type
    }
  | IntType
  | StrType

data Intro atomty =
    Tuple {
      tupleElems :: Array FieldName (Intro atomty)
    }
  | Lambda {
      lambdaBody :: Scope Intro atomty ParamName
    }
  | Elim { elim :: Elim atomty }

data Elim atomty =
    Atom { atom :: !atomty }
  | Project {
      projectTerm :: Elim atomty,
      projectField :: !Word
    }
  | Call {
      callTerm :: Elim atomty,
      callArgs :: Array ParamName (Intro atomty)
    }
  | Intro { intro :: Typed atomty }

data Typed atomty =
  Typed {
    typedTerm :: Intro atomty,
    typedType :: Type
  }

data Literal =
    Number { number :: Integer }
  | String { string :: String }

instance Functor Typed where
  fmap = fmapDefault

instance Foldable Typed where
  foldMap = foldMapDefault

instance Traversable Typed where
  traverse f t @ Typed { typedTerm = term } =
    (\term' -> t { typedTerm = term' }) <$> traverse f term

instance Retract Literal (Typed Literal) where
  retract = Just . embed

instance Embed Literal (Typed Literal) where
  embed n @ Number {} = Typed { typedTerm = Elim { elim = Atom { atom = n } },
                                typedType = IntType }
  embed s @ String {} = Typed { typedTerm = Elim { elim = Atom { atom = s } },
                                typedType = StrType }

instance Functor Elim where
  fmap = fmapDefault

instance Foldable Elim where
  foldMap = foldMapDefault

instance Traversable Elim where
  traverse f a @ Atom { atom = val } =
    (\newval -> a { atom = newval }) <$> f val
  traverse f p @ Project { projectTerm = term } =
    (\term' -> p { projectTerm = term' }) <$> traverse f term
  traverse f c @ Call { callTerm = term, callArgs = args } =
    (\term' args' -> c { callTerm = term', callArgs = args' }) <$>
      traverse f term <*> traverse (traverse f) args
  traverse f i @ Intro { intro = term } =
    (\term' -> i { intro = term' }) <$> traverse f term

instance Applicative Elim where
  pure = inject
  (<*>) = ap

instance Inject Elim where
  inject = Atom

instance Monad Elim where
  return = inject

  Atom { atom = val } >>= f = f val
  p @ Project { projectTerm = term } >>= f = p { projectTerm = term >>= f }
  c @ Call { callTerm = term, callArgs = args } >>= f =
    c { callTerm = term >>= f, callArgs = fmap (>>>= f) args }
  i @ Intro { intro = t @ Typed { typedTerm = term } } >>= f =
    i { intro = t { typedTerm = term >>>= f } }

instance (Embed valty resty, Embed (Elim valty) resty) =>
         Subst valty varty resty Elim where
  (>>>=) = substFunctor

instance Functor Intro where
  fmap = fmapDefault

instance Foldable Intro where
  foldMap = foldMapDefault

instance Traversable Intro where
  traverse f t @ Tuple { tupleElems = elemvals } =
    (\elems' -> t { tupleElems = elems' }) <$> traverse (traverse f) elemvals
  traverse f e @ Elim { elim = term } =
    (\term' -> e { elim = term' }) <$> traverse f term

instance Inject Intro where
  inject = Elim . inject

instance Applicative Intro where
  pure = inject

  Elim { elim = f } <*> e @ Elim { elim = a } = e { elim = f <*> a }
  f @ Elim {} <*> t @ Tuple { tupleElems = elemvals } =
    t { tupleElems = fmap (f <*>) elemvals }
  t @ Tuple { tupleElems = elemfuncs } <*> a =
    t { tupleElems = fmap (<*> a) elemfuncs }

instance (Embed valty resty, Embed (Intro valty) resty) =>
         Subst valty varty resty Intro where
  term >>>= f = embed (fmap f term)

instance Embed (Elim (Elim a)) (Elim a) where
  embed = join

instance Retract (Elim (Typed a)) (Elim a) where
  retract = Just . embed

instance Embed (Elim (Typed a)) (Elim a) where
  embed Atom { atom = typed } = Intro { intro = typed }
  embed p @ Project { projectTerm = term } = p { projectTerm = embed term }
  embed c @ Call { callTerm = term, callArgs = args } =
    c { callTerm = embed term, callArgs = fmap embed args }
  embed i @ Intro { intro = t } = i { intro = embed t }

instance Retract (Intro (Typed a)) (Intro a) where
  retract = Just . embed

instance Embed (Intro (Typed a)) (Intro a) where
  embed t @ Tuple { tupleElems = elems } = t { tupleElems = fmap embed elems }
  embed e @ Elim { elim = term } = e { elim = embed term }

instance Retract (Typed (Typed a)) (Typed a) where
  retract = Just . embed

instance Embed (Typed (Typed a)) (Typed a) where
  embed t @ Typed { typedTerm = term } = t { typedTerm = embed term }

instance Retract (Intro (Elim a)) (Intro a) where
  retract = Just . embed

instance Embed (Intro (Elim a)) (Intro a) where
  embed t @ Tuple { tupleElems = elems } = t { tupleElems = fmap embed elems }
  embed e @ Elim { elim = term } = e { elim = embed term }

instance Embed a (Elim a) where
  embed = Atom

instance Retract a (Elim a) where
  retract = Just . Atom

instance Retract (Elim a) a where
  retract Atom { atom = out } = Just out
  retract _ = Nothing

instance Retract (Elim a) (Intro a) where
  retract = Just . Elim

instance Embed (Elim a) (Intro a) where
  embed = Elim

instance Retract (Typed a) (Elim a) where
  retract = Just . Intro

instance Embed (Typed a) (Elim a) where
  embed = Intro

instance Retract (Typed a) (Intro a) where
  retract = Just . Elim . embed

instance Embed (Typed a) (Intro a) where
  embed = Elim . embed

class HSubst t where
  hsubst :: (a -> Typed a) -> t a -> t a

hsubstElim :: (a -> Typed a) -> Elim a -> Elim a
hsubstElim f a = a >>>= f

hsubstIntro :: (a -> Typed a) -> Intro a -> Intro a
hsubstIntro f a = a >>>= f

hsubstFreeElim :: (a -> Free Typed Literal a)
               -> Free Elim Literal a
               -> Free Elim Literal a
hsubstFreeElim f a = a >>>= f

hsubstFreeIntro :: (a -> Free Typed Literal a)
                -> Free Intro Literal a
                -> Free Intro Literal a
hsubstFreeIntro f a = a >>>= f

hsubstNamelessFreeElim :: (a -> Typed Literal)
                       -> Free Elim Literal a
                       -> Elim Literal
hsubstNamelessFreeElim f a = a >>>= f

hsubstNamelessFreeIntro :: (a -> Typed Literal)
                        -> Free Intro Literal a
                        -> Intro Literal
hsubstNamelessFreeIntro f a = a >>>= f

hsubstClosedElim :: (a -> Closed Typed Literal a)
               -> Closed Elim Literal a
               -> Closed Elim Literal a
hsubstClosedElim f a = a >>>= f

hsubstClosedIntro :: (a -> Closed Typed Literal a)
                -> Closed Intro Literal a
                -> Closed Intro Literal a
hsubstClosedIntro f a = a >>>= f

hsubstNamelessClosedElim :: (a -> Typed a)
                         -> Closed Elim Literal a
                         -> Closed Elim Literal a
hsubstNamelessClosedElim f a = a >>>= f

hsubstNamelessClosedIntro :: (a -> Typed a)
                          -> Closed Intro Literal a
                          -> Closed Intro Literal a
hsubstNamelessClosedIntro f a = a >>>= f
