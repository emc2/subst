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
{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             UndecidableInstances, FlexibleInstances,
             IncoherentInstances, ScopedTypeVariables #-}

-- | = Constructors for Free Terms
module Subst.Free(
       FreeAtom(..),
       Free(..),
       substFree'
       ) where

import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Hashable
import Data.Traversable
import Prelude.Extras
import Subst.Class
import Subst.Abstract.Class
import Subst.Embed.Class
import Subst.Retract.Class
import Subst.Term.Class

-- | A datatype for transforming ordinary atoms into atoms and
-- variables.  If we have a nameless data structure @innerty atomty@,
-- we can represent the free terms over variable type @varty@ as
-- @innerty (FreeAtom atomty varty)@
data FreeAtom atomty varty =
    -- | A free variable.
    FreeVar { freeVar :: !varty }
    -- | An atom.
  | FreeAtom { freeAtom :: !atomty }
    deriving (Eq, Ord)

-- | A free @innerty@ with @atomty@ as atoms and @varty@ as variables.
newtype Free innerty atomty varty =
  Free { freeTerm :: innerty (FreeAtom atomty varty) }

-- Structural equality, comparison, and hashing on free terms is
-- straightforward.

instance (Eq1 innerty, Eq atomty, Eq varty) =>
         Eq (Free innerty atomty varty) where
  Free { freeTerm = term1 } == Free { freeTerm = term2 } = term1 ==# term2

instance Eq atomty => Eq1 (FreeAtom atomty)
instance Eq2 FreeAtom
instance (Eq1 innerty, Eq atomty) => Eq1 (Free innerty atomty)
instance (Eq1 innerty) => Eq2 (Free innerty)

instance (Ord1 innerty, Ord atomty, Ord varty) =>
         Ord (Free innerty atomty varty) where
  compare Free { freeTerm = term1 } Free { freeTerm = term2 } =
    compare1 term1 term2

instance Ord atomty => Ord1 (FreeAtom atomty)
instance Ord2 FreeAtom
instance (Ord1 innerty, Ord atomty) => Ord1 (Free innerty atomty)
instance (Ord1 innerty) => Ord2 (Free innerty)

instance (Hashable atomty, Hashable varty) =>
         Hashable (FreeAtom atomty varty) where
  hashWithSalt s FreeAtom { freeAtom = term } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` term
  hashWithSalt s FreeVar { freeVar = var } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` var

instance (Hashable (innerty (FreeAtom atomty varty)),
          Hashable atomty, Hashable varty) =>
         Hashable (Free innerty atomty varty) where
  hashWithSalt s = hashWithSalt s . freeTerm

-- FreeAtoms are trivally traversable on both atoms and terms
instance Bifunctor FreeAtom where
  bimap = bimapDefault

instance Bifoldable FreeAtom where
  bifoldMap = bifoldMapDefault

instance Bitraversable FreeAtom where
  bitraverse atomfunc _ FreeAtom { freeAtom = atom } =
    FreeAtom <$> atomfunc atom
  bitraverse _ varfunc FreeVar { freeVar = var } = FreeVar <$> varfunc var

instance Functor (FreeAtom atomty) where
  fmap = fmapDefault

instance Foldable (FreeAtom atomty) where
  foldMap = foldMapDefault

instance Traversable (FreeAtom atomty) where
  traverse atomfunc FreeVar { freeVar = var } =
    FreeVar <$> atomfunc var
  traverse _ FreeAtom { freeAtom = atom } = pure FreeAtom { freeAtom = atom }

-- FreeAtoms are trivially applicative monads on the variable type.
instance Applicative (FreeAtom atomty) where
  pure = return
  (<*>) = ap

instance Monad (FreeAtom atomty) where
  return = FreeVar

  FreeAtom { freeAtom = atom } >>= _ = FreeAtom { freeAtom = atom }
  FreeVar { freeVar = var } >>= f = f var

-- We get the Term class for free on FreeAtoms.
instance Term FreeAtom where
  retype = retypeDefault
  closed = closedDefault

-- We can trivially embed and retract atoms and variables into and
-- from FreeAtoms.
instance Embed atomty (FreeAtom atomty varty) where
  embed = FreeAtom

instance Retract atomty (FreeAtom atomty varty) where
  retract = Just . FreeAtom

instance Embed a b => Embed a (FreeAtom atomty b) where
  embed = FreeVar . embed

instance Retract a b => Retract a (FreeAtom atomty b) where
  retract = fmap FreeVar . retract

instance Retract (FreeAtom atomty varty) atomty where
  retract FreeAtom { freeAtom = atom } = Just atom
  retract FreeVar {} = Nothing

instance Retract a b => Retract (FreeAtom atomty a) b where
  retract FreeVar { freeVar = var } = retract var
  retract FreeAtom {} = Nothing

instance Inject (FreeAtom atomty) where
  inject = FreeVar

instance (Embed atomty resty, Embed valty resty) =>
         Subst valty resty (FreeAtom atomty) where
  FreeAtom { freeAtom = atom } >>>= _ = embed atom
  FreeVar { freeVar = var } >>>= f = embed (f var)

-- Free terms are traversable on both the atoms and the variables,
-- provided the inner term type is.

instance Functor innerty => Bifunctor (Free innerty) where
  bimap f g = Free . fmap (bimap f g) . freeTerm

instance Foldable innerty => Bifoldable (Free innerty) where
  bifoldMap f g = foldMap (bifoldMap f g) . freeTerm

instance Traversable innerty => Bitraversable (Free innerty) where
  bitraverse f g = (Free <$>) . traverse (bitraverse f g) . freeTerm

instance Functor innerty => Functor (Free innerty atomty) where
  fmap f = Free . fmap (fmap f) . freeTerm

instance Foldable innerty => Foldable (Free innerty atomty) where
  foldMap f = foldMap (foldMap f) . freeTerm

instance Traversable innerty => Traversable (Free innerty atomty) where
  traverse f = (Free <$>) . traverse (traverse f) . freeTerm

-- Free Terms are applicative monads on variables, where the monad
-- instance represents variable substitution
instance Applicative innerty => Applicative (Free innerty atomty) where
  pure = Free . pure . pure

  Free { freeTerm = f } <*> Free { freeTerm = a } =
    Free { freeTerm = fmap (<*>) f <*> a }

instance (Traversable innerty, Monad innerty) =>
         Monad (Free innerty atomty) where
  return = Free . return . FreeVar

  Free { freeTerm = term } >>= f =
    Free { freeTerm = term >>= (freeTerm . (>>= f) . Free . return) }

-- Free terms have a Term instance as long as the inner type is traversable.
instance (Traversable innerty) => Term (Free innerty) where
  retype = retypeDefault
  closed = closedDefault

-- We can embed both variables and atoms into Free terms.

instance (Functor innerty, Embed atomty (innerty atomty)) =>
         Embed atomty (Free innerty atomty varty) where
  embed = Free . fmap FreeAtom . embed

instance (Functor innerty, Embed atomty (innerty atomty)) =>
         Retract atomty (Free innerty atomty varty) where
  retract = Just . embed

instance (Functor innerty, Embed varty (innerty varty)) =>
         Embed varty (Free innerty atomty varty) where
  embed = Free . fmap FreeVar . embed

instance (Functor innerty, Embed varty (innerty varty)) =>
         Retract varty (Free innerty atomty varty) where
  retract = Just . embed

-- We can also embed nameless terms into free terms.
instance (Functor innerty) =>
         Embed (innerty atomty) (Free innerty atomty varty) where
  embed = Free . fmap embed

instance (Functor innerty) =>
         Retract (innerty atomty) (Free innerty atomty varty) where
  retract = Just . Free . fmap embed

instance (Functor innerty) =>
         Embed (innerty (FreeAtom atomty varty))
               (Free innerty atomty varty) where
  embed = Free

instance (Functor innerty) =>
         Retract (innerty (FreeAtom atomty varty))
                 (Free innerty atomty varty) where
  retract = Just . Free

retractFree :: (Traversable innerty) =>
               (Free innerty atomty varty) -> Maybe (innerty atomty)
retractFree = mapM retract . freeTerm

-- We can retract nameless terms from a Free term.
instance (Traversable innerty) =>
         Retract (Free innerty atomty varty) (innerty atomty) where
  retract = retractFree

-- If we can retract atoms from nameless terms, then we can retract
-- atoms from Free terms as well.
instance (Traversable innerty, Retract (innerty atomty) atomty) =>
         Retract (Free innerty atomty varty) atomty where
  retract term = retractFree term >>= retract

-- We can abstract from atoms to variables over a nameless term, giving
-- us a Free term.
instance Functor innerty =>
         Abstract atomty varty (innerty atomty)
                  (Free innerty atomty varty) where
  abstract f =
    let
       toFreeAtom atom = maybe FreeAtom { freeAtom = atom } FreeVar (f atom)
    in
      Free . fmap toFreeAtom

-- We can abstract from nameless terms to variables over a nameless
-- term, giving us a Free term.
instance (Abstract (innerty atomty) (FreeAtom atomty varty)
                   (innerty atomty) (innerty (FreeAtom atomty varty)),
          Traversable innerty) =>
         Abstract (innerty atomty) varty (innerty atomty)
                  (Free innerty atomty varty) where
  abstract f =
    let
      absfun :: Traversable innerty =>
                (innerty atomty -> Maybe varty) ->
                innerty atomty ->
                Maybe (FreeAtom atomty varty)
      absfun f' term =
        do
          var <- f' term
          return FreeVar { freeVar = var }
    in
      Free . abstract (absfun f)

-- We can abstract from atoms to variables in a Free term.
instance Functor innerty =>
         Abstract atomty varty (Free innerty atomty varty)
                  (Free innerty atomty varty) where
  abstract f =
    let
      mapfun FreeAtom { freeAtom = atom } =
       maybe FreeAtom { freeAtom = atom } FreeVar (f atom)
      mapfun FreeVar { freeVar = var } = FreeVar { freeVar = var }
    in
      Free . fmap mapfun . freeTerm

-- We can abstract from FreeAtoms to variables in a Free term.
instance Functor innerty =>
         Abstract (FreeAtom atomty varty) varty
                  (Free innerty atomty varty)
                  (Free innerty atomty varty) where
  abstract f =
    let
      mapfun f' atom = maybe atom FreeVar (f' atom)
    in
      Free . fmap (mapfun f) . freeTerm

-- We can abstract from nameless terms to variables in a Free term.
instance (Abstract (innerty (FreeAtom atomty varty))
                   (FreeAtom atomty varty)
                   (innerty (FreeAtom atomty varty))
                   (innerty (FreeAtom atomty varty)),
          Traversable innerty) =>
         Abstract (innerty atomty) varty (Free innerty atomty varty)
                  (Free innerty atomty varty) where
  abstract f =
    let
      absfun :: Traversable innerty =>
                (innerty atomty -> Maybe varty) ->
                innerty (FreeAtom atomty varty) ->
                Maybe (FreeAtom atomty varty)
      absfun f' term =
        do
          retracted <- mapM retract term
          var <- f' retracted
          return FreeVar { freeVar = var }
    in
      Free . abstract (absfun f) . freeTerm

-- We can abstract from free terms to variables in a Free term.
instance (Abstract (innerty (FreeAtom atomty varty))
                   (FreeAtom atomty varty)
                   (innerty (FreeAtom atomty varty))
                   (innerty (FreeAtom atomty varty)),
          Traversable innerty) =>
         Abstract (innerty (FreeAtom atomty varty)) varty
                  (Free innerty atomty varty)
                  (Free innerty atomty varty) where
  abstract f =
    let
      absfun :: Traversable innerty =>
                (innerty (FreeAtom atomty varty) -> Maybe varty) ->
                innerty (FreeAtom atomty varty) ->
                Maybe (FreeAtom atomty varty)
      absfun f' term =
        do
          var <- f' term
          return FreeVar { freeVar = var }
    in
      Free . abstract (absfun f) . freeTerm

instance (Functor termty,
          Embed (otherty (FreeAtom atomty varty))
                (termty (FreeAtom atomty varty))) =>
         Embed (Free otherty atomty varty) (Free termty atomty varty) where
  embed = Free . embed . freeTerm

instance (Functor termty,
          Embed (otherty (FreeAtom atomty varty))
                (termty (FreeAtom atomty varty))) =>
         Retract (Free otherty atomty varty) (Free termty atomty varty) where
  retract = Just . embed

instance (Functor termty,
          Embed (termty (innerty (FreeAtom atomty varty)))
                (termty (FreeAtom atomty varty))) =>
         Retract (termty (Free innerty atomty varty))
                 (Free termty atomty varty) where
  retract = Just . embed

instance (Functor termty,
          Embed (termty (innerty (FreeAtom atomty varty)))
                (termty (FreeAtom atomty varty))) =>
         Embed (termty (Free innerty atomty varty))
               (Free termty atomty varty) where
  embed = Free . embed . fmap freeTerm

substFree' :: forall valty resty termty atomty a.
              (Subst valty resty termty, Embed atomty valty) =>
              Free termty atomty a
           -- ^ The term into which to perform the substitution.
           -> (a -> valty)
           -- ^ The substitution function.
           -> resty
           -- ^ The resulting term.
substFree' Free { freeTerm = term } f =
  let
    substFunc :: FreeAtom atomty a -> valty
    substFunc FreeVar { freeVar = var } = embed (f var)
    substFunc FreeAtom { freeAtom = atom } = embed atom
  in
    term >>>= substFunc

-- 'Subst' instance using 'substDefault'.
instance (Inject termty) => Inject (Free termty atomty) where
  inject = Free . inject . inject

instance (Subst valty resty termty, Embed atomty valty) =>
         Subst valty resty (Free termty atomty) where
  Free { freeTerm = term } >>>= f =
    let
      substfun :: (a -> valty) -> FreeAtom atomty a -> valty
      substfun f' FreeVar { freeVar = var } = f' var
      substfun _ FreeAtom { freeAtom = atom } = embed atom
    in
      term >>>= substfun f
