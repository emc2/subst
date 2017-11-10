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
             IncoherentInstances #-}

module Subst.Bound(
       Bound(..)
       ) where

--import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Hashable
import Data.Traversable
import Prelude.Extras
--import Subst.Abstract.Class
--import Subst.Class
--import Subst.Embed.Class
--import Subst.Free
--import Subst.Retract.Class
import Subst.Term.Class

--type FlippedFree innerty varty atomty = Free innerty atomty varty

newtype NonZero = NonZero { nonZero :: Word }
  deriving (Ord, Eq)

data BoundTerm innerty atomty varty =
    -- | A constructor for terms without any bound variables.
    BoundInner {
      -- | A base nameless term.
      innerTerm :: innerty atomty
    }
    -- | A constructor for terms containing bound variables.  This can
    -- also be used to increment the deBruijn index of entire
    -- subterms.
  | BoundOuter {
      -- | Amount by which to increment the deBruijn index of the
      -- subterm.
      outerDepth :: !NonZero,
      -- | The subterm.
      outerTerm :: innerty (BoundTerm innerty atomty varty)
    }
    -- | A deBruijn indexed variable.
  | BoundVar {
      -- | The deBruijn index of the variable.  Note that this can be
      -- incremented by enclosing 'BoundOuter's.
      varDepth :: !Word,
      -- | The base variable name, for multi-name binding contexts.
      varAtom :: !varty
    }

-- | A term that contains nothing but bound variables.
newtype Bound innerty atomty varty =
  Bound { boundTerm :: innerty (BoundTerm innerty atomty varty) }

instance (Eq1 innerty, Eq atomty, Eq varty) =>
         Eq (BoundTerm innerty atomty varty) where
  BoundInner { innerTerm = inner1 } == BoundInner { innerTerm = inner2 } =
    inner1 ==# inner2
  BoundOuter { outerDepth = depth1, outerTerm = outer1 } ==
    BoundOuter { outerDepth = depth2, outerTerm = outer2 } =
      depth1 == depth2 && outer1 ==# outer2
  BoundVar { varDepth = depth1, varAtom = var1 } ==
    BoundVar { varDepth = depth2, varAtom = var2 } =
      depth1 == depth2 && var1 == var2
  _ == _ = False

instance (Eq1 innerty, Eq atomty) => Eq1 (BoundTerm innerty atomty)
instance (Eq1 innerty) => Eq2 (BoundTerm innerty)

instance (Eq1 innerty, Eq atomty, Eq varty) =>
         Eq (Bound innerty atomty varty) where
  Bound { boundTerm = t1 } == Bound { boundTerm = t2 } = t1 ==# t2

instance (Eq1 innerty, Eq atomty) => Eq1 (Bound innerty atomty)
instance (Eq1 innerty) => Eq2 (Bound innerty)

instance (Ord1 innerty, Ord atomty, Ord varty) =>
         Ord (BoundTerm innerty atomty varty) where
  compare BoundInner { innerTerm = inner1 }
          BoundInner { innerTerm = inner2 } = compare1 inner1 inner2
  compare BoundInner {} _ = LT
  compare _ BoundInner {} = GT
  compare BoundOuter { outerDepth = depth1, outerTerm = outer1 }
          BoundOuter { outerDepth = depth2, outerTerm = outer2 } =
    case compare depth1 depth2 of
      EQ -> compare1 outer1 outer2
      out -> out
  compare BoundOuter {} _ = LT
  compare _ BoundOuter {} = GT
  compare BoundVar { varDepth = depth1, varAtom = var1 }
          BoundVar { varDepth = depth2, varAtom = var2 } =
    case compare depth1 depth2 of
      EQ -> compare var1 var2
      out -> out

instance (Ord1 innerty, Ord atomty) => Ord1 (BoundTerm innerty atomty)
instance (Ord1 innerty) => Ord2 (BoundTerm innerty)

instance (Ord1 innerty, Ord atomty, Ord varty) =>
         Ord (Bound innerty atomty varty) where
  compare Bound { boundTerm = t1 }
          Bound { boundTerm = t2 } =
    compare1 t1 t2

instance (Ord1 innerty, Ord atomty) => Ord1 (Bound innerty atomty)
instance (Ord1 innerty) => Ord2 (Bound innerty)

instance Enum NonZero where
  toEnum n = NonZero { nonZero = toEnum n - 1 }
  fromEnum NonZero { nonZero = n } = fromEnum n - 1

instance Hashable NonZero where
  hashWithSalt s = hashWithSalt s . nonZero

instance (Hashable (innerty (BoundTerm innerty atomty varty)),
          Hashable (innerty atomty), Hashable atomty, Hashable varty) =>
         Hashable (BoundTerm innerty atomty varty) where
  hashWithSalt s BoundInner { innerTerm = inner } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` inner
  hashWithSalt s BoundOuter { outerDepth = depth, outerTerm = outer } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` depth `hashWithSalt` outer
  hashWithSalt s BoundVar { varDepth = depth, varAtom = var } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` depth `hashWithSalt` var

instance (Hashable (innerty (BoundTerm innerty atomty varty)),
          Hashable (innerty atomty), Hashable atomty, Hashable varty) =>
         Hashable (Bound innerty atomty varty) where
  hashWithSalt s = hashWithSalt s . boundTerm

-- Bound terms are traversable on both atoms and terms
instance Traversable innerty => Bifunctor (BoundTerm innerty) where
  bimap = bimapDefault

instance Traversable innerty => Bifoldable (BoundTerm innerty) where
  bifoldMap = bifoldMapDefault

instance Traversable innerty => Bitraversable (BoundTerm innerty) where
  bitraverse atomfunc _ BoundInner { innerTerm = inner } =
    BoundInner <$> traverse atomfunc inner
  bitraverse atomfunc varfunc b @ BoundOuter { outerTerm = outer } =
    (\newouter -> b { outerTerm = newouter }) <$>
      traverse (bitraverse atomfunc varfunc) outer
  bitraverse _ varfunc b @ BoundVar { varAtom = var } =
    (\newvar -> b { varAtom = newvar }) <$> varfunc var

instance Traversable innerty => Functor (BoundTerm innerty atomty) where
  fmap = fmapDefault

instance Traversable innerty => Foldable (BoundTerm innerty atomty) where
  foldMap = foldMapDefault

instance Traversable innerty => Traversable (BoundTerm innerty atomty) where
  traverse _ BoundInner { innerTerm = inner } = BoundInner <$> pure inner
  traverse varfunc b @ BoundOuter { outerTerm = outer } =
    (\newouter -> b { outerTerm = newouter }) <$>
      traverse (traverse varfunc) outer
  traverse varfunc b @ BoundVar { varAtom = var } =
    (\newvar -> b { varAtom = newvar }) <$> varfunc var

instance Traversable innerty => Term (BoundTerm innerty) where
  retype = retypeDefault
  closed = closedDefault

-- The Applicative instance doesn't differentiate between different
-- levels of binding.

toWord :: Enum n => n -> Word
toWord = toEnum . fromEnum

-- | Apply a term containing functions to a single atom.
applyInner :: Functor innerty =>
              Word
           -- ^ The context depth accumulated so far.
           -> a
           -- ^ The atom to apply to each function.
           -> BoundTerm innerty atomty (a -> b)
           -- ^ The term to which to apply the atom.
           -> BoundTerm innerty atomty b
applyInner _ _ BoundInner { innerTerm = inner } =
  BoundInner { innerTerm = inner }
applyInner ctxdepth a f @ BoundOuter { outerDepth = depth, outerTerm = term } =
  f { outerTerm = fmap (applyInner (ctxdepth + toWord depth) a) term }
applyInner ctxdepth a b @ BoundVar { varDepth = depth, varAtom = f } =
  b { varDepth = ctxdepth + depth, varAtom = f a }

-- | Applicative instance, with context depths.
applyOuter :: Functor innerty =>
              Word
           -- ^ The context depth accumulated so far.
           -> BoundTerm innerty atomty (a -> b)
           -- ^ The term to apply..
           -> BoundTerm innerty atomty a
           -- ^ The term to which to apply..
           -> BoundTerm innerty atomty b
applyOuter _ _ BoundInner { innerTerm = inner } =
  BoundInner { innerTerm = inner }
applyOuter ctxdepth f a @ BoundOuter { outerDepth = depth, outerTerm = term } =
  a { outerTerm = fmap (applyOuter (ctxdepth + toWord depth) f) term }
applyOuter ctxdepth f BoundVar { varDepth = fdepth, varAtom = a } =
  applyInner (ctxdepth + fdepth) a f

instance (Applicative innerty, Traversable innerty) =>
         Applicative (BoundTerm innerty atomty) where
  pure var = BoundVar { varDepth = 0, varAtom = var }

  (<*>) = applyOuter 0

-- Bound terms are traversable on both atoms and terms
instance Traversable innerty => Bifunctor (Bound innerty) where
  bimap = bimapDefault

instance Traversable innerty => Bifoldable (Bound innerty) where
  bifoldMap = bifoldMapDefault

instance Traversable innerty => Bitraversable (Bound innerty) where
  bitraverse atomfunc varfunc =
    (Bound <$>) .  traverse (bitraverse atomfunc varfunc) . boundTerm

instance Traversable innerty => Functor (Bound innerty atomty) where
  fmap = fmapDefault

instance Traversable innerty => Foldable (Bound innerty atomty) where
  foldMap = foldMapDefault

instance Traversable innerty => Traversable (Bound innerty atomty) where
  traverse varfunc = (Bound <$>) .  traverse (traverse varfunc) . boundTerm

instance Traversable innerty => Term (Bound innerty) where
  retype = retypeDefault
  closed = closedDefault

instance (Applicative termty, Traversable termty) =>
         Applicative (Bound termty atomty) where
  pure = Bound . pure . pure

  Bound { boundTerm = f } <*> Bound { boundTerm = a } =
    Bound { boundTerm = fmap (<*>) f <*> a }

{-
embedInner :: (Traversable innerty, Monad innerty) =>
              innerty atomty -> BoundTerm innerty atomty varty
embedInner = BoundInner

retractInner :: (Traversable innerty, Monad innerty) =>
                BoundTerm innerty atomty varty -> Maybe (innerty atomty)
retractInner BoundInner { innerTerm = inner } = Just inner
retractInner BoundOuter { outerTerm = outer } = fmap join (mapM retract outer)
retractInner BoundVar {} = Nothing

instance (Traversable innerty, Monad innerty) =>
         Embed atomty (BoundTerm innerty atomty varty) where
  embed = embedInner . return

instance (Traversable innerty, Monad innerty) =>
         Embed (innerty atomty) (BoundTerm innerty atomty varty) where
  embed = embedInner

instance (Traversable innerty, Monad innerty) =>
         Retract (innerty atomty) (BoundTerm innerty atomty varty) where
  retract = retractInner

instance (Traversable innerty, Monad innerty) =>
         Retract (Free innerty atomty varty)
                 (BoundTerm innerty atomty varty) where
  retract BoundInner { innerTerm = inner } = Just (embed inner)
  retract BoundOuter { outerTerm = outer } =
    fmap (embed . join) (mapM retractInner outer)
  retract BoundVar {} = Nothing

instance Traversable innerty =>
         Abstract atomty varty (innerty atomty)
                  (Closed innerty atomty varty) where
  abstract f =
    let
       toFreeAtom atom = maybe FreeAtom { freeAtom = atom } FreeVar (f atom)
    in
      Closed . fmap toFreeAtom

{-
instance Traversable innerty =>
         Abstract varty varty (innerty (FreeAtom atomty varty))
                  (Bound innerty atomty (FreeAtom atomty varty)) where
  abstract f =
    let
       toFreeAtom atom = maybe FreeAtom { freeAtom = atom } FreeVar (f atom)
    in
      fmap _
-}
-}
