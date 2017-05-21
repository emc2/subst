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
--       Bound(..)
       ) where

import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Hashable
import Data.Traversable
import Prelude.Extras
import Subst.Abstract.Class
import Subst.Embed.Class
import Subst.Free
import Subst.Retract.Class
import Subst.Term.Class

type FlippedFree innerty varty atomty = Free innerty atomty varty

data BoundTerm innerty atomty varty =
    -- | A constructor for terms without any bound variables.
    BoundInner {
      -- | A base nameless term.
      boundInner :: innerty atomty
    }
    -- | A constructor for terms containing bound variables.  This can
    -- also be used to increment the deBruijn index of entire
    -- subterms.
  | BoundOuter {
      -- | Amount by which to increment the deBruijn index of the
      -- subterm.
      boundDepth :: !Word,
      -- | The subterm.
      boundOuter :: innerty (BoundTerm innerty atomty varty)
    }
    -- | A deBruijn indexed variable.
  | BoundVar {
      -- | The deBruijn index of the variable.  Note that this can be
      -- incremented by enclosing 'BoundOuter's.
      boundDepth :: !Word,
      -- | The base variable name, for multi-name binding contexts.
      boundVar :: !varty
    }

-- | A term that contains nothing but bound variables.
newtype Closed innerty atomty varty =
  Closed { closedTerm :: innerty (BoundTerm innerty atomty varty) }

instance (Eq1 innerty, Eq atomty, Eq varty) =>
         Eq (BoundTerm innerty atomty varty) where
  BoundInner { boundInner = inner1 } == BoundInner { boundInner = inner2 } =
    inner1 ==# inner2
  BoundOuter { boundDepth = depth1, boundOuter = outer1 } ==
    BoundOuter { boundDepth = depth2, boundOuter = outer2 } =
      depth1 == depth2 && outer1 ==# outer2
  BoundVar { boundDepth = depth1, boundVar = var1 } ==
    BoundVar { boundDepth = depth2, boundVar = var2 } =
      depth1 == depth2 && var1 == var2
  _ == _ = False

instance (Eq1 innerty, Eq atomty) => Eq1 (BoundTerm innerty atomty)
instance (Eq1 innerty) => Eq2 (BoundTerm innerty)

instance (Ord1 innerty, Ord atomty, Ord varty) =>
         Ord (BoundTerm innerty atomty varty) where
  compare BoundInner { boundInner = inner1 }
          BoundInner { boundInner = inner2 } = compare1 inner1 inner2
  compare BoundInner {} _ = LT
  compare _ BoundInner {} = GT
  compare BoundOuter { boundDepth = depth1, boundOuter = outer1 }
          BoundOuter { boundDepth = depth2, boundOuter = outer2 } =
    case compare depth1 depth2 of
      EQ -> compare1 outer1 outer2
      out -> out
  compare BoundOuter {} _ = LT
  compare _ BoundOuter {} = GT
  compare BoundVar { boundDepth = depth1, boundVar = var1 }
          BoundVar { boundDepth = depth2, boundVar = var2 } =
    case compare depth1 depth2 of
      EQ -> compare var1 var2
      out -> out

instance (Ord1 innerty, Ord atomty) => Ord1 (BoundTerm innerty atomty)
instance (Ord1 innerty) => Ord2 (BoundTerm innerty)

instance (Hashable (innerty (BoundTerm innerty atomty varty)),
          Hashable (innerty atomty), Hashable atomty, Hashable varty) =>
         Hashable (BoundTerm innerty atomty varty) where
  hashWithSalt s BoundInner { boundInner = inner } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` inner
  hashWithSalt s BoundOuter { boundDepth = depth, boundOuter = outer } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` depth `hashWithSalt` outer
  hashWithSalt s BoundVar { boundDepth = depth, boundVar = var } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` depth `hashWithSalt` var
{-
instance (Hashable1 innerty, Hashable atomty) =>
         Hashable1 (Bound innerty atomty)
instance (Hashable1 innerty) => Hashable2 (Bound innerty)
-}
instance Traversable innerty => Bifunctor (BoundTerm innerty) where
  bimap = bimapDefault

instance Traversable innerty => Bifoldable (BoundTerm innerty) where
  bifoldMap = bifoldMapDefault

instance Traversable innerty => Bitraversable (BoundTerm innerty) where
  bitraverse atomfunc _ BoundInner { boundInner = inner } =
    BoundInner <$> traverse atomfunc inner
  bitraverse atomfunc varfunc b @ BoundOuter { boundOuter = outer } =
    (\newouter -> b { boundOuter = newouter }) <$>
      traverse (bitraverse atomfunc varfunc) outer
  bitraverse _ varfunc b @ BoundVar { boundVar = var } =
    (\newvar -> b { boundVar = newvar }) <$> varfunc var

instance Traversable innerty => Functor (BoundTerm innerty atomty) where
  fmap = fmapDefault

instance Traversable innerty => Foldable (BoundTerm innerty atomty) where
  foldMap = foldMapDefault

instance Traversable innerty => Traversable (BoundTerm innerty atomty) where
  traverse _ BoundInner { boundInner = inner } = BoundInner <$> pure inner
  traverse varfunc b @ BoundOuter { boundOuter = outer } =
    (\newouter -> b { boundOuter = newouter }) <$>
      traverse (traverse varfunc) outer
  traverse varfunc b @ BoundVar { boundVar = var } =
    (\newvar -> b { boundVar = newvar }) <$> varfunc var

instance Traversable innerty => Term (BoundTerm innerty) where
  retype = retypeDefault
  closed = closedDefault

embedInner :: (Traversable innerty, Monad innerty) =>
              innerty atomty -> BoundTerm innerty atomty varty
embedInner = BoundInner

retractInner :: (Traversable innerty, Monad innerty) =>
                BoundTerm innerty atomty varty -> Maybe (innerty atomty)
retractInner BoundInner { boundInner = inner } = Just inner
retractInner BoundOuter { boundOuter = outer } = fmap join (mapM retract outer)
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
  retract BoundInner { boundInner = inner } = Just (embed inner)
  retract BoundOuter { boundOuter = outer } =
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
