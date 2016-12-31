-- Copyright (c) 2016 Eric McCorkle.  All rights reserved.
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
             UndecidableInstances, FlexibleInstances #-}

-- | = Constructors for Free Terms
module Subst.Free(
       FreeAtom(..),
       Free(..),
       ) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Hashable
import Data.Hashable.Extras
import Prelude.Extras
import Subst.Abstract.Class
import Subst.Embed.Class
import Subst.Term.Class

-- | A datatype for transforming ordinary atoms into atoms and
-- variables.  If we have a nameless data structure @innerty atomty@,
-- we can represent the free terms over variable type @varty@ as
-- @innerty (FreeAtom atomty varty)@
data FreeAtom varty atomty =
    -- | A free variable.
    FreeVar { freeVar :: !varty }
    -- | An atom.
  | FreeAtom { freeAtom :: !atomty }
    deriving (Eq, Ord)

-- | A free @innerty@ with @atomty@ as atoms and @varty@ as variables.
newtype Free innerty varty atomty =
  Free { freeTerm :: innerty (FreeAtom varty atomty) }

instance (Eq1 innerty, Eq atomty, Eq varty) =>
         Eq (Free innerty atomty varty) where
  Free { freeTerm = term1 } == Free { freeTerm = term2 } = term1 ==# term2

instance Eq varty => Eq1 (FreeAtom varty)
instance Eq2 FreeAtom
instance (Eq1 innerty, Eq atomty) => Eq1 (Free innerty atomty)
instance (Eq1 innerty) => Eq2 (Free innerty)

instance (Ord1 innerty, Ord atomty, Ord varty) =>
         Ord (Free innerty atomty varty) where
  compare Free { freeTerm = term1 } Free { freeTerm = term2 } =
    compare1 term1 term2

instance Ord varty => Ord1 (FreeAtom varty)
instance Ord2 FreeAtom
instance (Ord1 innerty, Ord atomty) => Ord1 (Free innerty atomty)
instance (Ord1 innerty) => Ord2 (Free innerty)

instance (Hashable atomty, Hashable varty) =>
         Hashable (FreeAtom varty atomty) where
  hashWithSalt s FreeAtom { freeAtom = term } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` term
  hashWithSalt s FreeVar { freeVar = var } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` var

instance (Hashable1 innerty, Hashable atomty, Hashable varty) =>
         Hashable (Free innerty atomty varty) where
  hashWithSalt s = hashWithSalt1 s . freeTerm

instance Hashable varty => Hashable1 (FreeAtom varty)
instance Hashable2 FreeAtom
instance (Hashable1 innerty, Hashable atomty) => Hashable1 (Free innerty atomty)
instance (Hashable1 innerty) => Hashable2 (Free innerty)

instance Traversable innerty => Bifunctor (Free innerty) where
  bimap = bimapDefault

instance Traversable innerty => Bifoldable (Free innerty) where
  bifoldMap = bifoldMapDefault

instance Traversable innerty => Bitraversable (Free innerty) where
  bitraverse f g = (Free <$>) . traverse (bitraverse f g) . freeTerm

instance Bifunctor FreeAtom where
  bimap = bimapDefault

instance Bifoldable FreeAtom where
  bifoldMap = bifoldMapDefault

instance Bitraversable FreeAtom where
  bitraverse _ atomfunc FreeAtom { freeAtom = atom } =
    FreeAtom <$> atomfunc atom
  bitraverse varfunc _ FreeVar { freeVar = var } = FreeVar <$> varfunc var

instance Term FreeAtom where
  retype = retypeDefault
  closed = closedDefault

instance Embed atomty atomty FreeAtom where
  embed = FreeAtom

  retract FreeAtom { freeAtom = atom } = Just atom
  retract FreeVar {} = Nothing

instance (Traversable innerty) =>
         Term (Free innerty) where
  retype = retypeDefault
  closed = closedDefault

instance (Traversable innerty) =>
         Embed (innerty atomty) atomty (Free innerty) where
  embed = Free . fmap embed
  retract = mapM retract . freeTerm

instance Traversable innerty =>
         Abstract atomty varty (innerty atomty)
                  (Free innerty varty atomty) where
  abstract f =
    let
       toFreeAtom atom = maybe FreeAtom { freeAtom = atom } FreeVar (f atom)
    in
      Free . fmap toFreeAtom
