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
       BoundTerm(..),
       Closed,
       Bound,
       closedTerm,
       boundTerm
       ) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Hashable
import Data.Traversable
import Prelude.Extras
--import Subst.Abstract.Class
import Subst.Class
import Subst.Embed.Class
import Subst.Free
import Subst.Retract.Class
import Subst.Term.Class

--type FlippedFree innerty varty atomty = Free innerty atomty varty

newtype NonZero = NonZero { nonZero :: Word }
  deriving (Ord, Eq)

data BoundTerm innerty termty atomty varty =
    -- | A constructor for terms without any bound variables.
    BoundInner {
      -- | A base nameless term.
      innerTerm :: innerty
    }
    -- | A constructor for terms containing bound variables.  This can
    -- also be used to increment the deBruijn index of entire
    -- subterms.
  | BoundOuter {
      -- | Amount by which to increment the deBruijn index of the
      -- subterm.
      outerDepth :: !NonZero,
      -- | The subterm.
      outerTerm :: termty (BoundTerm innerty termty atomty varty)
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
newtype Closed termty atomty varty =
  Closed {
    closedTerm :: termty (BoundTerm (termty atomty) termty atomty varty)
  }

-- | A term that contains a mix of bound and free variables.
newtype Bound termty atomty varty =
  Bound {
    boundTerm :: termty (BoundTerm (Free termty atomty varty)
                                   termty atomty varty)
  }

instance (Eq1 termty, Eq innerty, Eq atomty, Eq varty) =>
         Eq (BoundTerm innerty termty atomty varty) where
  BoundInner { innerTerm = inner1 } == BoundInner { innerTerm = inner2 } =
    inner1 == inner2
  BoundOuter { outerDepth = depth1, outerTerm = outer1 } ==
    BoundOuter { outerDepth = depth2, outerTerm = outer2 } =
      depth1 == depth2 && outer1 ==# outer2
  BoundVar { varDepth = depth1, varAtom = var1 } ==
    BoundVar { varDepth = depth2, varAtom = var2 } =
      depth1 == depth2 && var1 == var2
  _ == _ = False

instance (Eq1 termty, Eq innerty, Eq atomty) =>
         Eq1 (BoundTerm innerty termty atomty)
instance (Eq1 termty, Eq innerty) => Eq2 (BoundTerm innerty termty)

instance (Eq1 termty, Eq atomty, Eq (termty atomty), Eq varty) =>
         Eq (Closed termty atomty varty) where
  Closed { closedTerm = t1 } == Closed { closedTerm = t2 } = t1 ==# t2

instance (Eq1 termty, Eq atomty, Eq (termty atomty)) =>
         Eq1 (Closed termty atomty)

instance (Eq1 termty, Eq atomty, Eq (termty atomty), Eq varty) =>
         Eq (Bound termty atomty varty) where
  Bound { boundTerm = t1 } == Bound { boundTerm = t2 } = t1 ==# t2

instance (Eq1 termty, Eq atomty, Eq (termty atomty)) =>
         Eq1 (Bound termty atomty)

instance (Ord1 termty, Ord innerty, Ord atomty, Ord varty) =>
         Ord (BoundTerm innerty termty atomty varty) where
  compare BoundInner { innerTerm = inner1 }
          BoundInner { innerTerm = inner2 } = compare inner1 inner2
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

instance (Ord1 termty, Ord innerty, Ord atomty) =>
         Ord1 (BoundTerm innerty termty atomty)
instance (Ord1 termty, Ord innerty) => Ord2 (BoundTerm innerty termty)

instance (Ord1 termty, Ord atomty, Ord (termty atomty), Ord varty) =>
         Ord (Closed termty atomty varty) where
  compare Closed { closedTerm = t1 }
          Closed { closedTerm = t2 } =
    compare1 t1 t2

instance (Ord1 termty, Ord atomty, Ord (termty atomty)) =>
         Ord1 (Closed termty atomty)

instance (Ord1 termty, Ord atomty, Ord (termty atomty), Ord varty) =>
         Ord (Bound termty atomty varty) where
  compare Bound { boundTerm = t1 } Bound { boundTerm = t2 } =
    compare1 t1 t2

instance (Ord1 termty, Ord atomty, Ord (termty atomty)) =>
         Ord1 (Bound termty atomty)

instance Enum NonZero where
  toEnum n = NonZero { nonZero = toEnum n - 1 }
  fromEnum NonZero { nonZero = n } = fromEnum n - 1

toWord :: Enum n => n -> Word
toWord = toEnum . fromEnum

instance Hashable NonZero where
  hashWithSalt s = hashWithSalt s . nonZero

instance (Hashable (termty (BoundTerm innerty termty atomty varty)),
          Hashable (termty atomty), Hashable innerty,
          Hashable atomty, Hashable varty) =>
         Hashable (BoundTerm innerty termty atomty varty) where
  hashWithSalt s BoundInner { innerTerm = inner } =
    s `hashWithSalt` (0 :: Word) `hashWithSalt` inner
  hashWithSalt s BoundOuter { outerDepth = depth, outerTerm = outer } =
    s `hashWithSalt` (1 :: Word) `hashWithSalt` depth `hashWithSalt` outer
  hashWithSalt s BoundVar { varDepth = depth, varAtom = var } =
    s `hashWithSalt` (2 :: Word) `hashWithSalt` depth `hashWithSalt` var

instance (Hashable (termty (BoundTerm (termty atomty) termty atomty varty)),
          Hashable (termty atomty), Hashable atomty, Hashable varty) =>
         Hashable (Closed termty atomty varty) where
  hashWithSalt s = hashWithSalt s . closedTerm

instance (Hashable (termty (BoundTerm (Free termty atomty varty)
                                      termty atomty varty)),
          Hashable (termty atomty), Hashable atomty, Hashable varty) =>
         Hashable (Bound termty atomty varty) where
  hashWithSalt s = hashWithSalt s . boundTerm

instance Traversable termty => Functor (BoundTerm innerty termty atomty) where
   fmap = fmapDefault

instance Traversable termty => Foldable (BoundTerm innerty termty atomty) where
   foldMap = foldMapDefault

instance Traversable termty =>
         Traversable (BoundTerm innerty termty atomty) where
  traverse _ BoundInner { innerTerm = inner } = BoundInner <$> pure inner
  traverse varfunc b @ BoundOuter { outerTerm = outer } =
    (\newouter -> b { outerTerm = newouter }) <$>
      traverse (traverse varfunc) outer
  traverse varfunc b @ BoundVar { varAtom = var } =
    (\newvar -> b { varAtom = newvar }) <$> varfunc var


instance (Applicative termty, Traversable termty) =>
         Applicative (BoundTerm innerty termty atomty) where
  pure var = BoundVar { varDepth = 0, varAtom = var }

  (<*>) =
    let
      applyInner _ _ BoundInner { innerTerm = inner } =
        BoundInner { innerTerm = inner }
      applyInner ctxdepth a f @ BoundOuter { outerDepth = depth,
                                             outerTerm = term } =
        f { outerTerm = fmap (applyInner (ctxdepth + toWord depth) a) term }
      applyInner ctxdepth a b @ BoundVar { varDepth = depth, varAtom = f } =
        b { varDepth = ctxdepth + depth, varAtom = f a }

      applyOuter _ _ BoundInner { innerTerm = inner } =
        BoundInner { innerTerm = inner }
      applyOuter ctxdepth f a @ BoundOuter { outerDepth = depth,
                                             outerTerm = term } =
        a { outerTerm = fmap (applyOuter (ctxdepth + toWord depth) f) term }
      applyOuter ctxdepth f BoundVar { varDepth = fdepth, varAtom = a } =
        applyInner (ctxdepth + fdepth) a f
    in
      applyOuter 0
{-
instance (Embed (termty a) (termty' a), Functor termty',
          Applicative (BoundTerm (termty' atomty) termty' atomty)) =>
         Embed (termty a) (termty' (BoundTerm (termty' atomty)
                                              termty' atomty a)) where
  embed term = fmap pure (embed term)

instance (Embed (termty a) (termty' a), Functor termty',
          Applicative (BoundTerm (termty' atomty) termty' atomty)) =>
         Retract (termty a) (termty' (BoundTerm (termty' atomty)
                                                termty' atomty a)) where
  retract = Just . embed
-}
instance (Embed valty (termty (BoundTerm innerty termty atomty varty)),
          Inject termty) =>
         Subst valty varty (termty (BoundTerm innerty termty atomty varty))
               (BoundTerm innerty termty atomty) where
  BoundVar { varDepth = 0, varAtom = a } >>>= f = embed (f a)
  b >>>= _ = inject b

-- Closed terms are traversable on both atoms and terms
instance Traversable termty => Bifunctor (Closed termty) where
  bimap = bimapDefault

instance Traversable termty => Bifoldable (Closed termty) where
  bifoldMap = bifoldMapDefault

instance Traversable termty => Bitraversable (Closed termty) where
  bitraverse atomfunc varfunc =
    let
      traverseClosedTerm BoundInner { innerTerm = inner } =
        BoundInner <$> traverse atomfunc inner
      traverseClosedTerm b @ BoundOuter { outerTerm = outer } =
        (\newouter -> b { outerTerm = newouter }) <$>
        traverse traverseClosedTerm outer
      traverseClosedTerm b @ BoundVar { varAtom = var } =
        (\newvar -> b { varAtom = newvar }) <$> varfunc var
    in
      (Closed <$>) . traverse traverseClosedTerm . closedTerm

instance Traversable termty => Functor (Closed termty atomty) where
  fmap = fmapDefault

instance Traversable termty => Foldable (Closed termty atomty) where
  foldMap = foldMapDefault

instance Traversable termty => Traversable (Closed termty atomty) where
  traverse varfunc =
    let
      traverseClosedTerm BoundInner { innerTerm = inner } =
        BoundInner <$> pure inner
      traverseClosedTerm b @ BoundOuter { outerTerm = outer } =
        (\newouter -> b { outerTerm = newouter }) <$>
          traverse traverseClosedTerm outer
      traverseClosedTerm b @ BoundVar { varAtom = var } =
        (\newvar -> b { varAtom = newvar }) <$> varfunc var
    in
      (Closed <$>) . traverse traverseClosedTerm . closedTerm

instance Traversable termty => Term (Closed termty) where
  retype = retypeDefault
  closed = closedDefault

-- The Applicative and Monad instances don't differentiate between different
-- levels of binding.

instance (Applicative termty, Traversable termty) =>
         Applicative (Closed termty atomty) where
  pure var = Closed { closedTerm = pure BoundVar { varDepth = 0,
                                                   varAtom = var } }

  Closed { closedTerm = fterm } <*> Closed { closedTerm = aterm } =
    let
      -- | Apply a term containing functions to a single atom.
      applyInner _ _ BoundInner { innerTerm = inner } =
        BoundInner { innerTerm = inner }
      applyInner ctxdepth a f @ BoundOuter { outerDepth = depth,
                                             outerTerm = term } =
        f { outerTerm = fmap (applyInner (ctxdepth + toWord depth) a) term }
      applyInner ctxdepth a b @ BoundVar { varDepth = depth, varAtom = f } =
        b { varDepth = ctxdepth + depth, varAtom = f a }

      -- | Applicative instance, with context depths.
      applyOuter _ _ BoundInner { innerTerm = inner } =
        BoundInner { innerTerm = inner }
      applyOuter ctxdepth f a @ BoundOuter { outerDepth = depth,
                                             outerTerm = term } =
        a { outerTerm = fmap (applyOuter (ctxdepth + toWord depth) f) term }
      applyOuter ctxdepth f BoundVar { varDepth = fdepth, varAtom = a } =
        applyInner (ctxdepth + fdepth) a f
    in
      Closed { closedTerm = fmap (applyOuter 0) fterm <*> aterm }

instance (Traversable termty, Monad termty) =>
         Monad (Closed termty atomty) where
  return = pure

  Closed { closedTerm = term } >>= f =
    Closed { closedTerm = term >>= (closedTerm . (>>= f) . Closed . return) }
{-
instance (Embed (termty varty) (termty' varty),
          Embed (termty atomty) (termty' atomty), Functor termty,
          Embed (termty (BoundTerm (termty' atomty)
                                    termty' atomty varty))
                (termty' (BoundTerm (termty' atomty)
                                     termty' atomty varty))) =>
         (Embed (Closed termty atomty varty)
                (termty' (BoundTerm (termty' atomty)
                                    termty' atomty varty))) where
  embed =
    let
      mapfun :: (Embed (termty varty) (termty' varty),
                 Embed (termty atomty) (termty' atomty), Functor termty,
                 Embed (termty (BoundTerm (termty' atomty)
                                          termty' atomty varty))
                 (termty' (BoundTerm (termty' atomty)
                                      termty' atomty varty))) =>
                BoundTerm (termty atomty) termty atomty varty
              -> BoundTerm (termty' atomty) termty' atomty varty
      mapfun BoundInner { innerTerm = inner } =
        BoundInner { innerTerm = embed inner }
      mapfun b @ BoundOuter { outerTerm = term } =
        b { outerTerm = embed (fmap mapfun term) }
      mapfun b @ BoundVar { varAtom = var } = b { varAtom = var }
    in
      embed . fmap mapfun . closedTerm

instance (Embed (termty varty) (termty' varty), Functor termty) =>
         (Retract (Closed termty atomty varty)
                  (termty' (BoundTerm (termty' atomty)
                                      termty' atomty varty))) where
  retract = Just . embed
-}
-- Bound terms are traversable on both atoms and terms
instance Traversable termty => Bifunctor (Bound termty) where
  bimap = bimapDefault

instance Traversable termty => Bifoldable (Bound termty) where
  bifoldMap = bifoldMapDefault

instance Traversable termty => Bitraversable (Bound termty) where
  bitraverse atomfunc varfunc =
    let
      traverseBoundTerm BoundInner { innerTerm = inner } =
        BoundInner <$> bitraverse atomfunc varfunc inner
      traverseBoundTerm b @ BoundOuter { outerTerm = outer } =
        (\newouter -> b { outerTerm = newouter }) <$>
        traverse traverseBoundTerm outer
      traverseBoundTerm b @ BoundVar { varAtom = var } =
        (\newvar -> b { varAtom = newvar }) <$> varfunc var
    in
      (Bound <$>) . traverse traverseBoundTerm . boundTerm

instance Traversable termty => Functor (Bound termty atomty) where
  fmap = fmapDefault

instance Traversable termty => Foldable (Bound termty atomty) where
  foldMap = foldMapDefault

instance Traversable termty => Traversable (Bound termty atomty) where
  traverse varfunc =
    let
      traverseBoundTerm BoundInner { innerTerm = inner } =
        BoundInner <$> traverse varfunc inner
      traverseBoundTerm b @ BoundOuter { outerTerm = outer } =
        (\newouter -> b { outerTerm = newouter }) <$>
          traverse traverseBoundTerm outer
      traverseBoundTerm b @ BoundVar { varAtom = var } =
        (\newvar -> b { varAtom = newvar }) <$> varfunc var
    in
      (Bound <$>) . traverse traverseBoundTerm . boundTerm

instance Traversable termty => Term (Bound termty) where
  retype = retypeDefault
  closed = closedDefault

instance (Embed valty (termty (BoundTerm (termty atomty) termty atomty varty)),
          Subst (termty (BoundTerm (termty atomty) termty atomty varty))
                (BoundTerm (termty atomty) termty atomty varty)
                (termty (BoundTerm (termty atomty) termty atomty varty))
                termty, Inject termty) =>
         Subst valty varty (Closed termty atomty varty)
               (Closed termty atomty) where
  Closed { closedTerm = term } >>>= f =
    let
      substfun :: (Embed valty (termty (BoundTerm (termty atomty)
                                                  termty atomty varty)),
                   Subst (termty (BoundTerm (termty atomty)
                                            termty atomty varty))
                         (BoundTerm (termty atomty) termty atomty varty)
                         (termty (BoundTerm (termty atomty)
                                            termty atomty varty))
                   termty,
                   Inject termty) =>
                  (varty -> valty)
               -> BoundTerm (termty atomty) termty atomty varty
               -> termty (BoundTerm (termty atomty) termty atomty varty)
      substfun _ BoundInner { innerTerm = inner } =
        inject BoundInner { innerTerm = inner }
      substfun f' b @ BoundOuter { outerTerm = term' } =
        inject b { outerTerm = term' >>>= substfun f' }
      substfun f' BoundVar { varDepth = 0, varAtom = a } = embed (f' a)
      substfun _ b @ BoundVar { varAtom = a } = inject b { varAtom = a }
    in
      Closed { closedTerm = term >>>= substfun f }

-- The Applicative and Monad instances don't differentiate between different
-- levels of binding.

instance (Applicative termty, Traversable termty) =>
         Applicative (Bound termty atomty) where
  pure var = Bound { boundTerm = pure BoundVar { varDepth = 0, varAtom = var } }

  Bound { boundTerm = fterm } <*> Bound { boundTerm = aterm } =
    let
      applyOuter _ fbound BoundInner { innerTerm = a } =
        let
          applyInner BoundInner { innerTerm = inner } =
            BoundInner { innerTerm = inner <*> a }
          applyInner b @ BoundOuter { outerTerm = term } =
            b { outerTerm = fmap applyInner term }
          applyInner BoundVar { varAtom = f } =
            BoundInner { innerTerm = fmap f a }
        in
          applyInner fbound
      applyOuter ctxdepth f a @ BoundOuter { outerDepth = depth,
                                             outerTerm = term } =
        a { outerTerm = fmap (applyOuter (ctxdepth + toWord depth) f) term }
      applyOuter ctxdepth fbound BoundVar { varDepth = fdepth, varAtom = a } =
        let
          applyInner _ BoundInner { innerTerm = inner } =
            BoundInner { innerTerm = inner <*> pure a }
          applyInner ctxdepth' f @ BoundOuter { outerDepth = depth,
                                               outerTerm = term } =
            f { outerTerm = fmap (applyInner (ctxdepth' + toWord depth)) term }
          applyInner ctxdepth' b @ BoundVar { varDepth = depth, varAtom = f } =
            b { varDepth = ctxdepth' + depth, varAtom = f a }
        in
          applyInner (ctxdepth + fdepth) fbound
    in
      Bound { boundTerm = fmap (applyOuter 0) fterm <*> aterm }

instance (Traversable termty, Monad termty) =>
         Monad (Bound termty atomty) where
  return = pure

  Bound { boundTerm = term } >>= f =
    Bound { boundTerm = term >>= (boundTerm . (>>= f) . Bound . return) }
