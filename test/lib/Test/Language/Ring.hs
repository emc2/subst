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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, UndecidableInstances #-}

module Test.Language.Ring(
       Operator(..),
       Ring(..),
       Literal(..),
       Nameless
       ) where

import Control.Monad
import Data.Traversable
import Subst
import Subst.Abstract.Class
import Subst.Embed.Class
import Subst.Free
import Subst.Retract.Class

data Operator = Add | Mult

data Ring atomty =
    Binop {
      binopOp :: !Operator,
      binopLeft :: Ring atomty,
      binopRight :: Ring atomty
    }
  | Neg { negInner :: Ring atomty }
  | Atom { atom :: !atomty }

-- | Integers as literals
newtype Literal = Literal { literalVal :: Int }

type Nameless = Ring Literal

instance Functor Ring where
  fmap = fmapDefault

instance Foldable Ring where
  foldMap = foldMapDefault

instance Traversable Ring where
  traverse f b @ Binop { binopLeft = left, binopRight = right } =
    (\newleft newright -> b { binopLeft = newleft, binopRight = newright }) <$>
      traverse f left <*> traverse f right
  traverse f n @ Neg { negInner = inner } =
    (\newinner -> n { negInner = newinner }) <$> traverse f inner
  traverse f a @ Atom { atom = val } =
    (\newval -> a { atom = newval }) <$> f val

instance Applicative Ring where
  pure = return
  (<*>) = ap

instance Monad Ring where
  return = inject
  (>>=) = (>>>=)

instance Inject Ring where
  inject = Atom

instance (Embed valty resty, Embed (Ring valty) resty) =>
         Subst valty varty resty Ring where
  (>>>=) = substFunctor

instance Retract atomty (Ring atomty) where
  retract = Just . Atom

instance Embed atomty (Ring atomty) where
  embed = Atom

instance Embed (Ring (Ring atomty)) (Ring atomty) where
  embed b @ Binop { binopLeft = left, binopRight = right } =
    b { binopLeft = embed left, binopRight = embed right }
  embed n @ Neg { negInner = inner } = n { negInner = embed inner }
  embed Atom { atom = val } = val

instance Retract (Ring atomty) atomty where
  retract Atom { atom = out } = Just out
  retract _ = Nothing
