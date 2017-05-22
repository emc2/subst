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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Subst.Class(
       Subst(..),
       substDefault
       ) where

import Subst.Embed.Class
import Subst.Retract.Class

-- | Class of terms supporting hereditary substitutions (that is, a
-- substitution from varty to valty can be applied to a wholly
-- different term type).
class Subst varty valty termty where
  -- | Hereditary substitution of some of the variables in a term.
  subst :: (varty -> Maybe valty)
        -- ^ The substitution function.
        -> termty
        -- ^ The term into which to perform the substitution.
        -> termty
        -- ^ The resulting term.

  -- | Hereditary substitution of all of the variables in a term.
  (>>>=) :: (varty -> valty)
         -- ^ The substitution function.
         -> termty
         -- ^ The term into which to perform the substitution.
         -> termty
         -- ^ The resulting term.
  f >>>= term = subst (Just . f) term

-- | Default implementation for 'subst' suitable for many
-- applications.  This depends on a 'Monad' instance on the term
-- constructor, a partial bijection between the variable and atom
-- types, and an embedding of the substitution result into the term
-- type.
substDefault :: (Monad termty, Embed varty atomty, Retract atomty varty,
                 Embed valty (termty atomty)) =>
                (varty -> Maybe valty)
             -- ^ The substitution function.
             -> termty atomty
             -- ^ The term into which to perform the substitution.
             -> termty atomty
             -- ^ The resulting term.
substDefault f =
  let
    bindfunc atom =
      case retract atom of
        Just var -> maybe (return (embed var)) embed (f var)
        Nothing -> return atom
  in
    (>>= bindfunc)
