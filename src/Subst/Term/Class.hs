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

module Subst.Term.Class(
       -- * Term Typeclass
       Term(..),

       -- * Default Implementations
       retypeDefault,
       closedDefault
       ) where

import Data.Bifoldable
import Data.Bitraversable

-- | Typeclass defining operations on terms.
class Term termty where
  -- | Transform the variable type of a closed term.
  retype :: termty varty atomty
         -- ^ Term to transform.
         -> Maybe (termty varty' atomty)
         -- ^ Transformed term, or 'Nothing' if the original term
         -- was not closed.

  -- | Check if a term is closed (meaning, whether it actually
  -- contains any variables).
  closed :: termty varty atomty
         -- ^ The free term.
         -> Bool

-- | Implementation of 'retype' when a 'Bitraversable' instance is available.
retypeDefault :: Bitraversable termty =>
                 termty varty atomty
              -- ^ Term to transform.
              -> Maybe (termty varty' atomty)
              -- ^ Transformed term, or 'Nothing' if the original term
              -- was not closed.
retypeDefault = bimapM (const Nothing) Just

-- | Implementation of 'closed' when a 'Bifoldable' instance is available.
closedDefault :: Bifoldable termty =>
                 termty varty atomty
              -- ^ The free term.
              -> Bool
closedDefault = bifoldr (const (False &&)) (const id) True
