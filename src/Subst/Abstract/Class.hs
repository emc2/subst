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
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies  #-}

module Subst.Abstract.Class(
       Abstract(..)
       ) where

class Abstract atomty varty termty absty | absty -> atomty, absty -> varty where
  abstract :: (atomty -> Maybe varty)
           -> termty
           -> absty

  -- | Convert a term to the abstract form without doing any actual
  -- substitutions.  This is equivalent to @abstract (const Nothing)@,
  -- though specific instances may have more efficient
  -- implementations.
  abstract_ :: termty
            -- ^ The concrete term to abstract.
            -> absty
            -- ^ The concrete term abstracted, with no substitutions
            -- performed.
  abstract_ = abstract (const Nothing)

{-
  instantiate :: (varty -> Maybe atomty)
              -- ^ The instantiation function.
              -> absty
              -- ^ The abstract term to instantiate.
              -> Either termty absty

  -- | Instantiate some or possibly all of the bindings in an abstract
  -- term, but remain in the abstract form even if all bindings are
  -- instantiated.
  curry :: (varty -> Maybe atomty)
        -- ^ The instantiation function.
        -> absty
        -> absty
  curry f = either abstract_ id . instantiate f

  unabstract :: (varty -> Maybe atomty)
             -- ^ The instantiation function.
             -> absty
             -> Maybe termty
             -- ^ A concrete term, or 'Nothing'.
  unabstract f = either Just (const Nothing) . instantiate f
-}
