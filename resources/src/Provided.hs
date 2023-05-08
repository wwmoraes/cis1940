{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : Provided
-- Description : Pre-defined resources to help with the assignments
-- Copyright   : (c) Brent Yorgey, 2013
--                   William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Unmodified (functionally-speaking) copy of the resources given by the CIS 194
-- lecture instructor, [Brent Yorgey](http://www.cis.upenn.edu/~byorgey/), to
-- help with the homework assignments.
--
-- Changes done compared to the original material:
--
--   * added documentation to each module
--   * added the 'Provided' module prefix
--   * updated imports between modules to reflect the prefix
--   * made 'Size' an instance of 'Semigroup' as it is a superclass of 'Monoid'
-- since [/base-4.11.0.0/](https://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#t:Monoid)
--   * applied multiple lint suggestions
--
-- The lint changes are:
--
--   * re-formatted the file with recent indentation best practices
--   * refactored imports from module-wide to fully-explicit
--   * removed unused pattern matches
--   * renamed local functions that shadowed base ones
--
-- Text files are not part of the library sources, and are either on one of the
-- binary source trees or the tests, depending on the use case.
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module Provided
  ( -- * Homework 2
    module Provided.Log,

    -- * Homework 4
    module Provided.Wholemeal,

    -- * Homework 5
    module Provided.ExprT,
    module Provided.Parser,
    module Provided.StackVM,

    -- * Homework 7
    -- | /StringBufEditor/ is not included here as it is a Main module sample
    module Provided.Buffer,
    module Provided.Editor,
    module Provided.Sized,
    module Provided.StringBuffer,
    module Provided.JoinList,
    -- ^ contains the snippets provided within the homework assignment

    -- * Homework 8
    module Provided.Employee,

    -- * Homework 10
    module Provided.AParser,

    -- * Homework 11
    module Provided.AParser2,

    -- * Homework 12
    module Provided.Risk,
  )
where

import qualified Provided.AParser
import qualified Provided.AParser2
import qualified Provided.Buffer
import qualified Provided.Editor
import qualified Provided.Employee
import qualified Provided.ExprT
import qualified Provided.JoinList
import qualified Provided.Log
import qualified Provided.Parser
import qualified Provided.Risk
import qualified Provided.Sized
import qualified Provided.StackVM
import qualified Provided.StringBuffer
import qualified Provided.Wholemeal
