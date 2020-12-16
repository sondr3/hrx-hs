{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module: Codec.Archive.HRX
-- Description: Haskell implementation of the HRX file format
-- Copyright: (c) Sondre Nilsen
-- SPDX-License-Identifier: (MIT OR Apache-2.0)
--
-- Maintainer: Sondre Nilsen <sondre.nilsen@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Sate Haskell: Safe
--
-- Reading and writing \"@.hrx@\" archive files.
--
-- == Usage
--
-- To use this library you need to add it as a dependency in your projuct, e.g.
-- by adding it to @build-depends@ in your @.cabal@ file.
--
-- @
-- build-depends: hrx-hs ^>= 0.1.0
-- @
--
-- This module does not use common names and can be directly imported:
--
-- @
-- __import__ Codec.Archive.HRX
-- @
module Codec.Archive.HRX
  ( -- | HRX - Human Readable Archive - is a plain-text, human-readable format for
    -- defining multiple virtual text files in a single physical file.
    --
    -- Here is an example archive containing two files:
    --
    -- @
    -- \<===\> input.scss
    -- ul {
    --   margin-left: 1em;
    --   li {
    --     list-style-type: none;
    --   }
    -- }
    --
    -- \<===\> output.css
    -- ul {
    --   margin-left: 1em;
    -- }
    -- ul li {
    --   list-style-type: none;
    -- }
    -- @
    --
    -- And here is a file containing multiple files, folders and comments. __NOTE:__
    -- this file will be used as the input for all examples in the following sections.
    --
    -- @
    -- \<===\> file
    -- file contents
    -- \<===\> dir/
    -- \<===\>
    -- comment contents
    -- \<===\> super/sub
    -- sub contents
    -- \<===\> very\/deeply\/
    -- \<===\> very\/deeply\/nested\/file
    -- nested contents
    -- \<===\> last
    -- the last file
    -- @
    --
    -- See their [reference](https://github.com/google/hrx) for more information.

    -- * Data structures
    Archive (..),
    Entry (..),
    EntryType (..),

    -- * Functions
    -- $functions

    -- ** Pure

    -- | Functions for getting one or more 'Entry' out of an 'Archive'.
    readEntry,
    findEntry,
    findEntriesGlob,

    -- ** IO

    -- | Functions that read or write to/from the file system.
    readArchive,
    writeArchive,

    -- ** Serialization
    fromHRX,
    fromHRX',
    toHRX,
    toHRX',

    -- * Errors
    ParserError,
  )
where

import Codec.Archive.HRX.Internal
  ( Archive (..),
    Entry (..),
    EntryType (..),
    ParserError,
    findEntriesGlob,
    findEntry,
    fromHRX,
    fromHRX',
    readArchive,
    readEntry,
    toHRX,
    toHRX',
    writeArchive,
  )
