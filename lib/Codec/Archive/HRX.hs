module Codec.Archive.HRX
  ( -- * Data structures
    Archive (..),
    Entry (..),
    EntryType (..),

    -- * Pure functions for working with HRX archives
    readEntry,
    findEntry,
    findEntriesGlob,

    -- * IO functions to read/write archives and entries
    readArchive,
    writeArchive,

    -- * Serialization functions to/from text
    fromHRX,
    toHRX,
    toHRX',
  )
where

import Codec.Archive.HRX.Internal
  ( Archive (..),
    Entry (..),
    EntryType (..),
    findEntriesGlob,
    findEntry,
    fromHRX,
    readArchive,
    readEntry,
    toHRX,
    toHRX',
    writeArchive,
  )
