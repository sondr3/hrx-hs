module HRX
  ( -- * Data structures
    Archive (..),
    Entry (..),
    EntryType (..),

    -- * Pure functions for working with HRX archives
    findEntriesGlob,

    -- * IO functions to read/write archives and entries
    readArchive,
    writeArchive,

    -- * Serialization functions to/from text
    fromHRX,
    toHRX,
  )
where

import HRX.Internal
  ( Archive (..),
    Entry (..),
    EntryType (..),
    findEntriesGlob,
    fromHRX,
    readArchive,
    toHRX,
    writeArchive,
  )
