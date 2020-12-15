module HRX
  ( -- * Data structures
    Archive (..),
    Entry (..),
    EntryType (..),
    Path (..),

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
    Path (..),
    fromHRX,
    readArchive,
    toHRX,
    writeArchive,
  )
