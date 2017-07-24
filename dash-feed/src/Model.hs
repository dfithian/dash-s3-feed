module Model where

import ClassyPrelude
import Composite.Opaleye (defaultRecTable)
import Composite.Record (Record, (:->))
import Composite.TH (withLensesAndProxies)
import Control.Arrow (returnA)
import Control.Lens (view)
import Opaleye
  ( Column, PGText, PGTimestamptz, Query, Table(Table)
  , desc, offset, orderBy, queryTable )

import RecordOrphans ()

withLensesAndProxies [d|
  type CCreated = "created"         :-> Column PGTimestamptz
  type CBucket = "bucket"           :-> Column PGText
  type CKey = "key"                 :-> Column PGText
  type CManifestKey = "manifestKey" :-> Column PGText
  |]

type FileCols = '[CCreated, CBucket, CKey, CManifestKey]

fileTable :: Table (Record FileCols) (Record FileCols)
fileTable = Table "files" defaultRecTable

-- |Return everything other than the most recent 50 files
selectOldFiles :: Query (Record FileCols)
selectOldFiles =
  orderBy (desc $ view cCreated) . offset 50 $ proc () -> do
    file <- queryTable fileTable -< ()
    returnA -< file
