module Model where

import ClassyPrelude
import Composite.Opaleye (defaultRecTable)
import Composite.Record (Record, (:->)(Val))
import Composite.TH (withLensesAndProxies)
import Control.Arrow (returnA)
import Control.Lens (view)
import Opaleye
  ( Column, PGBytea, PGText, PGTimestamptz, Query, Table(Table), (.==)
  , constant, desc, limit, offset, orderBy, queryTable, restrict )

import RecordOrphans ()
import Types (FName)

withLensesAndProxies [d|
  type CCreated = "created"         :-> Column PGTimestamptz
  type CBucket = "bucket"           :-> Column PGText
  type CKey = "key"                 :-> Column PGText
  type CManifestKey = "manifestKey" :-> Column PGText

  type CName = "name"               :-> Column PGBytea
  type CPassword = "password"       :-> Column PGBytea
  |]

type FileCols = '[CCreated, CBucket, CKey, CManifestKey]
type UserCols = '[CName, CPassword]

fileTable :: Table (Record FileCols) (Record FileCols)
fileTable = Table "files" defaultRecTable

userTable :: Table (Record UserCols) (Record UserCols)
userTable = Table "users" defaultRecTable

-- |Return everything other than the most recent 50 files
selectOldFiles :: Query (Record FileCols)
selectOldFiles =
  orderBy (desc $ view cCreated) . offset 50 $ proc () -> do
    file <- queryTable fileTable -< ()
    returnA -< file

selectUser :: FName -> Query (Record UserCols)
selectUser (Val name) =
  limit 1 $ proc () -> do
    user <- queryTable userTable -< ()
    restrict -< view cName user .== constant name
    returnA -< user
