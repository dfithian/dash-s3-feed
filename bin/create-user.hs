import ClassyPrelude
import Composite.Record ((:->)(Val), pattern (:*:), Rec(RNil), Record)
import Crypto.KDF.BCrypt (hashPassword)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Opaleye (constant, runInsertMany, runQuery)
import Options.Applicative (execParser, header, helper, info, help, long, progDesc, strOption)

import Model (UserCols, userTable, selectUser)
import Types (User)

data Opts = Opts
  { optsUsername       :: ByteString
  , optsPassword       :: ByteString
  , optsPostgresConfig :: ByteString
  }

parseArgs :: IO Opts
parseArgs = execParser $ info (parser <**> helper) mods
  where
    bsOption = map (encodeUtf8 . pack) . strOption
    parser = Opts
      <$> (bsOption $ long "username" <> help "Username to insert")
      <*> (bsOption $ long "password" <> help "Password to insert")
      <*> (bsOption $ long "conn-string" <> help "Postgres connection string")
    mods = header "Create user" <> progDesc "Insert a user with a salted password into Postgres"

main :: IO ()
main = do
  Opts {..} <- parseArgs
  bracket (connectPostgreSQL optsPostgresConfig) close $ \ conn -> do
    users <- runQuery conn . selectUser $ Val optsUsername
    let _ = users :: [Record User]
    case headMay users of
      Just _ -> putStrLn "User already exists"
      Nothing -> do
        hashedPassword <- hashPassword 4 optsPassword
        void . runInsertMany conn userTable . singleton
          . (constant :: Record User -> Record UserCols)
          $ optsUsername :*: hashedPassword :*: RNil
