{-# LANGUAGE
    QuasiQuotes,
    TemplateHaskell,
    TypeFamilies,
    OverloadedStrings,
    GADTs,
    GeneralizedNewtypeDeriving,
    FlexibleContexts #-}

import Data.Text
import Data.Time
import System.Locale (defaultTimeLocale)
import Control.Monad.IO.Class (liftIO)
import Database.Persist as Per
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "tasks"] [persistLowerCase|
Task
    payload Text
    posttourl Text
    attime UTCTime
        deriving (Show)
|]

main = runSqlite ":memory:" $ do
    runMigration tasks
    insert_ $ Task "payload1" "http://yahoo.com" (readTime defaultTimeLocale "%d/%m/%Y" "24/12/1982" :: UTCTime)
    insert_ $ Task "payload2" "http://bb.com" (readTime defaultTimeLocale "%d/%m/%Y" "24/12/1982" :: UTCTime)
    insert_ $ Task "payload3" "http://happyfox.com" (readTime defaultTimeLocale "%d/%m/%Y" "24/12/1982" :: UTCTime)
    insert_ $ Task "payload4" "http://tenmiles.com" (readTime defaultTimeLocale "%d/%m/%Y" "24/12/1982" :: UTCTime)
    (task:_) <- selectList [TaskPayload ==. "payload3"] []
    liftIO $ print $ entityVal task

