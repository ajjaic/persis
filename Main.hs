{-# LANGUAGE
    QuasiQuotes,
    TemplateHaskell,
    TypeFamilies,
    OverloadedStrings,
    GADTs,
    GeneralizedNewtypeDeriving,
    FlexibleContexts #-}

import Control.Monad.IO.Class (liftIO)
import Database.Persist as Per
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH
import Data.Maybe (fromJust)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Person
    name String
    age Int Maybe
        deriving (Show)
BlogPost
    title String
    authorId PersonId
        deriving (Show)
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateTables
    johnid <- insert $ Person "John Doe" (Just 30)
    janeid <- insert $ Person "Jane bab" Nothing
    insert_ $ Person "Blade raj" (Just 27)
    insert $ BlogPost "Yea daddy" johnid
    insert $ BlogPost "Bye man" janeid
    johnpost <- selectList [BlogPostAuthorId ==. johnid] [LimitTo 1]
    bladeraj <- selectFirst [Filter PersonName (Right ["Blade raj"]) Per.Eq] []
    liftIO $ print $ entityVal $ fromJust bladeraj
    john <- get johnid
    liftIO $ print (john :: Maybe Person)
    delete janeid
    deleteWhere [BlogPostAuthorId ==. johnid]

