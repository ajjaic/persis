{-# LANGUAGE
    QuasiQuotes,
    TemplateHaskell,
    TypeFamilies,
    OverloadedStrings,
    GADTs,
    GeneralizedNewtypeDeriving,
    FlexibleContexts #-}

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Tutorial
    title   Text
    url     Text
    school  Bool
    deriving Show
|]

main = return ()
