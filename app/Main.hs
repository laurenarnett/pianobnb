{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Yesod
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Data.Text
import Data.Aeson
import GHC.Generics
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

data HelloWorld = HelloWorld {postgres :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Piano
    name String
    age Int Maybe
    deriving Show
|]

connStr = "host=localhost dbname=piano user=larnett password=laurel port=5432"

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
/search SearchR POST
/piano PianoR POST
|]

instance Yesod HelloWorld where

instance YesodPersist HelloWorld where
  type YesodPersistBackend HelloWorld = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ postgres master


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  addScriptRemote "https://unpkg.com/htmx.org"
  [whamlet|
<h3>
  Search Contacts
  <span class="htmx-indicator">
     Searching...
<input class="form-control" type="search"
       name="search" placeholder="Begin Typing To Search Users..."
       hx-post=@{SearchR}
       hx-trigger="keyup changed delay:500ms, search"
       hx-target="#search-results"
       hx-indicator=".htmx-indicator">

<table class="table">
    <thead>
    <tr>
      <th>First Name
      <th>Last Name
      <th>Email
    <tbody id="search-results">

<form hx-post="@{PianoR}">
  <input class="form-control" type="text"
       name="make-piano" placeholder="Make a piano">
  <button class="btn btn-default">Submit

!|]

postSearchR :: Handler Html
postSearchR = getPostParams >>= \case
  [("search", search)] -> pure [shamlet|<tr><td>#{search}|]

postPianoR :: Handler Html
postPianoR = getPostParams >>= \case
  [("make-piano", piano)] ->
    do
      entryId <- runDB . insert $ Piano (unpack piano) (Just 10)
      pure [shamlet|<tr><td>#{(show entryId)}|]

getPage1R = defaultLayout [whamlet|<p> You are currently on page 1
                                   <a href=@{Page2R}>Go to page 2!|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go home!|]

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        pianoId <- insert $ Piano "piano1" (Just 1)
        onePianoPost <- selectList [PianoId ==. pianoId] [LimitTo 1]
        liftIO $ print onePianoPost
    liftIO $ warp 3000 (HelloWorld pool) -- this must be here
