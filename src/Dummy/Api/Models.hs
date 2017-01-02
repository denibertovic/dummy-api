{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Dummy.Api.Models where

import           Control.Monad.Reader        (ReaderT, asks, liftIO)
import           Data.Aeson                  (FromJSON, ToJSON,
                                              genericParseJSON, genericToJSON,
                                              object, parseJSON, toJSON, (.!=),
                                              (.:), (.:?), (.=))
import qualified Data.ByteString             as BS
import           Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Database.Persist
import           Database.Persist.Postgresql (SqlBackend (..), runMigration,
                                              runSqlPool)
import           Database.Persist.Sql        (toSqlKey)
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                              persistLowerCase, share,
                                              sqlSettings)
import           GHC.Generics                (Generic)

import           Dummy.Api.Config

type Email = T.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    fullName T.Text
    email Email
    password T.Text
    UniqueEmail email
    deriving Eq Show Generic
  Board
    name T.Text
    ownerId UserId eq
    BoardUniqueness name ownerId
    deriving Eq Show Generic
  List
    name T.Text
    boardId BoardId eq
    deriving Eq Show Generic
  Card
    title T.Text
    description T.Text
    assignedTo UserId eq
    listId ListId eq
    deriving Eq Show Generic
|]

instance ToJSON User
instance FromJSON User

instance ToJSON Board
instance FromJSON Board

instance ToJSON List
instance FromJSON List

instance ToJSON Card
instance FromJSON Card

insertInitialUsers = do
    Right jk <- insertBy $ User "John Doe" "john@example.com" "password"
    Right jak <- insertBy $ User "Jane Doe" "jane@example.com" "password"
    return (jk,  jak)

inserInitialBoard iid = do
    _ <- insertBy $ Board "Default Board" iid
    return ()

doDataMigrations :: ReaderT SqlBackend IO ()
doDataMigrations = do
    (jk, jak) <- insertInitialUsers
    _ <- inserInitialBoard jk
    return ()

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

userToUpdate :: User -> [Update User]
userToUpdate u = [UserFullName =. userFullName u,
                  UserEmail =. userEmail u]

boardToUpdate :: Board -> [Update Board]
boardToUpdate b = [BoardName =. boardName b]

listToUpdate :: List -> [Update List]
listToUpdate s = [ListName =. listName s,
                  ListBoardId =. listBoardId s]

cardToUpdate :: Card -> [Update Card]
cardToUpdate c = [CardTitle =. cardTitle c,
                  CardDescription =. cardDescription c,
                  CardAssignedTo =. cardAssignedTo c,
                  CardListId =. cardListId c]

