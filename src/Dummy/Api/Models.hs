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
import qualified Data.Aeson                  as JSON

import           Crypto.PasswordStore        (makePassword)
import qualified Data.ByteString             as BS
import           Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Database.Persist
import           Database.Persist.Postgresql (SqlBackend (..), runMigration,
                                              runSqlPool)
import           Database.Persist.Sql        (toSqlKey)
import           Database.Persist.TH         (derivePersistField, mkMigrate,
                                              mkPersist, persistLowerCase,
                                              share, sqlSettings)
import           GHC.Generics                (Generic)
import           Servant.Auth.Server

import           Dummy.Api.Config
import           Dummy.Api.Types

data Login = Login {email :: Email, password :: T.Text}
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

newtype JsonError = JsonError String
instance ToJSON JsonError where
  toJSON (JsonError b) = object ["error" .= b]

data Password = Password !T.Text | PasswordHidden deriving (Eq, Show, Read, Generic)

instance ToJSON Password where
  toJSON (Password p)   = object ["password" .= toJSON PasswordHidden]
  toJSON PasswordHidden = JSON.String "***encrypted***"

instance FromJSON Password where
    parseJSON (JSON.Object o) = do
        p <- o .: "password"
        return $ Password p


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

instance ToJSON User where
    toJSON u = object [ "userFullName" .= userFullName u
                      , "userEmail" .= userEmail u
                      , "userPassword" .= PasswordHidden
                      ]


instance FromJSON User

instance ToJWT User
instance FromJWT User

instance ToJSON Board
instance FromJSON Board

instance ToJSON List
instance FromJSON List

instance ToJSON Card
instance FromJSON Card

insertInitialUsers = do
    janePassword <- liftIO $ flip makePassword 17 $ encodeUtf8 "password"
    johnPassword <- liftIO $ flip makePassword 17 $ encodeUtf8 "password"
    Right jk <- insertBy $ User "John Doe" (Email "john@example.com") (decodeUtf8 johnPassword)
    Right jak <- insertBy $ User "Jane Doe" (Email "jane@example.com") (decodeUtf8 janePassword)
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

