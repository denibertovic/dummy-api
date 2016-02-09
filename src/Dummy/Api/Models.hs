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
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Text.Lazy              as T
import           Database.Persist
import           Database.Persist.Postgresql (SqlBackend (..), runMigration,
                                              runSqlPool)
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
    UniqueEmail email
    deriving Eq Show Generic
  Board
    name T.Text
    deriving Eq Show Generic
  Scroll
    name T.Text
    deriving Eq Show Generic
  BoardScroll
    board BoardId eq
    scroll ScrollId eq
    deriving Eq Show Generic
  ScrollCard
    scroll ScrollId eq
    card CardId eq
    deriving Eq Show Generic
  Card
    title T.Text
    description T.Text
    assignedTo UserId
    deriving Eq Show Generic
|]

instance ToJSON User
instance FromJSON User

instance ToJSON Board
instance FromJSON Board

instance ToJSON Scroll
instance FromJSON Scroll

instance ToJSON Card
instance FromJSON Card

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

-- data Person = Person
--     { name :: String
--     , email :: String
--     } deriving (Eq, Show, Generic)

-- instance ToJSON Person
-- instance FromJSON Person

-- userToPerson :: User -> Person
-- userToPerson User{..} = Person { name = userName, email = userEmail }

userToUpdate :: User -> [Update User]
userToUpdate u = [UserFullName =. (userFullName u), UserEmail =. (userEmail u)]

-- boardToUpdate :: Board -> [Update Board]
-- boardToUpdate b = [BoardName =. (boardName b), BoardScrolls =. (boardScrolls b)]

-- scrollToUpdate :: Scroll -> [Update Scroll]
-- scrollToUpdate s = [ScrollName =. (scrollName s), ScrollCards =. (scrollCards s)]

-- cardToUpdate :: Card -> [Update Card]
-- cardToUpdate c = [CardTitle =. (cardTitle c), CardDescription =. (cardDescription c), CardAssignedTo =. (cardAssignedTo c)]

