{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dummy.Api.Docs where

import           Control.Monad.Reader    (ReaderT, lift, runReaderT)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Int                (Int64)
import           Data.Proxy
import           Data.Text.Lazy          (pack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Database.Persist.Sql    (toSqlKey)
import           Network.HTTP.Types
import           Network.Wai
import           Servant.API
import           Servant.Docs
import           Servant.Server

import           Dummy.Api.Api
import           Dummy.Api.Config
import           Dummy.Api.Models

instance ToCapture (Capture "id" Int64) where
  toCapture _ =
    DocCapture "id"                                -- name
               "(integer) ID of the Resource"      -- description

instance ToSample () () where
  toSample _ = Nothing -- example of output

john = User "John Doe" "john.doe@example.com"
jane = User "Jane Doe" "jane.doe@example.com"

board = Board "Sample Board"
scroll = Scroll "Sample Scroll" (toSqlKey 1)
card = Card "Sample title" "Sample description" (toSqlKey 1) (toSqlKey 1)


instance ToSample [User] [User] where
  toSample _ = Just [john, jane]

instance ToSample User User where
  toSample _ = Just john

instance ToSample [Board] [Board] where
  toSample _ = Just [board]

instance ToSample Board Board where
  toSample _ = Just (Board "Board Name") -- example of output

instance ToSample [Scroll] [Scroll] where
  toSample _ = Just [scroll]

instance ToSample Scroll Scroll where
  toSample _ = Just scroll

instance ToSample [Card] [Card] where
  toSample _ = Just [card]

instance ToSample Card Card where
  toSample _ = Just card

apiDocs :: API
apiDocs = docs dummyAPI

main = putStrLn $ markdown apiDocs

