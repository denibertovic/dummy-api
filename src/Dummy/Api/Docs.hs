{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dummy.Api.Docs where

-- import           Control.Monad.Reader (ReaderT, lift, runReaderT)
-- import           Data.Int             (Int64)
-- import           Data.Proxy
-- import           Database.Persist.Sql (toSqlKey)
-- import           Network.HTTP.Types
-- import           Network.Wai
-- import           Servant.API
-- import           Servant.Docs         hiding (List)
-- import           Servant.Server

-- import           Dummy.Api.Config
-- import           Dummy.Api.Models

-- instance ToSample () where
--   toSamples _ = singleSample ()

-- instance ToCapture (Capture "id" Int64) where
--   toCapture _ =
--     DocCapture "id"                                -- name
--                "(integer) ID of the Resource"      -- description

-- john = User "John Doe" "john.doe@example.com" "password"

-- board = Board "Sample Board" (toSqlKey 1)
-- list = List "Sample List" (toSqlKey 1)
-- card = Card "Sample title" "Sample description" (toSqlKey 1) (toSqlKey 1)

-- instance ToSample User where
--   toSamples _ = singleSample john

-- instance ToSample Board where
--   toSamples _ = singleSample board

-- instance ToSample List where
--   toSamples _ = singleSample list

-- instance ToSample Card where
--   toSamples _ = singleSample card

