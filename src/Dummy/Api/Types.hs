{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dummy.Api.Types where

import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Text           as T
import           Database.Persist.TH (derivePersistField)
import           GHC.Generics        (Generic)


newtype Email = Email T.Text deriving (Eq, Show, Read, Generic)
derivePersistField "Email"

instance FromJSON Email
instance ToJSON Email


