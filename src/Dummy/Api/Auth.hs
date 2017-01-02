{-# LANGUAGE OverloadedStrings #-}

module Dummy.Api.Auth where

import           Crypto.PasswordStore (makePassword)
import           Data.Aeson           (FromJSON, ToJSON, object, parseJSON,
                                       toJSON, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString      as BS
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)

data Password = PasswordHash T.Text | PasswordHidden deriving (Eq, Show, Read)

instance ToJSON Password where
    toJSON (PasswordHash p) = toJSON PasswordHidden
    toJSON PasswordHidden   = "__hashed__"

hashPassword :: T.Text -> IO BS.ByteString
hashPassword p = makePassword (encodeUtf8 p) 17

-- | Strip the password from the user type.
-- hidePassword :: User -> User
-- hidePassword user =
--   user { userPassword = PasswordHidden  }

