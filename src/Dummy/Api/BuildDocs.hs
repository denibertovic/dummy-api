{-# LANGUAGE OverloadedStrings #-}

module Dummy.Api.BuildDocs where

import           Servant.Docs   hiding (List)

import           Dummy.Api.Api
import           Dummy.Api.Docs

main = putStrLn $ markdown apiDocs

