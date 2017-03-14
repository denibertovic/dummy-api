{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Dummy.Api.Api where

import           Control.Monad.Except        (ExceptT, throwError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, lift, runReaderT)
import           Crypto.PasswordStore        (makePassword)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Int                    (Int64)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy              (pack)
import           Data.Time.Clock             (addUTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              rawSql, selectList, (<-.), (==.))
import           Database.Persist.Sql
import           Dummy.Api.Config            (Config (..))
import           Dummy.Api.Docs
import           Dummy.Api.Models
import           GHC.Generics                (Generic)
import           Network.HTTP.Types          (ok200)
import           Network.Wai                 (Application)
import           Network.Wai                 (responseLBS)

import           Servant
import           Servant.API
import           Servant.Auth.Server
-- import           Servant.Docs                hiding (List)
import           Servant.Server

import           Data.Aeson                  (encode)

jsonError e = encode $ JsonError e

data AuthUser = AuthUser { uid :: Int64, user :: User } deriving (Eq, Show, Generic)

data JsonToken = JsonToken { token :: T.Text } deriving (Eq, Show, Generic)

instance ToJSON JsonToken
instance FromJSON JsonToken

instance ToJSON AuthUser
instance FromJSON AuthUser

instance ToJWT AuthUser
instance FromJWT AuthUser

-- apiDocs :: API
-- apiDocs = docs dummyAPI

-- docsBS :: BS.ByteString
-- docsBS = encodeUtf8
--        . pack
--        . markdown
--        $ docsWithIntros [intro] dummyAPI
--   where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

type UnprotectedAPI = "login" :> ReqBody '[JSON] Login
                      :> Post '[JSON] JsonToken
    :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] Int64


type UserAPI = Get '[JSON] [User]
    -- :<|> Get '[JSON] User
    :<|> DeleteNoContent '[JSON] NoContent
    :<|> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> ReqBody '[JSON] Password :> Post '[JSON] NoContent

-- type BoardAPI = Get '[JSON] [Board]
--     :<|> Capture "id" Int64 :> "lists" :> Get '[JSON] [List]
--     :<|> Capture "id" Int64 :> "cards" :> Get '[JSON] [Card]
--     :<|> Capture "id" Int64 :> Get '[JSON] Board
--     :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
--     :<|> ReqBody '[JSON] Board :> Post '[JSON] Board
--     :<|> Capture "id" Int64
--          :> ReqBody '[JSON] Board
--          :> Post '[JSON] Board

-- type ListAPI = Get '[JSON] [List]
--     :<|> Capture "id" Int64 :> "cards" :> Get '[JSON] [Card]
--     :<|> Capture "id" Int64 :> Get '[JSON] List
--     :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
--     :<|> ReqBody '[JSON] List :> Post '[JSON] List
--     :<|> Capture "id" Int64
--          :> ReqBody '[JSON] List
--          :> Post '[JSON] List

-- type CardAPI = Get '[JSON] [Card]
--     :<|> Capture "id" Int64 :> Get '[JSON] Card
--     :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
--     :<|> ReqBody '[JSON] Card :> Post '[JSON] Card
--     :<|> Capture "id" Int64
--                  :> ReqBody '[JSON] Card
--                  :> Post '[JSON] Card

type DummyAPI auths = (Auth auths AuthUser :> "users" :> UserAPI)
  :<|> UnprotectedAPI
  -- :<|> "boards" :> BoardAPI
  -- :<|> "lists" :> ListAPI
  -- :<|> "cards" :> CardAPI

-- type DocsAPI = DummyAPI :<|> Raw

type AppM = ReaderT Config (ExceptT ServantErr IO)

-- Handlers

userServer :: AuthResult AuthUser -> ServerT UserAPI AppM
userServer au = listUsers au
        -- :<|> getUser
        :<|> deleteUser au
        :<|> updateUser au
        :<|> updateUserPassword au

-- boardServer :: AuthResult AuthUser -> ServerT BoardAPI AppM
-- boardServer au = listBoards
--         :<|> getBoardLists au
--         :<|> getBoardCards au
--         :<|> getBoard au
--         :<|> deleteBoard au
--         :<|> createBoard au
--         :<|> updateBoard au

-- listServer :: AuthResult AuthUser -> ServerT ListAPI AppM
-- listServer au = listLists au
--         :<|> getListCards au
--         :<|> getList au
--         :<|> deleteList au
--         :<|> createList au
--         :<|> updateList au

-- cardServer :: AuthResult AuthUser -> ServerT CardAPI AppM
-- cardServer au = listCards au
--         :<|> getCard au
--         :<|> deleteCard au
--         :<|> createCard au
--         :<|> updateCard au

unprotectedServer :: CookieSettings -> JWTSettings -> ServerT UnprotectedAPI AppM
unprotectedServer cs jwts = loginUser cs jwts :<|> registerNewUser cs jwts

dummyServer :: CookieSettings -> JWTSettings -> ServerT (DummyAPI auths) AppM
dummyServer cs jwts = userServer :<|> unprotectedServer cs jwts

-- docsServer _ respond = respond $ responseLBS ok200 [plain] docsBS
--   where plain = ("Content-Type", "text/plain")

readerToHandler :: Config -> AppM :~> ExceptT ServantErr IO
readerToHandler cfg = Nat $ \x -> runReaderT x cfg

readerToServer :: Config -> CookieSettings -> JWTSettings -> Server (DummyAPI auths)
readerToServer cfg cC jwtC = enter (readerToHandler cfg) (dummyServer cC jwtC)
                        -- :<|> docsServer

dummyAPI :: Proxy (DummyAPI '[JWT])
dummyAPI = Proxy

-- docsAPI :: Proxy (DocsAPI '[JWT])
-- docsAPI = Proxy

app :: Config -> CookieSettings -> JWTSettings -> Application
app cfg cC jwtC = serveWithContext dummyAPI ctx' (readerToServer cfg cC jwtC)
  where ctx' = cC :. jwtC :. EmptyContext


-- User controllers

listUsers :: AuthResult AuthUser -> AppM [User]
listUsers (Authenticated au) = do
    users :: [Entity User] <- runDb $ selectList [] []
    let us = map (\(Entity _ y) -> y) users
    return us
listUsers _ = throwError err401

-- getUser :: Int64 -> AppM User
-- getUser uid = do
--     user <- runDb $ get (toSqlKey uid)
--     case user of
--         Nothing -> throwError err404
--         Just u  -> return u

-- Here is the login handler
loginUser :: CookieSettings -> JWTSettings -> Login
  -> AppM JsonToken -- (Headers '[Header "Set-Cookie" SetCookie] NoContent))
loginUser cookieSettings jwtSettings (Login e p) = do
   -- Usually you would ask a database for the user info. This is just a
   -- regular servant handler, so you can follow your normal database access
   -- patterns (including using 'enter').
   user <- runDb $ getBy $ UniqueEmail e
   -- TODO: verify password here
   case user of
      Nothing -> throwError err400
      Just (Entity i u) -> do
        let usr = AuthUser {uid = (fromSqlKey i), user=u}
        -- mcookie <- liftIO $ makeCookie cookieSettings jwtSettings usr
        -- case mcookie of
        --   Nothing     -> throwError err401
        --   Just cookie -> return $ addHeader cookie NoContent
        currentTime <- liftIO getCurrentTime
        -- expires in 24 hours from when it was issued
        let expiresIn = addUTCTime 86400 currentTime
        token <- liftIO $ makeJWT usr jwtSettings (Just expiresIn)
        liftIO $ print token
        case token of
          Left e  -> throwError err401
          Right t -> return $ JsonToken { token = decodeUtf8 . BSL.toStrict $ t }
loginUser _ _ _ = throwError err401

registerNewUser :: CookieSettings -> JWTSettings -> User -> AppM Int64
registerNewUser cs jwts user = do
    existing <- runDb $ getBy $ UniqueEmail $ userEmail user
    case existing of
      Nothing -> do
        hpass <- liftIO $ flip makePassword 17 . encodeUtf8 . userPassword $ user
        let u = user {userPassword = (decodeUtf8 hpass)}
        newUserId <- runDb $ insert u
        return $ fromSqlKey newUserId
      Just u -> throwError (err400 { errBody = jsonError "Email already exists."})

updateUser :: AuthResult AuthUser -> User ->  AppM User
updateUser (Authenticated au) u = do
    runDb $ update (toSqlKey $ uid au) $ userToUpdate u
    return u
updateUser _ _  = throwError err401

updateUserPassword :: AuthResult AuthUser -> Password -> AppM NoContent
updateUserPassword (Authenticated au) (Password p) = do
    pass <- liftIO $ flip makePassword 17 $ encodeUtf8 p
    -- FIXME: decodeUtf8 throws exceptions. Switch to safer version
    runDb $ update (toSqlKey $ uid au) $ [UserPassword =. decodeUtf8 pass]
    return NoContent
updateUserPassword _ _  = throwError err401

deleteUser :: AuthResult AuthUser -> AppM NoContent
deleteUser (Authenticated au) = do
    runDb $ delete ((toSqlKey $ uid au) :: Key User)
    return NoContent
deleteUser _  = throwError err401

-- Board controllers

-- listBoards :: AppM [Board]
-- listBoards = do
--         boards :: [Entity Board] <- runDb $ selectList [] []
--         let bs = map (\(Entity _ y) -> y) boards
--         return bs

-- getBoardLists :: Int64 -> AppM [List]
-- getBoardLists bid = do
--         lists <- runDb $ selectList [ListBoardId ==. (toSqlKey bid)] []
--         let cs = map (\(Entity _ y) -> y) lists
--         return cs

-- getBoardCards :: Int64 -> AppM [Card]
-- getBoardCards bid = do
--         lists <- runDb $ selectList [ListBoardId ==. (toSqlKey bid)] []
--         let cs = map (\(Entity x _) -> x) lists
--         cards <- runDb $ selectList [CardListId <-. cs] []
--         let cc = map (\(Entity _ y) -> y) cards
--         return cc

-- getBoard :: Int64 -> AppM Board
-- getBoard bid = do
--         board <- runDb $ get (toSqlKey bid)
--         case board of
--             Nothing -> throwError err404
--             Just b  -> return b

-- createBoard :: Board -> AppM Board
-- createBoard b = do
--         newBoardId <- runDb $ insert b
--         return b

-- updateBoard :: Int64 -> Board -> AppM Board
-- updateBoard i b = do
--         board <- runDb $ get ((toSqlKey i) :: Key Board)
--         case board of
--             Nothing -> throwError err404
--             Just _ -> do
--                 runDb $ update (toSqlKey i) $ boardToUpdate b
--                 return b

-- deleteBoard :: Int64 -> AppM NoContent
-- deleteBoard bid = do
--     runDb $ delete ((toSqlKey bid) :: Key Board)
--     return NoContent


-- -- List controllers

-- listLists :: AppM [List]
-- listLists = do
--         lists :: [Entity List] <- runDb $ selectList [] []
--         let cs = map (\(Entity _ y) -> y) lists
--         return cs

-- getListCards :: Int64 -> AppM [Card]
-- getListCards lid = do
--         cards <- runDb $ selectList [CardListId ==. (toSqlKey lid)] []
--         let cs = map (\(Entity _ y) -> y) cards
--         return cs

-- getList :: Int64 -> AppM List
-- getList lid = do
--         list <- runDb $ get (toSqlKey lid)
--         case list of
--             Nothing -> throwError err404
--             Just l  -> return l

-- createList :: List -> AppM List
-- createList l = do
--         newListId <- runDb $ insert l
--         return l

-- updateList :: Int64 -> List -> AppM List
-- updateList i l = do
--         list <- runDb $ get ((toSqlKey i) :: Key List)
--         case list of
--             Nothing -> throwError err404
--             Just _ -> do
--                 runDb $ update (toSqlKey i) $ listToUpdate l
--                 return l

-- deleteList :: Int64 -> AppM NoContent
-- deleteList lid = do
--     runDb $ delete ((toSqlKey lid) :: Key List)
--     return NoContent


-- -- Card controllers

-- listCards :: AppM [Card]
-- listCards = do
--         cards :: [Entity Card] <- runDb $ selectList [] []
--         let cs = map (\(Entity _ y) -> y) cards
--         return cs

-- getCard :: Int64 -> AppM Card
-- getCard cid = do
--         card <- runDb $ get (toSqlKey cid)
--         case card of
--             Nothing -> throwError err404
--             Just c  -> return c

-- createCard :: Card -> AppM Card
-- createCard c = do
--         newCardId <- runDb $ insert c
--         return c

-- updateCard :: Int64 -> Card -> AppM Card
-- updateCard i c = do
--         card <- runDb $ get ((toSqlKey i) :: Key Card)
--         case card of
--             Nothing -> throwError err404
--             Just _ -> do
--                 runDb $ update (toSqlKey i) $ cardToUpdate c
--                 return c

-- deleteCard :: Int64 -> AppM NoContent
-- deleteCard cid = do
--     runDb $ delete ((toSqlKey cid) :: Key Card)
--     return NoContent

