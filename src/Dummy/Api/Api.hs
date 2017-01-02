{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Dummy.Api.Api where

import           Control.Monad.Except             (ExceptT, throwError)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader             (ReaderT, lift, runReaderT)
import qualified Data.ByteString.Lazy             as BS
import           Data.Int                         (Int64)
import qualified Data.Text                        as T
import           Data.Text.Lazy                   (pack)
import           Data.Text.Lazy.Encoding          (encodeUtf8)
import           Database.Persist
import           Database.Persist.Postgresql      (Entity (..), fromSqlKey,
                                                   insert, rawSql, selectList,
                                                   (<-.), (==.))
import           Database.Persist.Sql
import           Network.HTTP.Types               (ok200)
import           Network.Wai                      (Application)
import           Network.Wai                      (responseLBS)
import           Servant
import           Servant.API
import           Servant.Docs                     hiding (List)
import           Servant.Server
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Servant.Server.Experimental.Auth ()

import           Dummy.Api.Config                 (Config (..))
import           Dummy.Api.Docs
import           Dummy.Api.Models


apiDocs :: API
apiDocs = docs dummyAPI

docsBS :: BS.ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] dummyAPI
  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

type UserAPI = Get '[JSON] [User]
    :<|> Capture "id" Int64 :> Get '[JSON] User
    :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
    :<|> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] User
         :> Post '[JSON] User

type BoardAPI = Get '[JSON] [Board]
    :<|> Capture "id" Int64 :> "lists" :> Get '[JSON] [List]
    :<|> Capture "id" Int64 :> "cards" :> Get '[JSON] [Card]
    :<|> Capture "id" Int64 :> Get '[JSON] Board
    :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
    :<|> ReqBody '[JSON] Board :> Post '[JSON] Board
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] Board
         :> Post '[JSON] Board

type ListAPI = Get '[JSON] [List]
    :<|> Capture "id" Int64 :> "cards" :> Get '[JSON] [Card]
    :<|> Capture "id" Int64 :> Get '[JSON] List
    :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
    :<|> ReqBody '[JSON] List :> Post '[JSON] List
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] List
         :> Post '[JSON] List

type CardAPI = Get '[JSON] [Card]
    :<|> Capture "id" Int64 :> Get '[JSON] Card
    :<|> Capture "id" Int64 :> DeleteNoContent '[JSON] NoContent
    :<|> ReqBody '[JSON] Card :> Post '[JSON] Card
    :<|> Capture "id" Int64
                 :> ReqBody '[JSON] Card
                 :> Post '[JSON] Card

type DummyAPI = "users" :> UserAPI :<|> "boards" :> BoardAPI :<|> "lists" :> ListAPI :<|> "cards" :> CardAPI

type DocsAPI = DummyAPI :<|> Raw

type AppM = ReaderT Config (ExceptT ServantErr IO)

-- Handlers

userServer :: ServerT UserAPI AppM
userServer = listUsers
        :<|> getUser
        :<|> deleteUser
        :<|> createUser
        :<|> updateUser

boardServer :: ServerT BoardAPI AppM
boardServer = listBoards
        :<|> getBoardLists
        :<|> getBoardCards
        :<|> getBoard
        :<|> deleteBoard
        :<|> createBoard
        :<|> updateBoard

listServer :: ServerT ListAPI AppM
listServer = listLists
        :<|> getListCards
        :<|> getList
        :<|> deleteList
        :<|> createList
        :<|> updateList


cardServer :: ServerT CardAPI AppM
cardServer = listCards
        :<|> getCard
        :<|> deleteCard
        :<|> createCard
        :<|> updateCard

dummyServer :: ServerT DummyAPI AppM
dummyServer = userServer :<|> boardServer :<|> listServer :<|> cardServer

docsServer _ respond = respond $ responseLBS ok200 [plain] docsBS
  where plain = ("Content-Type", "text/plain")

readerToHandler :: Config -> AppM :~> ExceptT ServantErr IO
readerToHandler cfg = Nat $ \x -> runReaderT x cfg

readerDummyServer :: Config -> Server DocsAPI
readerDummyServer cfg = enter (readerToHandler cfg) dummyServer
                        :<|> docsServer

dummyAPI :: Proxy DummyAPI
dummyAPI = Proxy

docsAPI :: Proxy DocsAPI
docsAPI = Proxy

app :: Config -> Application
app cfg = serve docsAPI (readerDummyServer cfg)

-- User controllers

listUsers :: AppM [User]
listUsers = do
    users :: [Entity User] <- runDb $ selectList [] []
    let us = map (\(Entity _ y) -> y) users
    return us

getUser :: Int64 -> AppM User
getUser uid = do
    user <- runDb $ get (toSqlKey uid)
    case user of
        Nothing -> throwError err404
        Just u  -> return u

createUser :: User -> AppM User
createUser user = do
    newUserId <- runDb $ insert user
    return $ user

updateUser :: Int64 -> User ->  AppM User
updateUser i u = do
    user <- runDb $ get ((toSqlKey i) :: Key User)
    case user of
        Nothing -> throwError err404
        Just x ->  do
            runDb $ update (toSqlKey i) $ userToUpdate u
            return u

deleteUser :: Int64 -> AppM NoContent
deleteUser uid = do
    runDb $ delete ((toSqlKey uid) :: Key User)
    return NoContent

-- Board controllers

listBoards :: AppM [Board]
listBoards = do
        boards :: [Entity Board] <- runDb $ selectList [] []
        let bs = map (\(Entity _ y) -> y) boards
        return bs

getBoardLists :: Int64 -> AppM [List]
getBoardLists bid = do
        lists <- runDb $ selectList [ListBoardId ==. (toSqlKey bid)] []
        let cs = map (\(Entity _ y) -> y) lists
        return cs

getBoardCards :: Int64 -> AppM [Card]
getBoardCards bid = do
        lists <- runDb $ selectList [ListBoardId ==. (toSqlKey bid)] []
        let cs = map (\(Entity x _) -> x) lists
        cards <- runDb $ selectList [CardListId <-. cs] []
        let cc = map (\(Entity _ y) -> y) cards
        return cc

getBoard :: Int64 -> AppM Board
getBoard bid = do
        board <- runDb $ get (toSqlKey bid)
        case board of
            Nothing -> throwError err404
            Just b  -> return b

createBoard :: Board -> AppM Board
createBoard b = do
        newBoardId <- runDb $ insert b
        return b

updateBoard :: Int64 -> Board -> AppM Board
updateBoard i b = do
        board <- runDb $ get ((toSqlKey i) :: Key Board)
        case board of
            Nothing -> throwError err404
            Just _ -> do
                runDb $ update (toSqlKey i) $ boardToUpdate b
                return b

deleteBoard :: Int64 -> AppM NoContent
deleteBoard bid = do
    runDb $ delete ((toSqlKey bid) :: Key Board)
    return NoContent


-- List controllers

listLists :: AppM [List]
listLists = do
        lists :: [Entity List] <- runDb $ selectList [] []
        let cs = map (\(Entity _ y) -> y) lists
        return cs

getListCards :: Int64 -> AppM [Card]
getListCards lid = do
        cards <- runDb $ selectList [CardListId ==. (toSqlKey lid)] []
        let cs = map (\(Entity _ y) -> y) cards
        return cs

getList :: Int64 -> AppM List
getList lid = do
        list <- runDb $ get (toSqlKey lid)
        case list of
            Nothing -> throwError err404
            Just l  -> return l

createList :: List -> AppM List
createList l = do
        newListId <- runDb $ insert l
        return l

updateList :: Int64 -> List -> AppM List
updateList i l = do
        list <- runDb $ get ((toSqlKey i) :: Key List)
        case list of
            Nothing -> throwError err404
            Just _ -> do
                runDb $ update (toSqlKey i) $ listToUpdate l
                return l

deleteList :: Int64 -> AppM NoContent
deleteList lid = do
    runDb $ delete ((toSqlKey lid) :: Key List)
    return NoContent


-- Card controllers

listCards :: AppM [Card]
listCards = do
        cards :: [Entity Card] <- runDb $ selectList [] []
        let cs = map (\(Entity _ y) -> y) cards
        return cs

getCard :: Int64 -> AppM Card
getCard cid = do
        card <- runDb $ get (toSqlKey cid)
        case card of
            Nothing -> throwError err404
            Just c  -> return c

createCard :: Card -> AppM Card
createCard c = do
        newCardId <- runDb $ insert c
        return c

updateCard :: Int64 -> Card -> AppM Card
updateCard i c = do
        card <- runDb $ get ((toSqlKey i) :: Key Card)
        case card of
            Nothing -> throwError err404
            Just _ -> do
                runDb $ update (toSqlKey i) $ cardToUpdate c
                return c

deleteCard :: Int64 -> AppM NoContent
deleteCard cid = do
    runDb $ delete ((toSqlKey cid) :: Key Card)
    return NoContent

