{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Dummy.Api.Api where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT, lift, runReaderT)
import           Control.Monad.Trans.Either  (EitherT, left)
import           Data.Int                    (Int64)
import           Database.Persist
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              rawSql, selectList, (<-.), (==.))
import           Database.Persist.Sql
import           Network.Wai                 (Application)
import           Servant

-- import           Database.Esqueleto
import           Dummy.Api.Config            (Config (..))
import           Dummy.Api.Models


type UserAPI = Get '[JSON] [User]
    :<|> Capture "id" Int64 :> Get '[JSON] User
    :<|> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] User
         :> Post '[JSON] User

type BoardAPI = Get '[JSON] [Board]
    :<|> Capture "id" Int64 :> "scrolls" :> Get '[JSON] [Scroll]
    :<|> Capture "id" Int64 :> "cards" :> Get '[JSON] [Card]
    :<|> Capture "id" Int64 :> Get '[JSON] Board
    :<|> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> ReqBody '[JSON] Board :> Post '[JSON] Board
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] Board
         :> Post '[JSON] Board

type ScrollAPI = Get '[JSON] [Scroll]
    :<|> Capture "id" Int64 :> "cards" :> Get '[JSON] [Card]
    :<|> Capture "id" Int64 :> Get '[JSON] Scroll
    :<|> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> ReqBody '[JSON] Scroll :> Post '[JSON] Scroll
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] Scroll
         :> Post '[JSON] Scroll

type CardAPI = Get '[JSON] [Card]
    :<|> Capture "id" Int64 :> Get '[JSON] Card
    :<|> Capture "id" Int64 :> Delete '[JSON] ()
    :<|> ReqBody '[JSON] Card :> Post '[JSON] Card
    :<|> Capture "id" Int64
                 :> ReqBody '[JSON] Card
                 :> Post '[JSON] Card

type DummyAPI = "users" :> UserAPI :<|> "boards" :> BoardAPI :<|> "scrolls" :> ScrollAPI :<|> "cards" :> CardAPI

type AppM = ReaderT Config (EitherT ServantErr IO)

-- Handlers

userServer :: ServerT UserAPI AppM
userServer = listUsers
        :<|> getUser
        :<|> deleteUser
        :<|> createUser
        :<|> updateUser

boardServer :: ServerT BoardAPI AppM
boardServer = listBoards
        :<|> getBoardScrolls
        :<|> getBoardCards
        :<|> getBoard
        :<|> deleteBoard
        :<|> createBoard
        :<|> updateBoard

scrollServer :: ServerT ScrollAPI AppM
scrollServer = listScrolls
        :<|> getScrollCards
        :<|> getScroll
        :<|> deleteScroll
        :<|> createScroll
        :<|> updateScroll


cardServer :: ServerT CardAPI AppM
cardServer = listCards
        :<|> getCard
        :<|> deleteCard
        :<|> createCard
        :<|> updateCard

dummyServer :: ServerT DummyAPI AppM
dummyServer = userServer :<|> boardServer :<|> scrollServer :<|> cardServer

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerDummyServer :: Config -> Server DummyAPI
readerDummyServer cfg = enter (readerToEither cfg) dummyServer

dummyAPI :: Proxy DummyAPI
dummyAPI = Proxy

app :: Config -> Application
app cfg = serve dummyAPI (readerDummyServer cfg)

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
        Nothing -> lift $ left err404
        Just u -> return u

createUser :: User -> AppM User
createUser user = do
    newUserId <- runDb $ insert user
    return $ user

updateUser :: Int64 -> User ->  AppM User
updateUser i u = do
    user <- runDb $ get ((toSqlKey i) :: Key User)
    case user of
        Nothing -> lift $ left err404
        Just x ->  do
            runDb $ update (toSqlKey i) $ userToUpdate u
            return u

deleteUser :: Int64 -> AppM ()
deleteUser uid = do
    runDb $ delete ((toSqlKey uid) :: Key User)

-- Board controllers

listBoards :: AppM [Board]
listBoards = do
        boards :: [Entity Board] <- runDb $ selectList [] []
        let bs = map (\(Entity _ y) -> y) boards
        return bs

getBoardScrolls :: Int64 -> AppM [Scroll]
getBoardScrolls bid = do
        scrolls <- runDb $ selectList [ScrollBoardId ==. (toSqlKey bid)] []
        let ss = map (\(Entity _ y) -> y) scrolls
        return ss

getBoardCards :: Int64 -> AppM [Card]
getBoardCards bid = do
        scrolls <- runDb $ selectList [ScrollBoardId ==. (toSqlKey bid)] []
        let ss = map (\(Entity x _) -> x) scrolls
        cards <- runDb $ selectList [CardScrollId <-. ss] []
        let cc = map (\(Entity _ y) -> y) cards
        return cc

getBoard :: Int64 -> AppM Board
getBoard bid = do
        board <- runDb $ get (toSqlKey bid)
        case board of
            Nothing -> lift $ left err404
            Just b -> return b

createBoard :: Board -> AppM Board
createBoard b = do
        newBoardId <- runDb $ insert b
        return b

updateBoard :: Int64 -> Board -> AppM Board
updateBoard i b = do
        board <- runDb $ get ((toSqlKey i) :: Key Board)
        case board of
            Nothing -> lift $ left err404
            Just _ -> do
                runDb $ update (toSqlKey i) $ boardToUpdate b
                return b

deleteBoard :: Int64 -> AppM ()
deleteBoard bid = do
    runDb $ delete ((toSqlKey bid) :: Key Board)


-- Scroll controllers

listScrolls :: AppM [Scroll]
listScrolls = do
        scrolls :: [Entity Scroll] <- runDb $ selectList [] []
        let ss = map (\(Entity _ y) -> y) scrolls
        return ss

getScrollCards :: Int64 -> AppM [Card]
getScrollCards sid = do
        cards <- runDb $ selectList [CardScrollId ==. (toSqlKey sid)] []
        let cs = map (\(Entity _ y) -> y) cards
        return cs

getScroll :: Int64 -> AppM Scroll
getScroll sid = do
        scroll <- runDb $ get (toSqlKey sid)
        case scroll of
            Nothing -> lift $ left err404
            Just s -> return s

createScroll :: Scroll -> AppM Scroll
createScroll s = do
        newScrollId <- runDb $ insert s
        return s

updateScroll :: Int64 -> Scroll -> AppM Scroll
updateScroll i s = do
        scroll <- runDb $ get ((toSqlKey i) :: Key Scroll)
        case scroll of
            Nothing -> lift $ left err404
            Just _ -> do
                runDb $ update (toSqlKey i) $ scrollToUpdate s
                return s

deleteScroll :: Int64 -> AppM ()
deleteScroll sid = do
        runDb $ delete ((toSqlKey sid) :: Key Scroll)


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
            Nothing -> lift $ left err404
            Just c -> return c

createCard :: Card -> AppM Card
createCard c = do
        newCardId <- runDb $ insert c
        return c

updateCard :: Int64 -> Card -> AppM Card
updateCard i c = do
        card <- runDb $ get ((toSqlKey i) :: Key Card)
        case card of
            Nothing -> lift $ left err404
            Just _ -> do
                runDb $ update (toSqlKey i) $ cardToUpdate c
                return c

deleteCard :: Int64 -> AppM ()
deleteCard cid = do
        runDb $ delete ((toSqlKey cid) :: Key Card)

