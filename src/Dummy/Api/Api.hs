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
                                              selectList, (==.))
import           Database.Persist.Sql
import           Network.Wai                 (Application)
import           Servant

-- import           Database.Esqueleto
import           Dummy.Api.Config            (Config (..))
import           Dummy.Api.Models


type UserAPI = Get '[JSON] [User]
    :<|> Capture "id" Int64 :> Get '[JSON] User
    :<|> Capture "id" Int64 :> Delete '[] ()
    :<|> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] User
         :> Post '[JSON] ()

type BoardAPI = Get '[JSON] [Board]
    :<|> Capture "id" Int64 :> Get '[JSON] Board
    :<|> Capture "id" Int64 :> Delete '[] ()
    :<|> ReqBody '[JSON] Board :> Post '[JSON] Int64
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] Board
         :> Post '[JSON] Board

type ScrollAPI = Get '[JSON] [Scroll]
    :<|> Capture "id" Int64 :> Get '[JSON] Scroll
    :<|> Capture "id" Int64 :> Delete '[] ()
    :<|> ReqBody '[JSON] Scroll :> Post '[JSON] Int64
    :<|> Capture "id" Int64
         :> ReqBody '[JSON] Scroll
         :> Post '[JSON] Scroll

type CardAPI = Get '[JSON] [Card]
    :<|> Capture "id" Int64 :> Get '[JSON] Card
    :<|> Capture "id" Int64 :> Delete '[] ()
    :<|> ReqBody '[JSON] Card :> Post '[JSON] Int64
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
        :<|> getBoard
        :<|> deleteBoard
        :<|> createBoard
        :<|> updateBoard

scrollServer :: ServerT ScrollAPI AppM
scrollServer = listScrolls
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

-- User functions
-- userToPerson User{..} = Person { name = userName, email = userEmail  }
listUsers :: AppM [User]
listUsers = do
    users :: [Entity User] <- runDb $ selectList [] []
    let us = map (\(Entity _ y) -> y) users
    return us

getUser :: Int64 -> AppM User
getUser u = do
    user <- runDb $ get (toSqlKey u)
    case user of
        Nothing -> lift $ left err404
        Just u -> return u

createUser :: User -> AppM Int64
createUser user = do
    newUser <- runDb $ insert user
    return $ fromSqlKey newUser

updateUser :: Int64 -> User ->  AppM ()
updateUser i u = do
    user <- runDb $ get ((toSqlKey i) :: Key User)
    case user of
        Nothing -> lift $ left err404
        Just x ->  do
            runDb $ update (toSqlKey i) $ userToUpdate u

deleteUser :: Int64 -> AppM ()
deleteUser u = do
    runDb $ delete ((toSqlKey u) :: Key User)

-- Board functions

listBoards :: AppM [Board]
listBoards = undefined

getBoard :: Int64 -> AppM Board
getBoard = undefined

createBoard :: Board -> AppM Int64
createBoard = undefined

updateBoard :: Int64 -> Board -> AppM Board
updateBoard = undefined

deleteBoard :: Int64 -> AppM ()
deleteBoard = undefined


-- Scroll functions

listScrolls :: AppM [Scroll]
listScrolls = undefined

getScroll :: Int64 -> AppM Scroll
getScroll = undefined

createScroll :: Scroll -> AppM Int64
createScroll = undefined

updateScroll :: Int64 -> Scroll -> AppM Scroll
updateScroll = undefined

deleteScroll :: Int64 -> AppM ()
deleteScroll = undefined


-- Card functions

listCards :: AppM [Card]
listCards = undefined

getCard :: Int64 -> AppM Card
getCard = undefined

createCard :: Card -> AppM Int64
createCard = undefined

updateCard :: Int64 -> Card -> AppM Card
updateCard = undefined

deleteCard :: Int64 -> AppM ()
deleteCard = undefined


