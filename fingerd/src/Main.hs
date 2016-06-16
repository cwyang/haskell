{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

data User =
  User {
  userId :: Integer
  , username :: Text
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

instance ToRow User where
  toRow (User a b c d e f) = toRow (a, b, c, d, e, f)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  shell TEXT, homeDirectory TEXT,
  realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?,?,?,?,?,?)"
allUsers :: Query
allUsers = "SELECT * from users"
getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)
instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser sample
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where sample :: UserRow
        sample = (Null, "cwyang", "/bin/nosh", "/home/cwyang", "handsome guy", "1234")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn sock = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      l = T.concat $ intersperse "\n" usernames
  sendAll sock (encodeUtf8 l)

formatUser :: User -> ByteString
formatUser (User _ a b c d _) = BS.concat
  ["login: ", e a, "\t\t\t\t",
   "name: ", e d, "\n",
   "dir: ", e c, "\t\t\t",
   "shell:", e b, "\n"]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn sock username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn $ "cannot find user " ++ show username
    Just user -> sendAll sock (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn sock = do
  msg <- recv sock 1024
  case msg of
    "\r\n" -> returnUsers dbConn sock
    name -> returnUser dbConn sock (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (s, _) <- accept sock
  putStrLn "new conn"
  handleQuery dbConn s
  sClose s
  
main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
    Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  sClose sock

