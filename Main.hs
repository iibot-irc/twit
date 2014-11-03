{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Char8 (pack)
import Network.HTTP.Conduit
import Network
import System.Environment
import Web.Authenticate.OAuth
import Data.Aeson
import Data.Conduit
import Data.Maybe
import Data.Text (Text)
import Control.Applicative

data Tweet =
  Tweet { text :: Text,
          user :: Text
          } deriving (Show)

data User =
    User { name :: Text,
           id :: Int
         } deriving (Show)

instance FromJSON Tweet where 
    parseJSON (Object t) = 
        Tweet <$> t .: "text" 
              <*> ((t .: "user") >>= (.: "name"))

instance FromJSON User where
    parseJSON (Object t) = 
        User <$> t .: "name" 
             <*> t .: "id"
    
userStream :: (OAuth, Credential) -> [String] -> IO ()
userStream (oauth,cred) users = do
    userIds <- getUserIds (oauth,cred) users 
    print userIds
    print $ init $ concatMap (++",") userIds
    req <- parseUrl $ "https://stream.twitter.com/1.1/statuses/filter.json?follow=" ++ concatMap (++",") userIds
    withManager $ \m -> do
        signedreq <- signOAuth oauth cred req
        response <- http signedreq m
        responseBody response $$+- printStream

getUserIds :: (OAuth, Credential) -> [String] -> IO [String]
getUserIds (oauth,cred) users = do 
    usersRes <- withSocketsDo $ do
        req <- parseUrl $ "https://api.twitter.com/1.1/users/lookup.json?screen_name=" ++ concatMap (++",") users
        signedreq <- signOAuth oauth cred req
        withManager $ httpLbs signedreq
    let usersBody = responseBody usersRes
    return $ map (show . Main.id) $ maybe [] catMaybes (decode usersBody :: Maybe [Maybe User])

printStream =
    awaitForever $ \str-> do
        let mTweet = decode (fromStrict str) :: Maybe Tweet
        case mTweet of
            Nothing -> return () 
            Just tweet -> liftIO $ print tweet

main :: IO ()
main = do
    consumerKey : consumerSecret : accessToken : accessSecret : users <- getArgs
    let oauth = newOAuth { oauthServerName     = "api.twitter.com"
                         , oauthConsumerKey    = pack consumerKey
                         , oauthConsumerSecret = pack consumerSecret}
    let cred = newCredential (pack $ accessToken) (pack $ accessSecret)
    userStream (oauth,cred) users
