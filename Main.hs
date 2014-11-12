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
import Data.Text as T (Text,concat)
import Data.Text.IO as T (putStrLn)
import Control.Applicative
import Control.Monad(liftM,(<=<))

data StreamMessage =
        Tweet { text :: Text,
            username :: Text
          }
        deriving (Show)

data User =
    User { name :: Text,
           id :: Int
         } deriving (Show)

instance FromJSON StreamMessage where
    parseJSON (Object t) = do
        tweetText <- t .: "text"
        screenName <- (t .: "user") >>= (.: "screen_name")
        return Tweet { text = tweetText, username = screenName}

instance FromJSON User where
    parseJSON (Object t) = 
        User <$> t .: "name" 
             <*> t .: "id"
    
userStream :: (OAuth, Credential) -> [String] -> IO ()
userStream (oauth,cred) users = do
    userIds <- getUserIds (oauth,cred) users 
    followReq <- getFollowReq userIds
    signedFReq <- signOAuth oauth cred followReq
    withManager $ \m -> do
        followResp <-  liftM responseBody $ http signedFReq m
        followResp $$+- printStream
    where
            getFollowReq followIds = do
                req <- parseUrl "https://stream.twitter.com/1.1/statuses/filter.json"
                return $ setQueryString [("follow", Just (pack $ concatMap (++",") followIds))] req
            getUserIds (oauth,cred) users = do
                usersRes <- withSocketsDo $ do
                    req <- parseUrl "https://api.twitter.com/1.1/users/lookup.json"
                    let uidReq = setQueryString [("screen_name", Just (pack $ concatMap (++",") users))] req
                    signedreq <- signOAuth oauth cred uidReq
                    withManager $ httpLbs signedreq
                let usersBody = responseBody usersRes
                return $ map (show . Main.id) $ maybe [] catMaybes (decode usersBody :: Maybe [Maybe User])

printStream =
    awaitForever $ \str-> do
        let mTweet = decode (fromStrict str) :: Maybe StreamMessage
        case mTweet of
            Just Tweet { text = t , username = u} -> liftIO $ T.putStrLn $ T.concat ["\0336@@\033 ",u, ": ", t]
            _ -> return ()

main :: IO ()
main = do
    consumerKey : consumerSecret : accessToken : accessSecret : users <- getArgs
    let oauth = newOAuth { oauthServerName     = "api.twitter.com"
                         , oauthConsumerKey    = pack consumerKey
                         , oauthConsumerSecret = pack consumerSecret}
    let cred = newCredential (pack accessToken) (pack accessSecret)
    userStream (oauth,cred) users
