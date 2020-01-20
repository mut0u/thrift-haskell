{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Tests.Twitter where
import qualified Thrift.Type as Thrift
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe

data TweetType = Tweet
               | Retweet
               | Dm
               | Reply
               deriving (Eq, Ord, Show, Enum, Bounded, Thrift.Data,
                         Thrift.Typeable, Thrift.Generic, Thrift.Hashable)

instance Thrift.Thrift TweetType where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Int32
        defaultValue = Tweet
        toTValue Tweet = Thrift.TInt32 0
        toTValue Retweet = Thrift.TInt32 2
        toTValue Dm = Thrift.TInt32 4
        toTValue Reply = Thrift.TInt32 5
        fromTValue (Thrift.TInt32 0) = Tweet
        fromTValue (Thrift.TInt32 2) = Retweet
        fromTValue (Thrift.TInt32 4) = Dm
        fromTValue (Thrift.TInt32 5) = Reply
        fromTValue _ = error "bad enum value"

data Location = Location{locationLatitude :: Double,
                         locationLongitude :: Double}
              deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                        Thrift.Hashable)

instance Thrift.Thrift Location where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let locationLatitude = Thrift.defaultValue
                locationLongitude = Thrift.defaultValue
              in Location{..}
        toTValue Location{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue locationLatitude),
                  Just (2, Thrift.toTValue locationLongitude)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                locationLatitude = Thrift.lookupRequired 1 m
                locationLongitude = Thrift.lookupRequired 2 m
              in Location{..}
        fromTValue _ = error "bad struct value"

data Tweet = Tweet{tweetUserId :: Thrift.Int32,
                   tweetUserName :: Thrift.Text, tweetText :: Thrift.Text,
                   tweetLoc :: Maybe Location, tweetTweetType :: Maybe TweetType,
                   tweetLanguage :: Maybe Thrift.Text}
           deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                     Thrift.Hashable)

instance Thrift.Thrift Tweet where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let tweetUserId = Thrift.defaultValue
                tweetUserName = Thrift.defaultValue
                tweetText = Thrift.defaultValue
                tweetLoc = Nothing
                tweetTweetType = Just Tweet
                tweetLanguage = Just "english"
              in Tweet{..}
        toTValue Tweet{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue tweetUserId),
                  Just (2, Thrift.toTValue tweetUserName),
                  Just (3, Thrift.toTValue tweetText),
                  case tweetLoc of
                      Just x -> Just (4, Thrift.toTValue x)
                      _ -> Nothing,
                  case tweetTweetType of
                      Just x -> Just (5, Thrift.toTValue x)
                      _ -> Nothing,
                  case tweetLanguage of
                      Just x -> Just (16, Thrift.toTValue x)
                      _ -> Nothing])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                tweetUserId = Thrift.lookupRequired 1 m
                tweetUserName = Thrift.lookupRequired 2 m
                tweetText = Thrift.lookupRequired 3 m
                tweetLoc = Thrift.lookupOptional Nothing 4 m
                tweetTweetType = Thrift.lookupOptional (Just Tweet) 5 m
                tweetLanguage = Thrift.lookupOptional (Just "english") 16 m
              in Tweet{..}
        fromTValue _ = error "bad struct value"

type TweetList = [Tweet]

data TweetSearchResult = TweetSearchResult{tweetSearchResultTweets
                                           :: TweetList}
                       deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                 Thrift.Hashable)

instance Thrift.Thrift TweetSearchResult where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let tweetSearchResultTweets = Thrift.defaultValue in
              TweetSearchResult{..}
        toTValue TweetSearchResult{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue tweetSearchResultTweets)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                tweetSearchResultTweets
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in TweetSearchResult{..}
        fromTValue _ = error "bad struct value"

data TwitterUnavailable = TwitterUnavailable{twitterUnavailableMessage
                                             :: Thrift.Text}
                        deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                  Thrift.Hashable)

instance Thrift.Thrift TwitterUnavailable where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let twitterUnavailableMessage = Thrift.defaultValue in
              TwitterUnavailable{..}
        toTValue TwitterUnavailable{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue twitterUnavailableMessage)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                twitterUnavailableMessage
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in TwitterUnavailable{..}
        fromTValue _ = error "bad struct value"

instance Exception.Exception TwitterUnavailable

data TwitterUnavailable2 = TwitterUnavailable2{twitterUnavailable2Message
                                               :: Thrift.Text}
                         deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                   Thrift.Hashable)

instance Thrift.Thrift TwitterUnavailable2 where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let twitterUnavailable2Message = Thrift.defaultValue in
              TwitterUnavailable2{..}
        toTValue TwitterUnavailable2{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue twitterUnavailable2Message)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                twitterUnavailable2Message
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in TwitterUnavailable2{..}
        fromTValue _ = error "bad struct value"

instance Exception.Exception TwitterUnavailable2

maxResults :: Thrift.Int32
maxResults = 100

type TwitterPingReq = ()

type TwitterPingRes = ()

twitterPing ::
            Thrift.Protocol ->
              Thrift.Transport -> TwitterPingReq -> IO TwitterPingRes
twitterPing = Thrift.request "ping" False

data TwitterPostTweetReq = TwitterPostTweetReq{twitterPostTweetReqTweet
                                               :: Tweet}
                         deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                   Thrift.Hashable)

instance Thrift.Thrift TwitterPostTweetReq where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let twitterPostTweetReqTweet = Thrift.defaultValue in
              TwitterPostTweetReq{..}
        toTValue TwitterPostTweetReq{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue twitterPostTweetReqTweet)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                twitterPostTweetReqTweet
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in TwitterPostTweetReq{..}
        fromTValue _ = error "bad struct value"

data TwitterPostTweetRes = TwitterPostTweetRes Bool
                         | TwitterPostTweetResUnavailable TwitterUnavailable
                         | TwitterPostTweetResUnavailable2 TwitterUnavailable2
                         deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                   Thrift.Hashable)

instance Thrift.Thrift TwitterPostTweetRes where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue = TwitterPostTweetRes Thrift.defaultValue
        toTValue (TwitterPostTweetRes x)
          = Thrift.TStruct [(0, Thrift.toTValue x)]
        toTValue (TwitterPostTweetResUnavailable x)
          = Thrift.TStruct [(1, Thrift.toTValue x)]
        toTValue (TwitterPostTweetResUnavailable2 x)
          = Thrift.TStruct [(2, Thrift.toTValue x)]
        fromTValue (Thrift.TStruct m)
          | Just x <- Thrift.lookup 0 m' =
            TwitterPostTweetRes (Thrift.fromTValue x)
          | Just x <- Thrift.lookup 1 m' =
            TwitterPostTweetResUnavailable (Thrift.fromTValue x)
          | Just x <- Thrift.lookup 2 m' =
            TwitterPostTweetResUnavailable2 (Thrift.fromTValue x)
          | otherwise = error "bad union struct"
          where m' = Thrift.mkIntMap m
        fromTValue _ = error "bad union struct value"

twitterPostTweet ::
                 Thrift.Protocol ->
                   Thrift.Transport -> TwitterPostTweetReq -> IO TwitterPostTweetRes
twitterPostTweet = Thrift.request "postTweet" False

data TwitterSearchTweetsReq = TwitterSearchTweetsReq{twitterSearchTweetsReqQuery
                                                     :: Thrift.Text}
                            deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                      Thrift.Hashable)

instance Thrift.Thrift TwitterSearchTweetsReq where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue
          = let twitterSearchTweetsReqQuery = Thrift.defaultValue in
              TwitterSearchTweetsReq{..}
        toTValue TwitterSearchTweetsReq{..}
          = Thrift.TStruct
              (Maybe.catMaybes
                 [Just (1, Thrift.toTValue twitterSearchTweetsReqQuery)])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.mkIntMap x
                twitterSearchTweetsReqQuery
                  = Thrift.lookupDefault Thrift.defaultValue 1 m
              in TwitterSearchTweetsReq{..}
        fromTValue _ = error "bad struct value"

data TwitterSearchTweetsRes = TwitterSearchTweetsRes TweetSearchResult
                            deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                                      Thrift.Hashable)

instance Thrift.Thrift TwitterSearchTweetsRes where
        typeCode = Thrift.TypeCodeTagged Thrift.TC_Struct
        defaultValue = TwitterSearchTweetsRes Thrift.defaultValue
        toTValue (TwitterSearchTweetsRes x)
          = Thrift.TStruct [(0, Thrift.toTValue x)]
        fromTValue (Thrift.TStruct m)
          | Just x <- Thrift.lookup 0 m' =
            TwitterSearchTweetsRes (Thrift.fromTValue x)
          | otherwise = error "bad union struct"
          where m' = Thrift.mkIntMap m
        fromTValue _ = error "bad union struct value"

twitterSearchTweets ::
                    Thrift.Protocol ->
                      Thrift.Transport ->
                        TwitterSearchTweetsReq -> IO TwitterSearchTweetsRes
twitterSearchTweets = Thrift.request "searchTweets" False

type TwitterZipReq = ()

type TwitterZipRes = ()

twitterZip ::
           Thrift.Protocol ->
             Thrift.Transport -> TwitterZipReq -> IO TwitterZipRes
twitterZip = Thrift.request "zip" True

data TwitterHandler = TwitterHandler{twitterPingHandler ::
                                     TwitterPingReq -> IO TwitterPingRes,
                                     twitterPostTweetHandler ::
                                     TwitterPostTweetReq -> IO TwitterPostTweetRes,
                                     twitterSearchTweetsHandler ::
                                     TwitterSearchTweetsReq -> IO TwitterSearchTweetsRes,
                                     twitterZipHandler :: TwitterZipReq -> IO TwitterZipRes}