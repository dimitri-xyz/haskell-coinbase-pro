{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Rest
    ( coinbaseGet
    , coinbasePost
    , coinbaseDelete
    , coinbaseDeleteDiscardBody
    , voidBody
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Catch
import           Control.Monad.Catch
import           Crypto.Hash
import           Data.Aeson
import           Data.Byteable
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as CBS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Text.Printf

import           Coinbase.Exchange.Types

import           Debug.Trace

type Signed = Bool
type IsForExchange = Bool

voidBody :: Maybe ()
voidBody = Nothing

coinbaseGet :: ( ToJSON a
               , FromJSON b
               , MonadIO m
               , MonadThrow m
               , MonadReader ExchangeConf m
               , MonadError ExchangeFailure m )
            => Signed -> Path -> Maybe a -> m b
coinbaseGet sgn p ma = coinbaseRequest "GET" sgn p ma >>= processResponse True

coinbasePost :: ( ToJSON a
                , FromJSON b
                , MonadIO m
                , MonadThrow m
                , MonadReader ExchangeConf m
                , MonadError ExchangeFailure m )
             => Signed -> Path -> Maybe a -> m b
coinbasePost sgn p ma = coinbaseRequest "POST" sgn p ma >>= processResponse True

coinbaseDelete :: ( ToJSON a
                  , FromJSON b
                 , MonadIO m
                 , MonadThrow m
                 , MonadReader ExchangeConf m
                  , MonadError ExchangeFailure m )
               => Signed -> Path -> Maybe a -> m b
coinbaseDelete sgn p ma = coinbaseRequest "DELETE" sgn p ma >>= processResponse True

coinbaseDeleteDiscardBody :: ( ToJSON a
                             , MonadIO m
                             , MonadThrow m
                             , MonadReader ExchangeConf m
                             , MonadError ExchangeFailure m )
                             => Signed -> Path -> Maybe a -> m ()
coinbaseDeleteDiscardBody sgn p ma = coinbaseRequest "DELETE" sgn p ma >>= processEmpty

coinbaseRequest :: ( ToJSON a
                   , MonadIO m
                   , MonadThrow m
                   , MonadReader ExchangeConf m
                   , MonadError ExchangeFailure m )
                => Method -> Signed -> Path -> Maybe a -> m (Response BS.ByteString)
coinbaseRequest meth sgn p ma = do
        conf <- ask
        req  <- case apiType conf of
                    Sandbox -> parseUrlThrow $ sandboxRest ++ p
                    Live    -> parseUrlThrow $ liveRest ++ p
        let req' = req { method         = meth
                       , requestHeaders = [ ("user-agent", "haskell")
                                          , ("accept", "application/json")
                                          ]
                       }

        resp <- flip httpLbs (manager conf) =<< signMessage True sgn meth p
                                            =<< encodeBody ma req'
        return (LBS.toStrict <$> resp)

realCoinbaseRequest :: ( ToJSON a
                           , MonadIO m
                           , MonadThrow m
                           , MonadReader ExchangeConf m
                           , MonadError ExchangeFailure m )
                        => Method -> Signed -> Path -> Maybe a -> m (Response BS.ByteString)
realCoinbaseRequest meth sgn p ma = do
        conf <- ask
        req  <- case apiType conf of
                    Sandbox -> parseUrlThrow $ sandboxRealCoinbaseRest ++ p
                    Live    -> parseUrlThrow $ liveRealCoinbaseRest ++ p
        let req' = req { method         = meth
                       , requestHeaders = [ ("user-agent", "haskell")
                                          , ("accept", "application/json")
                                          , ("content-type", "application/json")
                                          ]
                       }

        resp <- flip httpLbs (manager conf) =<< signMessage False sgn meth p
                                            =<< encodeBody ma req'
        return (LBS.toStrict <$> resp)


encodeBody :: (ToJSON a, Monad m)
           => Maybe a -> Request -> m Request
encodeBody (Just a) req = return req
                            { requestHeaders = requestHeaders req ++
                                               [ ("content-type", "application/json") ]
                            , requestBody = RequestBodyBS $ LBS.toStrict $ encode a
                            }
encodeBody Nothing  req = return req

signMessage :: (MonadIO m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => IsForExchange -> Signed -> Method -> Path -> Request -> m Request
signMessage isForExchange True meth p req = do
        conf <- ask
        case authToken conf of
            Just tok -> do time <- liftM (realToFrac . utcTimeToPOSIXSeconds) (liftIO getCurrentTime)
                                    >>= \t -> return . CBS.pack $ printf "%.0f" (t::Double)
                           rBody <- pullBody $ requestBody req
                           let presign = CBS.concat [time, meth, CBS.pack p, rBody]
                               sign    = if isForExchange
                                            then Base64.encode         $ toBytes       (hmac (secret tok)                 presign :: HMAC SHA256)
                                            else digestToHexByteString $ hmacGetDigest (hmac (Base64.encode $ secret tok) presign :: HMAC SHA256)

                           return req
                                { requestBody    = RequestBodyBS rBody
                                , requestHeaders = requestHeaders req ++
                                       [ ("cb-access-key", key tok)
                                       , ("cb-access-sign", sign )
                                       , ("cb-access-timestamp", time)
                                       ] ++ if isForExchange
                                                then [("cb-access-passphrase", passphrase tok)]
                                                else [("cb-version", "2016-05-11")]
                                }
            Nothing  -> throwError $ AuthenticationRequiredFailure $ T.pack p
    where pullBody (RequestBodyBS  b) = return b
          pullBody (RequestBodyLBS b) = return $ LBS.toStrict b
          pullBody _                  = throwError AuthenticationRequiresByteStrings
signMessage _ False _ _ req = return req

--

processResponse :: ( FromJSON b
                   , MonadReader ExchangeConf m
                   , MonadError ExchangeFailure m )
                => IsForExchange -> Response BS.ByteString -> m b
processResponse isForExchange res =
    case responseStatus res of
        s | s == status200 || (s == created201 && not isForExchange) ->
               case eitherDecode' (LBS.fromStrict $ responseBody res) of
                   Right b -> return b
                   Left er -> throwError $ ParseFailure $ T.pack er

          | otherwise ->  throwError
                        $ ApiFailure
                        $ T.decodeUtf8
                        $ responseBody res

processEmpty :: ( MonadReader ExchangeConf m
                , MonadError ExchangeFailure m )
             => Response BS.ByteString -> m ()
processEmpty res
    | responseStatus res == status200 = return ()
    | otherwise                       = throwError $ ApiFailure $ T.decodeUtf8 $ responseBody res
