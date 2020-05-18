{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Coinbase.Exchange.Types.Socket where

-------------------------------------------------------------------------------

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char                    (toLower)
import           Data.Data
import           Data.Hashable
import qualified Data.HashMap.Strict          as H
import           Data.Int
import           Data.Text                    (Text)
import           Data.Time
import           Data.UUID                    (fromText)
import qualified Data.Vector                  as V
import           Data.Word
import           GHC.Generics

-------------------------------------------------------------------------------

import           Coinbase.Exchange.Types.Core hiding ( BookEntry(..))

-------------------------------------------------------------------------------

-- | Data channels available via the Websocket API
data Channel = Full| Heartbeat | Level2 | Matches | Status | Ticker | User
  deriving (Bounded, Enum, Eq, Show, Read, Data, Typeable, Generic)

instance NFData Channel

instance ToJSON Channel where
  toJSON = genericToJSON coinbaseAesonOptions

instance FromJSON Channel where
  parseJSON = genericParseJSON
                defaultOptions { constructorTagModifier = map toLower }

--------------------------------------------------------------------------------

-- Types for detailed data fields of messages received from server and parsed
-- into `ExchangeMessage` values

-- Used only for list of current subscriptions received from server in response
-- to subscribe and unsubscribe messages, but not for creating a list of
-- subscriptions to send to the server as a "subscribe" message.
-- TODO: Change the implementation of outbound `Subscribe` messages to use this?
data Subscription = Subscription Channel [ProductId]
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData Subscription

instance ToJSON Subscription where
  toJSON (Subscription channels productIds) = object
    [ "name"        .= channels
    , "product_ids" .= productIds
    ]

instance FromJSON Subscription where
  parseJSON (Object o) = Subscription <$> o .: "name" <*> o .: "product_ids"
  parseJSON _          = mzero

-- TODO: Docs do not specify possible status values beyond "online"; verify.
-- NOTE: Both products and currencies have statuses. The phantom parameter `a`
-- should be set to `Product` or `Currency` as appropriate.
data Status a = Online | Offline
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData (Status a)

instance FromJSON (Status a) where
  parseJSON = genericParseJSON
                defaultOptions { constructorTagModifier = map toLower }

instance ToJSON (Status a) where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = map toLower }

-- Used for messages received via "status" channel
-- NOTE: There is another `Product` type in the `...Types.MarketData` module,
-- used for the data returned by the `getProducts` REST API call. But that call
-- returns less information than the WebSocket "status" channel.
data Product = Product
  { prodId             :: ProductId
  , prodBaseCurrency   :: CurrencyId
  , prodQuoteCurrency  :: CurrencyId
  , prodBaseMinSize    :: Size
  , prodBaseMaxSize    :: Size
  , prodBaseIncrement  :: Size
  , prodQuoteIncrement :: Price
  , prodDisplayName    :: Text
  , prodStatus         :: Status Product
  , prodStatusMsg      :: Maybe Text
  , prodMinMarketFunds :: Cost
  , prodMaxMarketFunds :: Cost
  , prodPostOnly       :: Bool
  , prodLimitOnly      :: Bool
  , prodCancelOnly     :: Bool
  } deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData Product

instance FromJSON Product where
  parseJSON = genericParseJSON coinbaseAesonOptions

instance ToJSON Product where
  toJSON = genericToJSON coinbaseAesonOptions

-- Used for messages received via "status" channel
-- NOTE: There is another `Currency` type in the `...Types.MarketData` module,
-- used for the data returned by the `getCurrencies` REST API call. But that
-- call returns less information than the Websocket "status" channel.
data Currency = Currency
  { currId            :: CurrencyId
  , currName          :: Text
  , currMinSize       :: Size
  , currStatus        :: Status Currency
  , currStatusMsg     :: Maybe Text
  , currMaxPrecision  :: Price
  , currConvertibleTo :: [CurrencyId]
  , currDetails       :: CurrencyDetails -- NOTE: Undocumented
  } deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData Currency

instance FromJSON Currency where
  parseJSON = genericParseJSON coinbaseAesonOptions

instance ToJSON Currency where
  toJSON = genericToJSON coinbaseAesonOptions

-- Used in the `Currency` type for messages parsed from the "status" channel
-- NOTE: Undocumented
data CurrencyDetails = CurrencyDetails
  { cdSymbol                :: Text
  , cdGroupTypes            :: Maybe [Text]
  , cdSortOrder             :: Int64
  , cdPushPaymentMethods    :: [Text]
  , cdNetworkConfirmations  :: Int64
  , cdCryptoTransactionLink :: Text
  } deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData CurrencyDetails

instance FromJSON CurrencyDetails where
  parseJSON = genericParseJSON coinbaseAesonOptions

instance ToJSON CurrencyDetails where
  toJSON = genericToJSON coinbaseAesonOptions

-- Used in `L2SnapshotMsg` messages received via the "l2update" channel
data L2BookEntry = L2BookEntry Price Size
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData L2BookEntry

instance ToJSON L2BookEntry where
  toJSON (L2BookEntry price size) = toJSON
    [toJSON price, toJSON size]

instance FromJSON L2BookEntry where
  parseJSON (Array a) = case (V.toList a) of
    [jsnPrice, jsnSize] -> L2BookEntry
      <$> parseJSON jsnPrice
      <*> parseJSON jsnSize
    otherwise -> mzero

type L2Ask = L2BookEntry

type L2Bid = L2BookEntry

-- Used in `L2UpdateMsg` messages received via the "l2update" channel
data BookChange = BookChange Side Price Size
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData BookChange

instance ToJSON BookChange where
  toJSON (BookChange side price size) = toJSON
    [toJSON side, toJSON price, toJSON size]

instance FromJSON BookChange where
  parseJSON (Array a) = case (V.toList a) of
    [jsnSide, jsnPrice, jsnSize] -> BookChange
      <$> parseJSON jsnSide
      <*> parseJSON jsnPrice
      <*> parseJSON jsnSize
    otherwise -> mzero

-------------------------------------------------------------------------------
-- `SendExchangeMessage` is the type of outbound client-to-server messages for
-- subscribing to and unsubscribing from WebSocket channels.
--
-- NOTE: This implementation subscribes to or unsubscribes from all channels
-- listed for all products listed. Subscribing to or unsubscribing from selected
-- channels for selected products can be accomplished by sending multiple
-- messages.
-- TODO: But this implementation doesn't allow us to parse some documented
-- outbound messages. We don't need to do that except perhaps for testing
-- purposes, but we should probably re-implement it appropriately.
--
-- | Messages we can send to the exchange
data SendExchangeMessage
    = Subscribe   [ProductId] [Channel]
    | Unsubscribe [ProductId] [Channel]
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData SendExchangeMessage

-- TODO: Implement fields for authenticated "subscribe" requests; see docs.
instance ToJSON SendExchangeMessage where
  toJSON (Subscribe pids chans) = object
    [ "type"        .= ("subscribe" :: Text)
    , "product_ids" .= pids
    , "channels"    .= chans
    ]
  toJSON (Unsubscribe pids chans) = object
    [ "type" .= ("unsubscribe" :: Text)
    , "product_ids" .= pids
    , "channels"    .= chans
    ]

instance FromJSON SendExchangeMessage where
  parseJSON (Object o) = do
    msgType <- o .: "type"
    case (msgType :: Text) of
      "subscribe"   -> Subscribe <$> o .: "product_ids" <*> o .: "channels"

      -- The "product_ids" field is optional in an "unsubscribe" message; when
      -- missing, the specified channels will be unsubscribed for all products.
      -- Our `ToJSON` instance always creates a "product_ids" field (possibly
      -- set to an empty array), but the documentation includes an example
      -- without the field, which we should be able to parse correctly.
      "unsubscribe" -> Unsubscribe <$> o .:? "product_ids" .!= []
                                   <*> o .: "channels"

-------------------------------------------------------------------------------
-- `ExchangeMessage` is the type of all inbound server-to-client messages sent
-- via the WebSocket API.
--
-- | Messages they send back to us
data ExchangeMessage
    = ErrorMsg   -- Message type "error", explicit error message sent by the
                 -- exchange server
                 -- FIXME: Define this here or treat as failure case in
                 -- `FromJSON` instance?
        { msgErrorMessage :: Text
        }
    | SubscriptionsMsg  -- Message type "subscriptions", in response to client's
                        -- subscribe or unsubscribe message
        { msgSubscriptionsMsgs :: [Subscription]
        }
    | HeartbeatMsg  -- Message type "heartbeat", via "heartbeat" channel
        { msgTime        :: UTCTime
        , msgProductId   :: ProductId
        , msgSequence    :: Sequence
        , msgLastTradeId :: TradeId
        }
    | StatusMsg  -- Message type "status", via "status" channel
        { msgProducts :: [Product]
        , msgCurrencies :: [Currency]
        }
    | StartTickerMsg -- Message type "ticker, via "ticker" channel; sent only
                     -- as the first message immediately after subscribing
                     -- TODO: Undocumented, notify Coinbase?
    -- NOTE: This and `TickerMsg` don't yet implement market stats (hi, lo, vol)
    -- TODO: Check the fields
        { msgSequence  :: Sequence
        , msgProductId :: ProductId
        , msgPrice     :: Price
        , msgBestAsk   :: Maybe BestPrice
        , msgBestBid   :: Maybe BestPrice
        }
    | TickerMsg  -- Message type "ticker", via "ticker" channel, sent on each
                 -- match
        -- TODO: Use `Trade` type defined in ... Types.MarketData here instead?
        { msgTime :: UTCTime
        , msgSequence :: Sequence
        , msgProductId :: ProductId
        , msgTradeId :: TradeId
        , msgPrice :: Price
        , msgSide :: Side
        , msgLastSize :: Size
        , msgBestAsk :: Maybe BestPrice
        , msgBestBid :: Maybe BestPrice
        }
    | L2SnapshotMsg -- Message type "snapshot", via "level2" channel, sent only
                    -- at start of subscription
        { msgProductId :: ProductId
        , msgAsks :: [L2Ask]
        , msgBids :: [L2Bid]
        }
    | L2UpdateMsg  -- Message type "l2update", via "level2" channel, sent on
                   -- each change to the order book
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgChanges   :: [BookChange]
        }
    | ReceivedLimitMsg  -- Message type "received", via "full" or "user"
                        -- channels
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgSequence  :: Sequence
        , msgOrderId   :: OrderId
        , msgSide      :: Side
        , msgClientOid :: Maybe ClientOrderId
        --
        , msgPrice     :: Price
        , msgSize      :: Size
        }
    | ReceivedMarketMsg  -- Message type "received", via "full" or "user"
                         -- channels
        { msgTime         :: UTCTime
        , msgProductId    :: ProductId
        , msgSequence     :: Sequence
        , msgOrderId      :: OrderId
        , msgSide         :: Side
        , msgClientOid    :: Maybe ClientOrderId
        -- market orders have no price and are bounded by either size, funds or both
        , msgMarketBounds :: (Either Size (Maybe Size, Cost))
        }
    | OpenMsg  -- Message type "open", "full" or "user" channel
        { msgTime          :: UTCTime
        , msgProductId     :: ProductId
        , msgSequence      :: Sequence
        , msgOrderId       :: OrderId
        , msgSide          :: Side
        , msgRemainingSize :: Size
        , msgPrice         :: Price
        }
    | MatchMsg  -- Message type "match", via "matches", "full", or "user"
                -- channels
    -- TODO: There are four additional fields for authenticated user's matched
    -- orders; see docs.
        { msgTime         :: UTCTime
        , msgProductId    :: ProductId
        , msgSequence     :: Sequence
        , msgSide         :: Side
        , msgTradeId      :: TradeId
        , msgMakerOrderId :: OrderId
        , msgTakerOrderId :: OrderId
        , msgSize         :: Size
        , msgPrice        :: Price
        }
    | LastMatchMsg  -- Message type "last_match", via ???
    -- TODO: Which channel is this received on? Undocumented, notify Coinbase?
    -- Seems to be the same as "match", probably is received as the first
    -- message on subscribing to any channel that sends "match" messages
        { msgTime         :: UTCTime
        , msgProductId    :: ProductId
        , msgSequence     :: Sequence
        , msgSide         :: Side
        , msgTradeId      :: TradeId
        , msgMakerOrderId :: OrderId
        , msgTakerOrderId :: OrderId
        , msgSize         :: Size
        , msgPrice        :: Price
        }
    | DoneMsg  -- Message type "done", via "full" or "user" channels
        { msgTime         :: UTCTime
        , msgProductId    :: ProductId
        , msgSequence     :: Sequence
        , msgOrderId      :: OrderId
        , msgSide         :: Side
        , msgReason       :: Reason
        -- It is possible for these next two fields to be Nothing separately
        -- Filled market orders limited by funds will not have a price but may have remaining_size
        -- Filled limit orders may have a price but not a remaining_size (assumed zero)
        -- CURRENTLY ** `remaining_size` reported in DoneMsg messages is sometimes incorrect **
        -- This appears to be bug at Coinbase. I've told them about it.
        , msgMaybePrice   :: Maybe Price
        , msgMaybeRemSize :: Maybe Size
        }
    | ChangeLimitMsg  -- Message type "change", via "full" or "user" channels
        { msgTime       :: UTCTime
        , msgProductId  :: ProductId
        , msgSequence   :: Sequence
        , msgOrderId    :: OrderId
        , msgSide       :: Side
        -- Observation has revealed Price is not always present in
        -- change messages with old_size and new_size. This may be
        -- self trade prevention or something of the sort.
        , msgMaybePrice :: Maybe Price
        , msgNewSize    :: Size
        , msgOldSize    :: Size
        }
    | ChangeMarketMsg  -- Message type "change", via "full" or "user" channels
        { msgTime      :: UTCTime
        , msgProductId :: ProductId
        , msgSequence  :: Sequence
        , msgOrderId   :: OrderId
        , msgSide      :: Side
        , msgNewFunds  :: Cost
        , msgOldFunds  :: Cost
        }
    -- | Activate  -- Message type "activate", via "full" or "user" channel;
                   -- sent on activation of a stop order
    -- TODO: The docs for "activate" show odd JSON, e.g., "timestamp"
    -- field rather than the usual "time", and "product_id" set to
    -- "test_product".  Is this implemented? Experimental?
    deriving (Eq, Show, Read, Data, Typeable, Generic)

instance NFData ExchangeMessage

-- TODO: Add a constructor for unknown (or unimplemented) messages to the
-- `ExchangeMessage` type and produce explicit unknown messages rather than
-- parse errors? That is, there are two distinct kinds of failure here: (1)
-- parsing fails for a known message "type", and (2) we get a message with an
-- unrecognized "type" field.
instance FromJSON ExchangeMessage where
  parseJSON (Object m) = do
    msgType <- m .: "type"
    -- TO DO: `Subscribe` message type is missing as it is never received
    -- by the client.
    case (msgType :: Text) of
      "error" -> ErrorMsg <$> m .: "message"
      "subscriptions" -> do
        channels <- m .: "channels"
        SubscriptionsMsg <$> parseJSON channels
      "heartbeat"-> HeartbeatMsg
        <$> m .: "time"
        <*> m .: "product_id"
        <*> m .: "sequence"
        <*> m .: "last_trade_id"
      "status" -> StatusMsg
        <$> m .: "products"
        <*> m .: "currencies"
      "ticker" ->
        let tickMsg = TickerMsg
              <$> m .: "time"
              <*> m .: "sequence"
              <*> m .: "product_id"
              <*> m .: "trade_id"
              <*> m .: "price"
              <*> m .: "side"
              <*> m .: "last_size"
              <*> m .:? "best_ask"
              <*> m .:? "best_bid"
            startTickerMsg = StartTickerMsg
              <$> m .: "sequence"
              <*> m .: "product_id"
              <*> m .: "price"
              <*> m .:? "best_ask"
              <*> m .:? "best_bid"
        in tickMsg <|> startTickerMsg
      "snapshot" -> L2SnapshotMsg
        <$> m .: "product_id"
        <*> m .: "asks"
        <*> m .: "bids"
      "l2update" -> L2UpdateMsg
        <$> m .: "time"
        <*> m .: "product_id"
        <*> m .: "changes"
      "open" -> OpenMsg
        <$> m .: "time"
        <*> m .: "product_id"
        <*> m .: "sequence"
        <*> m .: "order_id"
        <*> m .: "side"
        <*> m .: "remaining_size"
        <*> m .: "price"
      "done" -> DoneMsg
        <$> m .: "time"
        <*> m .: "product_id"
        <*> m .: "sequence"
        <*> m .: "order_id"
        <*> m .: "side"
        <*> m .: "reason"
        <*> m .:? "price"
        <*> m .:? "remaining_size"
      "match" -> MatchMsg  -- TODO: Four extra fields for authenticated
                           -- user's matched orders; see docs
        <$> m .: "time"
        <*> m .: "product_id"
        <*> m .: "sequence"
        <*> m .: "side"
        <*> m .: "trade_id"
        <*> m .: "maker_order_id"
        <*> m .: "taker_order_id"
        <*> m .: "size"
        <*> m .: "price"
      "last_match" -> LastMatchMsg
        <$> m .: "time"
        <*> m .: "product_id"
        <*> m .: "sequence"
        <*> m .: "side"
        <*> m .: "trade_id"
        <*> m .: "maker_order_id"
        <*> m .: "taker_order_id"
        <*> m .: "size"
        <*> m .: "price"
      "change" -> do
        ms <- m .:? "price"
        let market = ChangeMarketMsg
              <$> m .: "time"
              <*> m .: "product_id"
              <*> m .: "sequence"
              <*> m .: "order_id"
              <*> m .: "side"
              <*> m .: "new_funds"
              <*> m .: "old_funds"
            limit = ChangeLimitMsg
              <$> m .: "time"
              <*> m .: "product_id"
              <*> m .: "sequence"
              <*> m .: "order_id"
              <*> m .: "side"
              <*> m .:? "price"
              <*> m .: "new_size"
              <*> m .: "old_size"
        case (ms :: Maybe Price) of
          Nothing -> market <|> limit
          Just _ -> limit <|> market
      "received" -> do
        typ  <- m .:  "order_type"
        -- The "client_oid" field is optional, present if the client supplied a
        -- UUID to the server on order submission. For reasons unknown, the
        -- field is sometimes set to the empty string in received messages;
        -- we first parse into Maybe Text, then convert to a UUID if possible, 
        -- treating the empty string as if the "client_oid" were not present.
        mbTxt <- m .:? "client_oid"
        let parserClientOrderId = case mbTxt of
              Nothing  -> pure Nothing
              Just ""  -> pure Nothing
              Just txt -> case fromText txt of
                Nothing   -> empty
                Just uuid ->
                  pure $ Just $ ClientOrderId uuid
        case typ of
          Limit  -> ReceivedLimitMsg
                      <$> m .: "time"
                      <*> m .: "product_id"
                      <*> m .: "sequence"
                      <*> m .: "order_id"
                      <*> m .: "side"
                      <*> parserClientOrderId
                      <*> m .: "price"
                      <*> m .: "size"
          Market -> ReceivedMarketMsg
                      <$> m .: "time"
                      <*> m .: "product_id"
                      <*> m .: "sequence"
                      <*> m .: "order_id"
                      <*> m .: "side"
                      <*> parserClientOrderId
                      <*> (do
                        -- I can't try to parse "size" or "funds" with (.:?) here, their type is CoinScientific
                        -- but the fields may be "size":null and that will fail the (m .:? "size") parser.
                        ms <- m .:? "size"
                        mf <- m .:? "funds"
                        case (ms,mf) of
                            (Nothing, Nothing) -> mzero
                            (Just s , Nothing) -> return $ Left  s
                            (Nothing, Just f ) -> return $ Right (Nothing, f)
                            (Just s , Just f ) -> return $ Right (Just s , f)
                            )
      -- TODO: Implement the "activate" message for stop orders;
      -- the docs on this are scant and suggest this is still in testing
      -- "activate" -> Activate

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- | Convenience/storage instance; never sent to exchange
instance ToJSON ExchangeMessage where
  toJSON ErrorMsg{..} = object
    [ "type" .= ("error" :: Text)
    , "message" .= msgErrorMessage
    ]
  toJSON SubscriptionsMsg{..} = object
    [ "type" .= ("subscriptions" :: Text)
    ,  "channels" .=  msgSubscriptionsMsgs
    ]
  toJSON HeartbeatMsg{..} = object
    [ "type"          .= ("heartbeat" :: Text)
    , "sequence"      .= msgSequence
    , "time"          .= msgTime
    , "product_id"    .= msgProductId
    , "last_trade_id" .= msgLastTradeId
    ]
  toJSON StatusMsg{..} = object
    [ "type"       .= ("status" :: Text)
    , "products"   .= msgProducts
    , "currencies" .= msgCurrencies
    ]
  toJSON StartTickerMsg{..} = object
    [ "type"       .= ("ticker" :: Text)
    , "sequence"   .= msgSequence
    , "product_id" .= msgProductId
    , "price"      .= msgPrice
    , "best_ask"   .= msgBestAsk
    , "best_bid"   .= msgBestBid
    ]
  toJSON TickerMsg{..} = object
    ( [ "type"       .= ("ticker" :: Text)
      , "time"       .= msgTime
      , "sequence"   .= msgSequence
      , "product_id" .= msgProductId
      , "trade_id"   .= msgTradeId
      , "price"      .= msgPrice
      , "side"       .= msgSide
      , "last_size"  .= msgLastSize
      ] ++ case msgBestAsk of
            Nothing      -> []
            Just bestAsk -> ["best_ask" .= bestAsk]
        ++ case msgBestBid of
            Nothing      -> []
            Just bestBid -> ["best_bid" .= bestBid]
    )
  toJSON L2SnapshotMsg{..} = object
    [ "type"       .= ("snapshot" :: Text)
    , "product_id" .= msgProductId
    , "asks"       .= msgAsks
    , "bids"       .= msgBids
    ]
  toJSON L2UpdateMsg{..} = object
    [ "type"       .= ("l2update" :: Text)
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "changes"    .= msgChanges
    ]
  toJSON OpenMsg{..} = object
    [ "type"       .= ("open" :: Text)
    , "sequence"   .= msgSequence
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "order_id"   .= msgOrderId
    , "side"       .= msgSide
    , "remaining_size" .= msgRemainingSize
    , "price"      .= msgPrice
    ]
  toJSON DoneMsg{..} = object
    ( [ "type"      .= ("done" :: Text)
      , "sequence"   .= msgSequence
      , "time"       .= msgTime
      , "product_id" .= msgProductId
      , "order_id"   .= msgOrderId
      , "side"       .= msgSide
      , "reason"     .= msgReason
      ] ++ case msgMaybePrice of
             Nothing -> []
             Just  p -> ["price" .= p]
        ++ case msgMaybeRemSize of
             Nothing -> []
             Just  s -> ["remaining_size" .= s]
    )
  toJSON MatchMsg{..} = object
  -- TODO: Four extra fields for authenticated user's matched orders; see docs
    [ "type"       .= ("match" :: Text)
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "sequence"   .= msgSequence
    , "side"       .= msgSide
    , "trade_id"   .= msgTradeId
    , "maker_order_id" .= msgMakerOrderId
    , "taker_order_id" .= msgTakerOrderId
    , "size"       .= msgSize
    , "price"      .= msgPrice
    ]
  toJSON LastMatchMsg{..} = object
    [ "type"       .= ("last_match" :: Text)
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "sequence"   .= msgSequence
    , "side"       .= msgSide
    , "trade_id"   .= msgTradeId
    , "maker_order_id" .= msgMakerOrderId
    , "taker_order_id" .= msgTakerOrderId
    , "size"       .= msgSize
    , "price"      .= msgPrice
    ]
  toJSON ChangeLimitMsg{..} = object $
    [ "type"       .= ("change" :: Text)
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "sequence"   .= msgSequence
    , "order_id"   .= msgOrderId
    , "side"       .= msgSide
    , "new_size"   .= msgNewSize
    , "old_size"   .= msgOldSize
    ] ++ case msgMaybePrice of
           Nothing -> []
           Just  p -> ["price" .= p]
  toJSON ChangeMarketMsg{..} = object
    [ "type"       .= ("change" :: Text)
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "sequence"   .= msgSequence
    , "order_id"   .= msgOrderId
    , "side"       .= msgSide
    , "new_funds"  .= msgNewFunds
    , "old_funds"  .= msgOldFunds
    ]
  toJSON ReceivedLimitMsg{..} = object (
    [ "type"       .= ("received" :: Text)
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "sequence"   .= msgSequence
    , "order_id"   .= msgOrderId
    , "side"       .= msgSide
    , "size"       .= msgSize
    , "price"      .= msgPrice
    , "order_type" .= Limit
    ] ++ clientID)
        where
            clientID = case msgClientOid of
                Nothing -> []
                Just ci -> ["client_oid" .= msgClientOid ]
  toJSON ReceivedMarketMsg{..} = object (
    ["type"       .= ("received" :: Text)
    , "time"       .= msgTime
    , "product_id" .= msgProductId
    , "sequence"   .= msgSequence
    , "order_id"   .= msgOrderId
    , "side"       .= msgSide
    , "order_type" .= Market
    ] ++ clientID ++ size ++ funds)
        where
            clientID = case msgClientOid of
                Nothing -> []
                Just ci -> ["client_oid" .= msgClientOid ]
            (size,funds) = case msgMarketBounds of
                Left  s -> (["size" .= s],[])
                Right (ms,f) -> case ms of
                            Nothing -> ( []            , ["funds" .= f] )
                            Just s' -> ( ["size" .= s'], ["funds" .= f] )
  -- TODO:
  -- toJSON Activate
