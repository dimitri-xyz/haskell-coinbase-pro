{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Coinbase.Exchange.Socket.Test where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import Data.Aeson.QQ.Simple

import Data.ByteString.Lazy (fromStrict)
import Data.UUID

import Generic.Random

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (Success)

import qualified Network.WebSockets as WS

import Coinbase.Exchange.Private
import Coinbase.Exchange.Socket
import Coinbase.Exchange.Types
import Coinbase.Exchange.Types.Core hiding (BookEntry(..))

import qualified Coinbase.Exchange.Private.Test as P

-------------------------------------
-- NOTE: [Connectivity Precondition]
--
-- Except for the parsing tests of the documented example messages, the parsing
-- tests are time-based and assume we are receiving messages during the time
-- we are connected. However, those tests are NOT FAILSAFE.
--
-- ** If no data is received, parsing succeeds and, therefore, the parsing tests succeed **
--
-- To ensure this unsafe behavior does not go unnoticed (we thinking we are
-- parsing correctly when, in fact, we are not parsing anything at all),
-- We first verify we can receive at least 20 messages (i.e. a fixed minimum number)
-- from the socket, before running the parsing tests.
-------------------------------------
tests :: ExchangeConf -> [ProductId] -> [Channel] -> TestTree
tests conf pids chans =
  testGroup "Socket" $
    -- See NOTE: [Connectivity Precondition]
    [ testCase "Do I receive messages?" (receiveSocket conf pids chans)
    , testCase "Parse Websocket Stream"
        (parseSocket conf pids chans (threadDelay $ 1000000 * 20))
    , testCase "Decode Re-Encode Decode" (reencodeSocket conf pids chans)
    ] ++
    [ propertyTests ] ++
    [ docMessageTests ]

-- A list of all WebSocket channels
allChannels :: [Channel]
allChannels = [minBound ..]

receiveSocket :: ExchangeConf -> [ProductId] -> [Channel] -> IO ()
receiveSocket conf pids chans =
  subscribe (apiType conf) pids chans $ \conn -> do
    sequence_ $ replicate 20 (receiveAndDecode conn)

-- Success: no parse errors found while running
-- Failure: a parse error is found while running
parseSocket :: ExchangeConf -> [ProductId] -> [Channel] -> IO a -> IO ()
parseSocket conf pids chans challenge =
  subscribe (apiType conf) pids chans $ \conn -> do
    waitCancelThreads challenge (forever $ receiveAndDecode conn)
    return ()

-- FIXME! there's no guarantee we are hitting all order types.
-- a more thorough test would be better.
reencodeSocket :: ExchangeConf -> [ProductId] -> [Channel] -> IO ()
reencodeSocket conf pids chans =
  subscribe (apiType conf) pids chans $ \conn -> do
    sequence_ $ replicate 1000 (decodeEncode conn)

decodeEncode :: WS.Connection -> IO ()
decodeEncode conn = do
  ds <- WS.receiveData conn
  let res = eitherDecode ds
  case res :: Either String ExchangeMessage of
    Left er -> assertFailure "Failure parsing data from exchange" >> print er
    Right received -> do
      let enc = encode received
          dec = eitherDecode enc
      if dec == res
        then return ()
        else do
          putStrLn $ "### original: " ++ show res
          putStrLn $ "### obtained: " ++ show dec
          assertFailure "decoded object is different from original"

receiveAndDecode :: WS.Connection -> IO ()
receiveAndDecode conn = do
  ds <- WS.receiveData conn
  let res = eitherDecode ds {-$ trace (show ds) -}
  case res :: Either String ExchangeMessage of
    Left er -> print er >> assertFailure "Parsing failure found"
    Right v -> return ()

waitCancelThreads :: IO a -> IO b -> IO (Either a b)
waitCancelThreads action1 action2 = do
  a <- async action1
  b <- async action2
  c <- waitEither a b
  case c of
    Left a -> cancel b
    Right b -> cancel a
  return c

--------------------------------------------------------------------------------
-- OFFLINE UNIT TESTS
--------------------------------------------------------------------------------
-- Tests of documented WebSocket messages
--
-- The quasiquoted JSON objects that follow are the officially documented
-- examples of valid messages that can be sent or received via Coinbase Pro's
-- Websocket API as described at https://docs.pro.coinbase.com/#websocket-feed
-- (retrieved 2019-11-5).
--
-- The examples below are presented in the order in which they appear in the
-- documentation. These examples should be updated if the documentation changes.
--
-- NOTE: Some of the examples in the documentation are not valid JSON: they
-- include comments, elisions, or invalid syntax such as trailing commas.
-- They're modified here just enough to make them resemble the messages actually
-- produced by the API.

docMessageTests =
  testGroup
    "Test parsing of example messages from official documentation"
    [ testCase "Parse documented \"error\" message (`ErrorMsg`)" $
        fromJSON docErrorMsg @?= Success expectedErrorMsg
  -- , testCase "Parse documented \"subscribe\" message (`Subscribe`)" $
  --     fromJSON docSubscribe @?= Success expectedSubscribe
    , testCase
        "Parse documented \"subscriptions\" message (`SubscriptionsMsg`)" $
        fromJSON docSubscriptionsMsg @?= Success expectedSubscriptionsMsg
    , testCase "Parse documented \"unsubscribe\" message #1 (`Unsubscribe`)" $
        fromJSON docUnsubscribe1 @?= Success expectedUnsubscribe1
    , testCase "Parse documented \"unsubscribe\" message #2 (`Unsubscribe`)" $
        fromJSON docUnsubscribe2 @?= Success expectedUnsubscribe2
 -- , testCase "Parse documented \"subscribe\" message for "heartbeat" channel (`Subscribe`)" $
 --     fromJSON docSubscribeHeartbeat @?= Success expectedSubscribeHeartbeat
    , testCase "Parse documented \"heartbeat\" message (`HeartbeatMsg`)" $
        fromJSON docHeartbeatMsg @?= Success expectedHeartbeatMsg
 -- , testCase "Parse documented \"subscribe\" message for "status" channel (`Subscribe`)" $
 --     fromJSON docSubscribeStatus @?= Success expectedSubscribeStatus
    , testCase "Parse documented \"ticker\" message (`TickerMsg`)" $
        fromJSON docTickerMsg @?= Success expectedTickerMsg
    , testCase "Parse documented \"snapshot\" message (`L2SnapshotMsg`)" $
        fromJSON docL2SnapshotMsg @?= Success expectedL2SnapshotMsg
    , testCase
        "Parse documented \"received\" message for limit order\
        \ (`ReceivedLimitMsg`)" $
        fromJSON docReceivedLimitMsg @?= Success expectedReceivedLimitMsg
    , testCase
        "Parse documented \"received\" message for market order\
        \ (`ReceivedMarketMsg`)" $
        fromJSON docReceivedMarketMsg @?= Success expectedReceivedMarketMsg
    , testCase "Parse documented \"open\" message (`OpenMsg`)" $
        fromJSON docOpenMsg @?= Success expectedOpenMsg
    , testCase "Parse documented \"done\" message (`DoneMsg`)" $
        fromJSON docDoneMsg @?= Success expectedDoneMsg
    , testCase "Parse documented \"match\" message (`MatchMsg`)" $
        fromJSON docMatchMsg @?= Success expectedMatchMsg
    , testCase
        "Parse documented \"change\" message for limit order\
        \ (`ChangeLimitMsg`)" $
        fromJSON docChangeLimitMsg @?= Success expectedChangeLimitMsg
    ]

docErrorMsg =
  [aesonQQ|
  {
      "type": "error",
      "message": "error message"
  }
  |]

expectedErrorMsg = ErrorMsg {msgErrorMessage = "error message"}

-- FIXME: The "subscribe" message below doesn't parse because the `Subscribe`
-- constructor of our `SendExchange` type can't handle the object nested inside
-- the "channels" array. The `Subscribe` messages we produce are valid, and the
-- functionality this example shows can be reproduced by sending multiple
-- `Subscribe` messages, but we may want to support this sort of message anyway.
--
-- docSubscribe = [aesonQQ|
--   {
--       "type": "subscribe",
--       "product_ids": [
--           "ETH-USD",
--           "ETH-EUR"
--       ],
--       "channels": [
--           "level2",
--           "heartbeat",
--           {
--               "name": "ticker",
--               "product_ids": [
--                   "ETH-BTC",
--                   "ETH-USD"
--               ]
--           }
--       ]
--   }
--   |]
--
-- expectedSubscribe = undefined
-- NOTE: Aeson chokes on the commas trailing the "product_ids" arrays that are
-- present in the original (removed below). The JSON spec does not allow
-- trailing commas, and I haven't seen any in messages received via the API.
-- TODO: Ask Coinbase to fix this example?
docSubscriptionsMsg =
  [aesonQQ|
  {
      "type": "subscriptions",
      "channels": [
          {
              "name": "level2",
              "product_ids": [
                  "ETH-USD",
                  "ETH-EUR"
              ]
          },
          {
              "name": "heartbeat",
              "product_ids": [
                  "ETH-USD",
                  "ETH-EUR"
              ]
          },
          {
              "name": "ticker",
              "product_ids": [
                  "ETH-USD",
                  "ETH-EUR",
                  "ETH-BTC"
              ]
          }
      ]
  }
  |]

expectedSubscriptionsMsg =
  SubscriptionsMsg
  { msgSubscriptionsMsgs =
      [ Subscription
          Level2
          [ ProductId {unProductId = "ETH-USD"}
          , ProductId {unProductId = "ETH-EUR"}
          ]
      , Subscription
          Heartbeat
          [ ProductId {unProductId = "ETH-USD"}
          , ProductId {unProductId = "ETH-EUR"}
          ]
      , Subscription
          Ticker
          [ ProductId {unProductId = "ETH-USD"}
          , ProductId {unProductId = "ETH-EUR"}
          , ProductId {unProductId = "ETH-BTC"}
          ]
      ]
  }

docUnsubscribe1 =
  [aesonQQ|
  {
      "type": "unsubscribe",
      "product_ids": [
          "ETH-USD",
          "ETH-EUR"
      ],
      "channels": ["ticker"]
  }
  |]

expectedUnsubscribe1 =
  Unsubscribe
    [ProductId {unProductId = "ETH-USD"}, ProductId {unProductId = "ETH-EUR"}]
    [Ticker]

docUnsubscribe2 =
  [aesonQQ|
  {
      "type": "unsubscribe",
      "channels": ["heartbeat"]
  }
  |]

expectedUnsubscribe2 = Unsubscribe [] [Heartbeat]

-- TODO: The docs contain two partial example objects related to authenticated
-- "subscribe" requests and the added private fields on authenticated feed
-- messages. Tests these somehow.
--
-- {
--     "type": "open", // "received" | "open" | "done" | "match" | "change" | "activate"
--     "user_id": "5844eceecf7e803e259d0365",
--     "profile_id": "765d1549-9660-4be2-97d4-fa2d65fa3352",
--     /* ... */
-- }
--
-- {
--     "type": "subscribe",
--     "product_ids": [
--         "BTC-USD"
--     ],
--     "channels": ["full"],
--     "signature": "...",
--     "key": "...",
--     "passphrase": "...",
--     "timestamp": "..."
-- }
-- FIXME: The following example doesn't parse for the same reason as the other
-- "subscribe" examples; see comment above.
-- docSubscribeHeartbeat = [aesonQQ|
--   {
--       "type": "subscribe",
--       "channels": [{ "name": "heartbeat", "product_ids": ["ETH-EUR"] }]
--   }  |]
--
-- expectedSubscribeHeartbeat = undefined
docHeartbeatMsg =
  [aesonQQ|
  {
      "type": "heartbeat",
      "sequence": 90,
      "last_trade_id": 20,
      "product_id": "BTC-USD",
      "time": "2014-11-07T08:19:28.464459Z"
  }
  |]

expectedHeartbeatMsg =
  HeartbeatMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgSequence = Sequence {unSequence = 90}
  , msgLastTradeId = TradeId {unTradeId = 20}
  }
  where
    time = read "2014-11-07 08:19:28.464459 UTC"

-- FIXME: The following example doesn't parse for the same reason as the other
-- "subscribe" examples; see comment above.
-- docSubscribeStatus = [aesonQQ|
-- {
--     "type": "subscribe",
--     "channels": [{ "name": "status"}]
-- }
--
-- expectedSubscribeStatus = undefined
-- FIXME: The following example does not parse because the "details" objects are
-- empty. These parse into our `CurrencyDetails` type, and I don't think the
-- object is actually ever empty in messages actually produced by the API.
-- TODO: Ask Coinbase to fix this example?
-- docStatusMsg = [aesonQQ|
--   {
--       "type": "status",
--       "products": [
--           {
--               "id": "BTC-USD",
--               "base_currency": "BTC",
--               "quote_currency": "USD",
--               "base_min_size": "0.001",
--               "base_max_size": "70",
--               "base_increment": "0.00000001",
--               "quote_increment": "0.01",
--               "display_name": "BTC/USD",
--               "status": "online",
--               "status_message": null,
--               "min_market_funds": "10",
--               "max_market_funds": "1000000",
--               "post_only": false,
--               "limit_only": false,
--               "cancel_only": false
--           }
--       ],
--       "currencies": [
--           {
--               "id": "USD",
--               "name": "United States Dollar",
--               "min_size": "0.01000000",
--               "status": "online",
--               "status_message": null,
--               "max_precision": "0.01",
--               "convertible_to": ["USDC"], "details": {}
--           },
--           {
--               "id": "USDC",
--               "name": "USD Coin",
--               "min_size": "0.00000100",
--               "status": "online",
--               "status_message": null,
--               "max_precision": "0.000001",
--               "convertible_to": ["USD"], "details": {}
--           },
--           {
--               "id": "BTC",
--               "name": "Bitcoin",
--               "min_size":" 0.00000001",
--               "status": "online",
--               "status_message": null,
--               "max_precision": "0.00000001",
--               "convertible_to": []
--           }
--       ]
--   }
--   |]
--
-- expectedStatusMsg = undefined
-- TODO: Check the fields on the below message type against what the API
-- actually produces. I think this message may now have more fields than are
-- documented.
-- NOTE: Comment removed from original.
docTickerMsg =
  [aesonQQ|
  {
      "type": "ticker",
      "trade_id": 20153558,
      "sequence": 3262786978,
      "time": "2017-09-02T17:05:49.250000Z",
      "product_id": "BTC-USD",
      "price": "4388.01000000",
      "side": "buy",
      "last_size": "0.03000000",
      "best_bid": "4388",
      "best_ask": "4388.01"
  }
  |]

expectedTickerMsg =
  TickerMsg
  { msgTime = time
  , msgSequence = Sequence {unSequence = 3262786978}
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgTradeId = TradeId {unTradeId = 20153558}
  , msgPrice = 4388.01
  , msgSide = Buy
  , msgLastSize = 0.03
  , msgBestAsk = Just 4388.01
  , msgBestBid = Just 4388.0
  }
  where
    time = read "2017-09-02 17:05:49.25 UTC"

-- NOTE: The docs don't contain an example of the abbreviated version of the
-- above message sent immediately upon subscribing to the "ticker" channel
-- (which we parse into `StartTicker`).
-- TODO: Ask Coinbase to document this?
docL2SnapshotMsg =
  [aesonQQ|
  {
      "type": "snapshot",
      "product_id": "BTC-USD",
      "bids": [["10101.10", "0.45054140"]],
      "asks": [["10102.55", "0.57753524"]]
  }
  |]

expectedL2SnapshotMsg =
  L2SnapshotMsg
  { msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgAsks = [L2BookEntry 10102.55 0.57753524]
  , msgBids = [L2BookEntry 10101.1 0.4505414]
  }

docL2UpdateMsg =
  [aesonQQ|
  {
    "type": "l2update",
    "product_id": "BTC-USD",
    "time": "2019-08-14T20:42:27.265Z",
    "changes": [
      [
        "buy",
        "10101.80000000",
        "0.162567"
      ]
    ]
  }
  |]

expectedL2UpdateMsg =
  L2UpdateMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgChanges = [BookChange Buy 10101.8 0.162567]
  }
  where
    time = read "2019-08-14 20:42:27.265 UTC"

docReceivedLimitMsg =
  [aesonQQ|
  {
      "type": "received",
      "time": "2014-11-07T08:19:27.028459Z",
      "product_id": "BTC-USD",
      "sequence": 10,
      "order_id": "d50ec984-77a8-460a-b958-66f114b0de9b",
      "size": "1.34",
      "price": "502.1",
      "side": "buy",
      "order_type": "limit"
  }
  |]

expectedReceivedLimitMsg =
  ReceivedLimitMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgSequence = Sequence {unSequence = 10}
  , msgOrderId = OrderId {unOrderId = orderId}
  , msgSide = Buy
  , msgClientOid = Nothing
  , msgPrice = 502.1
  , msgSize = 1.34
  }
  where
    time = read "2014-11-07 08:19:27.028459 UTC"
    orderId = read "d50ec984-77a8-460a-b958-66f114b0de9b"

docReceivedMarketMsg =
  [aesonQQ|
  {
      "type": "received",
      "time": "2014-11-09T08:19:27.028459Z",
      "product_id": "BTC-USD",
      "sequence": 12,
      "order_id": "dddec984-77a8-460a-b958-66f114b0de9b",
      "funds": "3000.234",
      "side": "buy",
      "order_type": "market"
  }
  |]

expectedReceivedMarketMsg =
  ReceivedMarketMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgSequence = Sequence {unSequence = 12}
  , msgOrderId = OrderId {unOrderId = orderId}
  , msgSide = Buy
  , msgClientOid = Nothing
  , msgMarketBounds = Right (Nothing, 3000.234)
  }
  where
    time = read "2014-11-09 08:19:27.028459 UTC"
    orderId = read "dddec984-77a8-460a-b958-66f114b0de9b"

docOpenMsg =
  [aesonQQ|
  {
      "type": "open",
      "time": "2014-11-07T08:19:27.028459Z",
      "product_id": "BTC-USD",
      "sequence": 10,
      "order_id": "d50ec984-77a8-460a-b958-66f114b0de9b",
      "price": "200.2",
      "remaining_size": "1.00",
      "side": "sell"
  }
  |]

expectedOpenMsg =
  OpenMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgSequence = Sequence {unSequence = 10}
  , msgOrderId = OrderId {unOrderId = orderId}
  , msgSide = Sell
  , msgRemainingSize = 1.0
  , msgPrice = 200.2
  }
  where
    time = read "2014-11-07 08:19:27.028459 UTC"
    orderId = read "d50ec984-77a8-460a-b958-66f114b0de9b"

-- NOTE: Comment removed from original.
docDoneMsg =
  [aesonQQ|
  {
      "type": "done",
      "time": "2014-11-07T08:19:27.028459Z",
      "product_id": "BTC-USD",
      "sequence": 10,
      "price": "200.2",
      "order_id": "d50ec984-77a8-460a-b958-66f114b0de9b",
      "reason": "filled",
      "side": "sell",
      "remaining_size": "0"
  }
  |]

expectedDoneMsg =
  DoneMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgSequence = Sequence {unSequence = 10}
  , msgOrderId = OrderId {unOrderId = orderId}
  , msgSide = Sell
  , msgReason = Filled
  , msgMaybePrice = Just 200.2
  , msgMaybeRemSize = Just 0.0
  }
  where
    time = read "2014-11-07 08:19:27.028459 UTC"
    orderId = read "d50ec984-77a8-460a-b958-66f114b0de9b"

docMatchMsg =
  [aesonQQ|
  {
      "type": "match",
      "trade_id": 10,
      "sequence": 50,
      "maker_order_id": "ac928c66-ca53-498f-9c13-a110027a60e8",
      "taker_order_id": "132fb6ae-456b-4654-b4e0-d681ac05cea1",
      "time": "2014-11-07T08:19:27.028459Z",
      "product_id": "BTC-USD",
      "size": "5.23512",
      "price": "400.23",
      "side": "sell"
  }
  |]

expectedMatchMsg =
  MatchMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgSequence = Sequence {unSequence = 50}
  , msgSide = Sell
  , msgTradeId = TradeId {unTradeId = 10}
  , msgMakerOrderId = OrderId {unOrderId = makerOrderId}
  , msgTakerOrderId = OrderId {unOrderId = takerOrderId}
  , msgSize = 5.23512
  , msgPrice = 400.23
  }
  where
    time = read "2014-11-07 08:19:27.028459 UTC"
    makerOrderId = read "ac928c66-ca53-498f-9c13-a110027a60e8"
    takerOrderId = read "132fb6ae-456b-4654-b4e0-d681ac05cea1"

-- FIXME: There are additional fields in "match" messages when authenticated.
-- Test the following documented fields; presumably there are corresponding
-- "maker" fields?
--
-- taker_user_id: "5844eceecf7e803e259d0365",
-- user_id: "5844eceecf7e803e259d0365",
-- taker_profile_id: "765d1549-9660-4be2-97d4-fa2d65fa3352",
-- profile_id: "765d1549-9660-4be2-97d4-fa2d65fa3352"-
docChangeLimitMsg =
  [aesonQQ|
  {
      "type": "change",
      "time": "2014-11-07T08:19:27.028459Z",
      "sequence": 80,
      "order_id": "ac928c66-ca53-498f-9c13-a110027a60e8",
      "product_id": "BTC-USD",
      "new_size": "5.23512",
      "old_size": "12.234412",
      "price": "400.23",
      "side": "sell"
  }
  |]

expectedChangeLimitMsg =
  ChangeLimitMsg
  { msgTime = time
  , msgProductId = ProductId {unProductId = "BTC-USD"}
  , msgSequence = Sequence {unSequence = 80}
  , msgOrderId = OrderId {unOrderId = orderId}
  , msgSide = Sell
  , msgMaybePrice = Just 400.23
  , msgNewSize = 5.23512
  , msgOldSize = 12.234412
  }
  where
    time = read "2014-11-07 08:19:27.028459 UTC"
    orderId = read "ac928c66-ca53-498f-9c13-a110027a60e8"

-- NOTE: The above example "change" message for a limit order is shown twice in
-- the docs, and there is no example of a "change" message for a market order.
-- Presumably the second example should document a "change" message for a market
-- order.
--TODO: Ask Coinbase to fix this?
--TODO: `ActivateMsg` isn't yet implemented
docActivateMsg =
  [aesonQQ|
  {
    "type": "activate",
    "product_id": "test-product",
    "timestamp": "1483736448.299000",
    "user_id": "12",
    "profile_id": "30000727-d308-cf50-7b1c-c06deb1934fc",
    "order_id": "7b52009b-64fd-0a2a-49e6-d8a939753077",
    "stop_type": "entry",
    "side": "buy",
    "stop_price": "80",
    "size": "2",
    "funds": "50",
    "taker_fee_rate": "0.0025",
    "private": true
  }
  |]

expectedActivageMsg = undefined

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- PROPERTY TESTS
--
-- These tests use QuickCheck to verify the Aeson instances for all types
-- involved in messages sent and received via the WebSocket API.
propertyTests :: TestTree
propertyTests =
  testGroup
    "QuickCheck property tests"
    [ testProperty "Aeson instances are inverses (`Channel`)" $
      (decodeInvertsEncode :: Channel -> Bool)
    , testProperty "Aeson instances are inverses (`Subscription`)" $
      (decodeInvertsEncode :: Subscription -> Bool)
    , testProperty "Aeson instances are inverses (`Status Currency`)" $
      (decodeInvertsEncode :: Status Currency -> Bool)
    , testProperty "Aeson instances are inverses (`Status Product`)" $
      (decodeInvertsEncode :: Status Product -> Bool)
    , testProperty "Aeson instances are inverses (`Currency`)" $
      (decodeInvertsEncode :: Currency -> Bool)
    , testProperty "Aeson instances are inverses (`Product`)" $
      (decodeInvertsEncode :: Product -> Bool)
    , testProperty "Aeson instances are inverses (`CurrencyDetails`)" $
      (decodeInvertsEncode :: CurrencyDetails -> Bool)
    , testProperty "Aeson instances are inverses (`L2BookEntry`)" $
      (decodeInvertsEncode :: L2BookEntry -> Bool)
    , testProperty "Aeson instances are inverses (`BookChange`)" $
      (decodeInvertsEncode :: BookChange -> Bool)
    , testProperty "Aeson instances are inverses (`ExchangeMessage`)" $
      (decodeInvertsEncode :: ExchangeMessage -> Bool)
    , testProperty "Aeson instances are inverses (`SendExchangeMessage`)" $
      (decodeInvertsEncode :: SendExchangeMessage -> Bool)
    ]

decodeInvertsEncode :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
decodeInvertsEncode x =
  case (decode $ encode $ x) of
    Just x' -> x == x'
    Nothing -> False

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- `Arbitrary` instances for types defined in `... .Socket.Types`, for use
-- in property tests using QuickCheck

instance Arbitrary Channel where
  arbitrary = genericArbitraryU

instance Arbitrary Subscription where
  arbitrary = genericArbitraryU

instance Arbitrary (Status Currency) where
  arbitrary = genericArbitraryU

instance Arbitrary (Status Product) where
  arbitrary = genericArbitraryU

instance Arbitrary Product where
  arbitrary = genericArbitraryU

instance Arbitrary Currency where
  arbitrary = genericArbitraryU

instance Arbitrary CurrencyDetails where
  arbitrary = genericArbitraryU

instance Arbitrary L2BookEntry where
  arbitrary = genericArbitraryU

instance Arbitrary BookChange where
  arbitrary = genericArbitraryU

instance Arbitrary SendExchangeMessage where
  arbitrary = genericArbitraryU

instance Arbitrary ExchangeMessage where
  arbitrary = genericArbitraryU

--------------------------------------------------------------------------------
-- `Arbitrary` instances for types defined in `... .MarketData.Types`
-- FIXME: These should be moved to the module in which these types are tested,
-- but the types themselves should probably move to `... .Core.Types` or
-- `... Core`.
instance Arbitrary Size where
  arbitrary = genericArbitraryU

instance Arbitrary Price where
  arbitrary = genericArbitraryU

instance Arbitrary Cost where
  arbitrary = genericArbitraryU

instance Arbitrary CurrencyId where
  arbitrary = genericArbitraryU

instance Arbitrary ProductId where
  arbitrary = genericArbitraryU

instance Arbitrary Side where
  arbitrary = genericArbitraryU

instance Arbitrary CoinScientific where
  arbitrary = genericArbitraryU

instance Arbitrary Sequence where
  arbitrary = genericArbitraryU

instance Arbitrary TradeId where
  arbitrary = genericArbitraryU

instance Arbitrary OrderId where
  arbitrary = genericArbitraryU

instance Arbitrary ClientOrderId where
  arbitrary = genericArbitraryU

instance Arbitrary Reason where
  arbitrary = genericArbitraryU
