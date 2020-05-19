{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Coinbase.Exchange.Private
    ( getAccountList
    , getAccount
    , getAccountLedger
    , getAccountHolds

    , createOrder
    , cancelOrder
    , cancelAllOrders
    , getOrderList
    , getOrder

    , getFills

    , createTransfer
    , createCryptoWithdrawal

    , createReport
    , getReportStatus

    , module Coinbase.Exchange.Types.Private
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Catch
import           Data.Char
import           Data.List
import qualified Data.Text                       as T
import           Data.UUID

import           Coinbase.Exchange.Rest
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Private

-- Accounts

getAccountList :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => m [Account]
getAccountList = coinbaseGet True "/accounts" voidBody

getAccount :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
           => AccountId -> m Account
getAccount (AccountId i) = coinbaseGet True ("/accounts/" ++ toString i) voidBody

getAccountLedger :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                 => AccountId -> m [Entry]
getAccountLedger (AccountId i) = coinbaseGet True ("/accounts/" ++ toString i ++ "/ledger") voidBody

getAccountHolds :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => AccountId -> m [Hold]
getAccountHolds (AccountId i) = coinbaseGet True ("/accounts/" ++ toString i ++ "/holds") voidBody

-- Orders

createOrder :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => NewOrder -> m OrderId
createOrder = liftM ocId . coinbasePost True "/orders" . Just

cancelOrder :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
            => OrderId -> m ()
cancelOrder (OrderId o) = coinbaseDeleteDiscardBody True ("/orders/" ++ toString o) voidBody

cancelAllOrders :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
                => Maybe ProductId -> m [OrderId]
cancelAllOrders prodId = coinbaseDelete True ("/orders" ++ opt prodId) voidBody
    where opt Nothing   = ""
          opt (Just id) = "?product_id=" ++ T.unpack (unProductId id)

getOrderList :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => [OrderStatus] -> m [Order]
getOrderList os = coinbaseGet True ("/orders?" ++ query os) voidBody
    where query [] = "status=open&status=pending&status=active"
          query xs = intercalate "&" $ map (\x -> "status=" ++ map toLower (show x)) xs

getOrder :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => OrderId -> m Order
getOrder (OrderId o) = coinbaseGet True ("/orders/" ++ toString o) voidBody

-- Fills

getFills :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
         => Maybe OrderId -> Maybe ProductId -> m [Fill]
getFills moid mpid = coinbaseGet True ("/fills?" ++ oid ++ "&" ++ pid) voidBody
    where oid = case moid of Just  v -> "order_id=" ++ toString (unOrderId v)
                             Nothing -> ""
          pid = case mpid of Just  v -> "product_id=" ++ T.unpack (unProductId v)
                             Nothing -> ""

-- Transfers

createTransfer :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
               => TransferToCoinbase -> m TransferToCoinbaseResponse
createTransfer = coinbasePost True "/transfers" . Just

createCryptoWithdrawal
    :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
    => CryptoWithdrawal
    -> m CryptoWithdrawalResp
createCryptoWithdrawal = coinbasePost True "/withdrawals/crypto" . Just

-- Reports

createReport :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => ReportRequest -> m ReportInfo
createReport = coinbasePost True "/reports" . Just

getReportStatus :: (MonadIO m, MonadThrow m, MonadReader ExchangeConf m, MonadError ExchangeFailure m)
             => ReportId -> m ReportInfo
getReportStatus (ReportId r) = coinbaseGet True ("/reports/" ++ toString r) voidBody
