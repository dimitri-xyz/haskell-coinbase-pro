{-# LANGUAGE OverloadedStrings #-}

module Coinbase.Exchange.Socket
    ( subscribe
    , module Coinbase.Exchange.Types.Socket
    ) where

-------------------------------------------------------------------------------
import           Data.Aeson
import           Network.Socket
import qualified Network.WebSockets             as WS
import           Wuss
-------------------------------------------------------------------------------
import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core
import           Coinbase.Exchange.Types.Socket
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
subscribe :: ApiType -> [ProductId] -> [Channel] -> WS.ClientApp a -> IO a
subscribe atype pids chans app = withSocketsDo $
        runSecureClient location 443 "/" $ \conn -> do
            WS.sendTextData conn $ encode (Subscribe pids chans)
            app conn
    where location = case atype of Sandbox -> sandboxSocket
                                   Live    -> liveSocket
