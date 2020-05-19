# Haskell + Coinbase Pro

This library provides an unofficial Haskell client for the [Coinbase Pro API](https://docs.pro.coinbase.com). 

The goal is to provide basic functionality for automated trading in Haskell on the Coinbase Pro exchange. We intend to implement the full functionality of both the public and authenticated REST and WebSocket APIs, but development is currently very much incomplete.

Use of this software is provided under an MIT license, on an "as-is" basis, and without warranty. Be aware that this software is a work in progress and expect unimplemented or broken functionality, including bugs that may cause financial losses if used with a funded account on Coinbase Pro's live trading server.

If you do encounter a bug, please report it on the [issue tracker](https://github.com/dimitri-xyz/haskell-coinbase-pro/issues). Feedback, feature requests, and pull requests are welcome.

Because there is no publicly available FIX engine written in Haskell, implementing the FIX API is beyond the scope of this project. Note also that this library does not implement any functionality provided by the [other Coinbase APIs](https://developers.coinbase.com) for wallets and e-commerce.

## Installation

An up-to-date version of this library is not currently available on Hackage. To install it, download the source from this repository and build with `cabal-install` or `stack` in the usual way:

### Download and build with cabal

```bash
> git clone https://github.com/dimitri-xyz/haskell-coinbase-pro.git
  ...
> cd haskell-coinbase-pro
> cabal install
  ...
> cabal build
  ...
```

### Download and build with stack

```bash
> git clone https://github.com/dimitri-xyz/haskell-coinbase-pro.git
  ...
> cd haskell-coinbase-pro
> stack build
  ...
```

## Configuration and Basic Usage

The client reads several environment variables at runtime to authenticate private API calls and to choose whether to connect to Coinbase Pro's sandbox server (for testing the API with fake money) or to the live server (for trading with real money).

Set `COINBASE_PRO_SANDBOX` to `TRUE` to connect to the sandbox server, or to `FALSE` to connect the live server; e.g., in `bash`, run the this command to run the client on the sandbox server:

```bash
> export COINBASE_PRO_SANDBOX=TRUE # Use the sandbox server
```

Always test code on the sandbox server before running it on the live server.

To make authenticated private API calls, users must provide their API key, secret, and passphrase. (To generate these credentials for a sandbox or live account, see the "API Keys" section of the account settings at, respectively, https://public.sandbox.pro.coinbase.com or https://pro.coinbase.com). Set the `COINBASE_PRO_KEY`, `COINBASE_PRO_SECRET` and `COINBASE_PRO_PASSPHRASE` variables appropriately; e.g., in ``bash``:

```bash
> export COINBASE_PRO_KEY=<your API key>
> export COINBASE_PRO_SECRET=<your API secret>
> export COINBASE_PRO_PASSPHRASE=<your API passphrase>
 ```

Provide credentials for the sandbox server if `COINBASE_PRO_SANDBOX` is set to `TRUE`, or credentials for the live server if it's set to `FALSE`.

After setting these environment variables, you can use `configure` to produce an `ExchangeConf` value. In GHCi:

```haskell
> conf <- configure
```

### REST API

Use the `runExchange` action with this configuration to call the REST API. For example, to fetch the top of the order book for the BTC-USD market, run `getTopOfBook` with the appropriate product ID:

```haskell
> :set -XOverloadedStrings
> runExchange conf $ getTopOfBook (ProductId "BTC-USD")
```
This call returns a value of type `Either ExchangeFailure (Book Aggregate)` in `IO`. Here the successful case of a `Right (Book {...})` provides an order book with order sizes aggregated by price.

The `runExchange` action is a type-specialized version of the more general `runExchangeT` monad transformer, which allows the user to choose alternative types for representing errors, etc.

### WebSocket API

To subscribe to streaming market data via the WebSocket API, extract the `ApiType` from the `ExchangeConf` and pass it to `subscribe` along with a list of product IDs of the relevant markets, a list of channels to subscribe to, and a WebSocket `ClientApp` to consume received data:

```haskell
> :set -XOverloadedStrings
> :set +m
> import Data.ByteString
> import Network.WebSockets
> :{
| subscribe (apiType conf) [ProductId "BTC-USD"] [Heartbeat, Ticker, User] $ \conn ->
|   forever $ do
|     data <- receiveData conn
|     print (data :: ByteString)
| }:

```

This example code prints the raw JSON sent by the server. See the documentation for the [`websockets` package](http://hackage.haskell.org/package/websockets) for details of working with WebSockets.

Note that `subscribe` requests data on all of the specified channels for all of the specified markets. Call `subscribe` multiple times to request data on different channels for different markets.
 
## Related Projects

This project continues development begun at [AndrewRademacher/coinbase-exchange](https://github.com/AndrewRademacher/coinbase-exchange), which is no longer maintained. There is another Haskell client under current development at [mdunnio/coinbase-pro](https://github.com/mdunnio/coinbase-pro).
