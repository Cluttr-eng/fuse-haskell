{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module Fuse.API
  ( -- * Client and Server
    Config(..)
  , FuseBackend(..)
  , createFuseClient
  , runFuseServer
  , runFuseMiddlewareServer
  , runFuseClient
  , runFuseClientWithManager
  , callFuse
  , FuseClient
  , FuseClientError(..)
  -- ** Servant
  , FuseAPI
  -- ** Plain WAI Application
  , serverWaiApplicationFuse
  -- ** Authentication
  , FuseAuth(..)
  , clientAuth
  , Protected
  ) where

import           Fuse.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.ByteString                    (ByteString)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth   (AuthHandler, AuthServerData, mkAuthHandler)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for Fuse.
type FuseAPI
    =    Protected :> "v1" :> "asset_report" :> "create" :> ReqBody '[JSON] CreateAssetReportRequest :> Verb 'POST 200 '[JSON] CreateAssetReportResponse -- 'createAssetReport' route
    :<|> Protected :> "v1" :> "link" :> "token" :> ReqBody '[JSON] CreateLinkTokenRequest :> Verb 'POST 200 '[JSON] CreateLinkTokenResponse -- 'createLinkToken' route
    :<|> Protected :> "v1" :> "session" :> ReqBody '[JSON] CreateSessionRequest :> Verb 'POST 200 '[JSON] CreateSessionResponse -- 'createSession' route
    :<|> Protected :> "v1" :> "financial_connections" :> "public_token" :> "exchange" :> ReqBody '[JSON] ExchangeFinancialConnectionsPublicTokenRequest :> Verb 'POST 200 '[JSON] ExchangeFinancialConnectionsPublicTokenResponse -- 'exchangePublicToken' route
    :<|> Protected :> "v1" :> "asset_report" :> ReqBody '[JSON] GetAssetReportRequest :> Verb 'POST 200 '[JSON] GetAssetReportResponse -- 'getAssetReport' route
    :<|> Protected :> "v1" :> "financial_connections" :> "accounts" :> "details" :> ReqBody '[JSON] GetFinancialConnectionsAccountDetailsRequest :> Verb 'POST 200 '[JSON] GetFinancialConnectionsAccountDetailsResponse -- 'getFinancialConnectionsAccountDetails' route
    :<|> Protected :> "v1" :> "financial_connections" :> "accounts" :> ReqBody '[JSON] GetFinancialConnectionsAccountsRequest :> Verb 'POST 200 '[JSON] GetFinancialConnectionsAccountsResponse -- 'getFinancialConnectionsAccounts' route
    :<|> Protected :> "v1" :> "financial_connections" :> "balances" :> ReqBody '[JSON] GetFinancialConnectionsBalanceRequest :> Verb 'POST 200 '[JSON] GetFinancialConnectionsAccountBalanceResponse -- 'getFinancialConnectionsBalances' route
    :<|> Protected :> "v1" :> "financial_connections" :> "owners" :> ReqBody '[JSON] GetFinancialConnectionsOwnersRequest :> Verb 'POST 200 '[JSON] GetFinancialConnectionsOwnersResponse -- 'getFinancialConnectionsOwners' route
    :<|> Protected :> "v1" :> "financial_connections" :> "transactions" :> ReqBody '[JSON] GetTransactionsRequest :> Verb 'POST 200 '[JSON] GetTransactionsResponse -- 'getFinancialConnectionsTransactions' route
    :<|> Protected :> "v1" :> "financial_connections" :> "investments" :> "holdings" :> ReqBody '[JSON] GetInvestmentHoldingsRequest :> Verb 'POST 200 '[JSON] GetInvestmentHoldingsResponse -- 'getInvestmentHoldings' route
    :<|> Protected :> "v1" :> "financial_connections" :> "investments" :> "transactions" :> ReqBody '[JSON] GetInvestmentTransactionsRequest :> Verb 'POST 200 '[JSON] GetInvestmentTransactionsResponse -- 'getInvestmentTransactions' route
    :<|> Protected :> "v1" :> "financial_connections" :> "sync" :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] SyncFinancialConnectionsDataResponse -- 'syncFinancialConnectionsData' route
    :<|> Protected :> "v1" :> "financial_connections" :> "transactions" :> "sync" :> ReqBody '[JSON] SyncTransactionsRequest :> Verb 'POST 200 '[JSON] SyncTransactionsResponse -- 'syncFinancialConnectionsTransactions' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype FuseClientError = FuseClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for Fuse.
-- The backend can be used both for the client and the server. The client generated from the Fuse OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createFuseClient@). Alternatively, provided
-- a backend, the API can be served using @runFuseMiddlewareServer@.
data FuseBackend a m = FuseBackend
  { createAssetReport :: a -> CreateAssetReportRequest -> m CreateAssetReportResponse{- ^ Use this endpoint to generate an Asset Report for a user. -}
  , createLinkToken :: a -> CreateLinkTokenRequest -> m CreateLinkTokenResponse{- ^ Create a link token to start the process of a user connecting to a specific financial institution. -}
  , createSession :: a -> CreateSessionRequest -> m CreateSessionResponse{- ^ Creates a session that returns a client_secret which is required as a parameter when initializing the Fuse SDK. -}
  , exchangePublicToken :: a -> ExchangeFinancialConnectionsPublicTokenRequest -> m ExchangeFinancialConnectionsPublicTokenResponse{- ^ API to exchange a public token for an access token and financial connection id -}
  , getAssetReport :: a -> GetAssetReportRequest -> m GetAssetReportResponse{- ^ Retrieves the Asset Report in JSON format. -}
  , getFinancialConnectionsAccountDetails :: a -> GetFinancialConnectionsAccountDetailsRequest -> m GetFinancialConnectionsAccountDetailsResponse{- ^  -}
  , getFinancialConnectionsAccounts :: a -> GetFinancialConnectionsAccountsRequest -> m GetFinancialConnectionsAccountsResponse{- ^  -}
  , getFinancialConnectionsBalances :: a -> GetFinancialConnectionsBalanceRequest -> m GetFinancialConnectionsAccountBalanceResponse{- ^  -}
  , getFinancialConnectionsOwners :: a -> GetFinancialConnectionsOwnersRequest -> m GetFinancialConnectionsOwnersResponse{- ^  -}
  , getFinancialConnectionsTransactions :: a -> GetTransactionsRequest -> m GetTransactionsResponse{- ^  -}
  , getInvestmentHoldings :: a -> GetInvestmentHoldingsRequest -> m GetInvestmentHoldingsResponse{- ^  -}
  , getInvestmentTransactions :: a -> GetInvestmentTransactionsRequest -> m GetInvestmentTransactionsResponse{- ^  -}
  , syncFinancialConnectionsData :: a -> Value -> m SyncFinancialConnectionsDataResponse{- ^ Call this endpoint upon receiving a SYNC_REQUIRED webhook. This will keep the financial connections data up to date. -}
  , syncFinancialConnectionsTransactions :: a -> SyncTransactionsRequest -> m SyncTransactionsResponse{- ^  -}
  }

-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for Fuse.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FuseAuth = FuseAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }

newtype FuseClient a = FuseClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative FuseClient where
  pure x = FuseClient (\_ -> pure x)
  (FuseClient f) <*> (FuseClient x) =
    FuseClient (\env -> f env <*> x env)

instance Monad FuseClient where
  (FuseClient a) >>= f =
    FuseClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO FuseClient where
  liftIO io = FuseClient (\_ -> liftIO io)

createFuseClient :: FuseBackend AuthClient FuseClient
createFuseClient = FuseBackend{..}
  where
    ((coerce -> createAssetReport) :<|>
     (coerce -> createLinkToken) :<|>
     (coerce -> createSession) :<|>
     (coerce -> exchangePublicToken) :<|>
     (coerce -> getAssetReport) :<|>
     (coerce -> getFinancialConnectionsAccountDetails) :<|>
     (coerce -> getFinancialConnectionsAccounts) :<|>
     (coerce -> getFinancialConnectionsBalances) :<|>
     (coerce -> getFinancialConnectionsOwners) :<|>
     (coerce -> getFinancialConnectionsTransactions) :<|>
     (coerce -> getInvestmentHoldings) :<|>
     (coerce -> getInvestmentTransactions) :<|>
     (coerce -> syncFinancialConnectionsData) :<|>
     (coerce -> syncFinancialConnectionsTransactions) :<|>
     _) = client (Proxy :: Proxy FuseAPI)

-- | Run requests in the FuseClient monad.
runFuseClient :: Config -> FuseClient a -> ExceptT ClientError IO a
runFuseClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runFuseClientWithManager manager clientConfig cl

-- | Run requests in the FuseClient monad using a custom manager.
runFuseClientWithManager :: Manager -> Config -> FuseClient a -> ExceptT ClientError IO a
runFuseClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a FuseClientError
callFuse
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> FuseClient a -> m a
callFuse env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (FuseClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the Fuse server at the provided host and port.
runFuseServer
  :: (MonadIO m, MonadThrow m)
  => Config -> FuseAuth -> FuseBackend AuthServer (ExceptT ServerError IO) -> m ()
runFuseServer config auth backend = runFuseMiddlewareServer config requestMiddlewareId auth backend

-- | Run the Fuse server at the provided host and port.
runFuseMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> FuseAuth -> FuseBackend AuthServer (ExceptT ServerError IO) -> m ()
runFuseMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationFuse auth backend

-- | Plain "Network.Wai" Application for the Fuse server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationFuse :: FuseAuth -> FuseBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationFuse auth backend = serveWithContextT (Proxy :: Proxy FuseAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend FuseBackend{..} =
      (coerce createAssetReport :<|>
       coerce createLinkToken :<|>
       coerce createSession :<|>
       coerce exchangePublicToken :<|>
       coerce getAssetReport :<|>
       coerce getFinancialConnectionsAccountDetails :<|>
       coerce getFinancialConnectionsAccounts :<|>
       coerce getFinancialConnectionsBalances :<|>
       coerce getFinancialConnectionsOwners :<|>
       coerce getFinancialConnectionsTransactions :<|>
       coerce getInvestmentHoldings :<|>
       coerce getInvestmentTransactions :<|>
       coerce syncFinancialConnectionsData :<|>
       coerce syncFinancialConnectionsTransactions :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Fuse-Api-Key" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Fuse-Api-Key")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Fuse-Client-Id" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Fuse-Client-Id")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Mx-Api-Key" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Mx-Api-Key")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Mx-Client-Id" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Mx-Client-Id")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Plaid-Client-Id" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Plaid-Client-Id")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Plaid-Secret" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Plaid-Secret")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Teller-Application-Id" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Teller-Application-Id")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Teller-Certificate" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Teller-Certificate")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Teller-Private-Key" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Teller-Private-Key")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Teller-Signing-Secret" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Teller-Signing-Secret")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FuseAuth -> AuthHandler Request AuthServer
authHandler FuseAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Teller-Token-Signing-Key" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "Teller-Token-Signing-Key")

serverContext :: FuseAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
