{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Fuse.Types (
  CreateAssetReportRequest (..),
  CreateAssetReportResponse (..),
  CreateLinkTokenRequest (..),
  CreateLinkTokenRequestMx (..),
  CreateLinkTokenRequestPlaid (..),
  CreateLinkTokenRequestPlaidConfig (..),
  CreateLinkTokenResponse (..),
  CreateSessionRequest (..),
  CreateSessionRequestMx (..),
  CreateSessionRequestPlaid (..),
  CreateSessionResponse (..),
  ExchangeFinancialConnectionsPublicTokenRequest (..),
  ExchangeFinancialConnectionsPublicTokenResponse (..),
  FinancialConnectionsAccount (..),
  FinancialConnectionsAccountBalance (..),
  FinancialConnectionsAccountDetails (..),
  FinancialConnectionsAccountDetailsAch (..),
  FinancialConnectionsAccountInstitution (..),
  FinancialConnectionsHolding (..),
  FinancialConnectionsInvestmentAccount (..),
  FinancialConnectionsInvestmentSecurity (..),
  FinancialConnectionsInvestmentTransaction (..),
  FinancialConnectionsInvestmentTransactionDataInner (..),
  FinancialConnectionsOwner (..),
  FinancialConnectionsOwnerAddressesInner (..),
  FinancialConnectionsOwnerAddressesInnerData (..),
  FinancialConnectionsOwnerEmailsInner (..),
  FinancialConnectionsOwnerNamesInner (..),
  FinancialConnectionsOwnerPhoneNumbersInner (..),
  GetAssetReportRequest (..),
  GetAssetReportResponse (..),
  GetAssetReportResponseReport (..),
  GetAssetReportResponseReportAccountsInner (..),
  GetAssetReportResponseReportAccountsInnerBalances (..),
  GetAssetReportResponseReportAccountsInnerHistoricalBalancesInner (..),
  GetFinancialConnectionsAccountBalanceResponse (..),
  GetFinancialConnectionsAccountDetailsRequest (..),
  GetFinancialConnectionsAccountDetailsResponse (..),
  GetFinancialConnectionsAccountsRequest (..),
  GetFinancialConnectionsAccountsResponse (..),
  GetFinancialConnectionsBalanceRequest (..),
  GetFinancialConnectionsOwnersRequest (..),
  GetFinancialConnectionsOwnersResponse (..),
  GetFinancialConnectionsOwnersResponseAccountsInner (..),
  GetInvestmentHoldingsRequest (..),
  GetInvestmentHoldingsResponse (..),
  GetInvestmentTransactionsRequest (..),
  GetInvestmentTransactionsResponse (..),
  GetTransactionsRequest (..),
  GetTransactionsResponse (..),
  GetTransactionsResponseDataInner (..),
  GetTransactionsResponseDataInnerMerchant (..),
  SyncFinancialConnectionsDataResponse (..),
  SyncTransactionsRequest (..),
  SyncTransactionsResponse (..),
  SyncTransactionsResponseRemovedInner (..),
  TransactionCommonModel (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data CreateAssetReportRequest = CreateAssetReportRequest
  { createAssetReportRequestAccessUnderscoretoken :: Text -- ^ Access token for the entity to create report for.
  , createAssetReportRequestDaysUnderscorerequested :: Double -- ^ The maximum integer number of days of history to include in the Asset Report
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateAssetReportRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createAssetReportRequest")
instance ToJSON CreateAssetReportRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createAssetReportRequest")


-- | 
data CreateAssetReportResponse = CreateAssetReportResponse
  { createAssetReportResponseAssetUnderscorereportUnderscoretoken :: Maybe Text -- ^ A token that can be provided to endpoints such as /asset_report or /asset_report/pdf to fetch an Asset Report.
  , createAssetReportResponseAssetUnderscorereportUnderscoreid :: Maybe Text -- ^ A unique ID identifying an Asset Report. 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateAssetReportResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createAssetReportResponse")
instance ToJSON CreateAssetReportResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createAssetReportResponse")


-- | 
data CreateLinkTokenRequest = CreateLinkTokenRequest
  { createLinkTokenRequestWebhookUnderscoreurl :: Maybe Text -- ^ The destination URL to which any webhooks should be sent.
  , createLinkTokenRequestUserUnderscoreid :: Text -- ^ An id that is unique for a user of your application.
  , createLinkTokenRequestSessionUnderscoreclientUnderscoresecret :: Text -- ^ The session client secret created from the 'Create session client secret' endpoint
  , createLinkTokenRequestMx :: Maybe CreateLinkTokenRequestMx -- ^ 
  , createLinkTokenRequestPlaid :: Maybe CreateLinkTokenRequestPlaid -- ^ 
  , createLinkTokenRequestInstitutionUnderscoreid :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateLinkTokenRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createLinkTokenRequest")
instance ToJSON CreateLinkTokenRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createLinkTokenRequest")


-- | An object specifying information about the MX configuration to use for deciding which MX supported financial institutions to display.
data CreateLinkTokenRequestMx = CreateLinkTokenRequestMx
  { createLinkTokenRequestMxConfig :: Maybe Value -- ^ Follows the same schema as MX's request a connect url(https://docs.mx.com/api#connect_request_a_url) schema. This is a stringified version of the config.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateLinkTokenRequestMx where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createLinkTokenRequestMx")
instance ToJSON CreateLinkTokenRequestMx where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createLinkTokenRequestMx")


-- | An object specifying information about the Plaid configuration to use when creating a link token. This option is required if Plaid was enabled when displaying the financial institutions to the user.
data CreateLinkTokenRequestPlaid = CreateLinkTokenRequestPlaid
  { createLinkTokenRequestPlaidConfig :: Maybe CreateLinkTokenRequestPlaidConfig -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateLinkTokenRequestPlaid where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createLinkTokenRequestPlaid")
instance ToJSON CreateLinkTokenRequestPlaid where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createLinkTokenRequestPlaid")


-- | Follows the same schema as Plaid&#39;s Link Token Create Schema(https://plaid.com/docs/api/tokens/#linktokencreate). This parameter takes a stringified version of the config. &#39;products&#39;, &#39;client_id&#39;, &#39;secret&#39;, &#39;client_user_id&#39;, &#39;webhook&#39;, &#39;institution_data&#39; and &#39;country_codes&#39; (only US supported right now) will be set by Fuse and override any values you set.
data CreateLinkTokenRequestPlaidConfig = CreateLinkTokenRequestPlaidConfig
  { createLinkTokenRequestPlaidConfigClientUnderscorename :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateLinkTokenRequestPlaidConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createLinkTokenRequestPlaidConfig")
instance ToJSON CreateLinkTokenRequestPlaidConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createLinkTokenRequestPlaidConfig")


-- | 
data CreateLinkTokenResponse = CreateLinkTokenResponse
  { createLinkTokenResponseLinkUnderscoretoken :: Maybe Text -- ^ Token needed by the frontend sdk to initiate the connection
  , createLinkTokenResponseRequestUnderscoreid :: Maybe Text -- ^ Used for debugging purposes
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateLinkTokenResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createLinkTokenResponse")
instance ToJSON CreateLinkTokenResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createLinkTokenResponse")


-- | 
data CreateSessionRequest = CreateSessionRequest
  { createSessionRequestSupportedUnderscorefinancialUnderscoreinstitutionUnderscoreaggregators :: Maybe [Text] -- ^ 
  , createSessionRequestMx :: Maybe CreateSessionRequestMx -- ^ 
  , createSessionRequestPlaid :: Maybe CreateSessionRequestPlaid -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSessionRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSessionRequest")
instance ToJSON CreateSessionRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSessionRequest")


-- | This can be left empty even if MX is in the list of supported_financial_institution_aggregators, although we would recommend having a look at what each field means to see if you want to override the default value of &#39;false&#39;. These fields are described here(https://docs.mx.com/api#core_resources_institutions_list_institutions). An object specifying information about the MX configuration to use for deciding which MX supported financial institutions to display.
data CreateSessionRequestMx = CreateSessionRequestMx
  { createSessionRequestMxSupportsUnderscoreaccountUnderscoreidentification :: Maybe Bool -- ^ 
  , createSessionRequestMxSupportsUnderscoreaccountUnderscorestatement :: Maybe Bool -- ^ 
  , createSessionRequestMxSupportsUnderscoreaccountUnderscoreverification :: Maybe Bool -- ^ 
  , createSessionRequestMxSupportsUnderscoretransactionUnderscorehistory :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSessionRequestMx where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSessionRequestMx")
instance ToJSON CreateSessionRequestMx where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSessionRequestMx")


-- | This field is REQUIRED if PLAID is in the list of supported_financial_institution_aggregators. An object specifying information about the Plaid configuration to use for deciding which Plaid supported financial institutions to display.
data CreateSessionRequestPlaid = CreateSessionRequestPlaid
  { createSessionRequestPlaidProducts :: Maybe [Text] -- ^ For a comprehensive list of supported plaid products, see https://plaid.com/docs/api/tokens/#link-token-create-request-products
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSessionRequestPlaid where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSessionRequestPlaid")
instance ToJSON CreateSessionRequestPlaid where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSessionRequestPlaid")


-- | 
data CreateSessionResponse = CreateSessionResponse
  { createSessionResponseExpiration :: Maybe Text -- ^ 4 hours from the point of creation
  , createSessionResponseClientUnderscoresecret :: Maybe Text -- ^ Token needed by the frontend sdk to start the process
  , createSessionResponseRequestUnderscoreid :: Maybe Text -- ^ Used for debugging purposes
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSessionResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSessionResponse")
instance ToJSON CreateSessionResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSessionResponse")


-- | 
data ExchangeFinancialConnectionsPublicTokenRequest = ExchangeFinancialConnectionsPublicTokenRequest
  { exchangeFinancialConnectionsPublicTokenRequestPublicUnderscoretoken :: Maybe Text -- ^ The public token created after a user connects with their financial institution
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExchangeFinancialConnectionsPublicTokenRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exchangeFinancialConnectionsPublicTokenRequest")
instance ToJSON ExchangeFinancialConnectionsPublicTokenRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exchangeFinancialConnectionsPublicTokenRequest")


-- | 
data ExchangeFinancialConnectionsPublicTokenResponse = ExchangeFinancialConnectionsPublicTokenResponse
  { exchangeFinancialConnectionsPublicTokenResponseAccessUnderscoretoken :: Maybe Text -- ^ Token used for querying data on the user
  , exchangeFinancialConnectionsPublicTokenResponseFinancialUnderscoreconnectionUnderscoreid :: Maybe Text -- ^ The id of the new financial connection. Every webhook will be sent with this id.
  , exchangeFinancialConnectionsPublicTokenResponseRequestUnderscoreid :: Maybe Text -- ^ Used for debugging purposes
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExchangeFinancialConnectionsPublicTokenResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exchangeFinancialConnectionsPublicTokenResponse")
instance ToJSON ExchangeFinancialConnectionsPublicTokenResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exchangeFinancialConnectionsPublicTokenResponse")


-- | 
data FinancialConnectionsAccount = FinancialConnectionsAccount
  { financialConnectionsAccountRemoteUnderscoreid :: Maybe Text -- ^ Remote Id of the account, ie Plaid or Teller account id
  , financialConnectionsAccountFingerprint :: Maybe Text -- ^ Uniquely identifies this account across all accounts associated with your organization. See more information here: https://letsfuse.readme.io/docs/duplicate-accounts
  , financialConnectionsAccountInstitution :: Maybe FinancialConnectionsAccountInstitution -- ^ 
  , financialConnectionsAccountLastUnderscorefour :: Maybe Text -- ^ The last four digits of the account number.
  , financialConnectionsAccountName :: Maybe Text -- ^ The account's name, ie 'My Checking'
  , financialConnectionsAccountType :: Maybe Text -- ^ The account's type e.g depository.
  , financialConnectionsAccountSubtype :: Maybe Text -- ^ The account's subtype e.g checking
  , financialConnectionsAccountBalance :: Maybe FinancialConnectionsAccountBalance -- ^ 
  , financialConnectionsAccountRemoteUnderscoredata :: Maybe Value -- ^ The exact data from the aggregator (ie plaid) that we retrieved the information from
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsAccount where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsAccount")
instance ToJSON FinancialConnectionsAccount where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsAccount")


-- | 
data FinancialConnectionsAccountBalance = FinancialConnectionsAccountBalance
  { financialConnectionsAccountBalanceAvailable :: Maybe Text -- ^ The amount of funds available to be withdrawn from the account, as determined by the financial institution Available balance may be cached and is not guaranteed to be up-to-date in realtime unless the value was returned by /financial_connections/balances.
  , financialConnectionsAccountBalanceCurrent :: Maybe Double -- ^ Amount without factoring in pending balances
  , financialConnectionsAccountBalanceIsoUnderscorecurrencyUnderscorecode :: Maybe Text -- ^ The ISO-4217 currency code of the balance.
  , financialConnectionsAccountBalanceLastUnderscoreupdatedUnderscoredate :: Maybe Text -- ^ The date of the last update to the balance.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsAccountBalance where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsAccountBalance")
instance ToJSON FinancialConnectionsAccountBalance where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsAccountBalance")


-- | 
data FinancialConnectionsAccountDetails = FinancialConnectionsAccountDetails
  { financialConnectionsAccountDetailsRemoteUnderscoreid :: Maybe Text -- ^ Remote Id of the account, ie Plaid or Teller account id
  , financialConnectionsAccountDetailsAch :: Maybe FinancialConnectionsAccountDetailsAch -- ^ 
  , financialConnectionsAccountDetailsRemoteUnderscoredata :: Maybe Value -- ^ The exact data from the aggregator (ie plaid) that we retrieved the information from
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsAccountDetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsAccountDetails")
instance ToJSON FinancialConnectionsAccountDetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsAccountDetails")


-- | 
data FinancialConnectionsAccountDetailsAch = FinancialConnectionsAccountDetailsAch
  { financialConnectionsAccountDetailsAchAccount :: Maybe Text -- ^ Account number
  , financialConnectionsAccountDetailsAchRouting :: Maybe Text -- ^ Routing number
  , financialConnectionsAccountDetailsAchWireUnderscorerouting :: Maybe Text -- ^ Wire routing number
  , financialConnectionsAccountDetailsAchBacsUnderscorerouting :: Maybe Text -- ^ BACS routing number
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsAccountDetailsAch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsAccountDetailsAch")
instance ToJSON FinancialConnectionsAccountDetailsAch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsAccountDetailsAch")


-- | 
data FinancialConnectionsAccountInstitution = FinancialConnectionsAccountInstitution
  { financialConnectionsAccountInstitutionName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsAccountInstitution where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsAccountInstitution")
instance ToJSON FinancialConnectionsAccountInstitution where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsAccountInstitution")


-- | 
data FinancialConnectionsHolding = FinancialConnectionsHolding
  { financialConnectionsHoldingCostUnderscorebasis :: Maybe Double -- ^ The original total value of the holding.
  , financialConnectionsHoldingValue :: Maybe Double -- ^ The value of the holding
  , financialConnectionsHoldingIsoUnderscorecurrencyUnderscorecode :: Maybe Text -- ^ The ISO-4217 currency code of the holding.
  , financialConnectionsHoldingQuantity :: Maybe Double -- ^ The number of units of the security involved in this transaction.
  , financialConnectionsHoldingSecurityUnderscoreid :: Maybe Text -- ^ The security_id associated with the holding.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsHolding where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsHolding")
instance ToJSON FinancialConnectionsHolding where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsHolding")


-- | 
data FinancialConnectionsInvestmentAccount = FinancialConnectionsInvestmentAccount
  { financialConnectionsInvestmentAccountRemoteUnderscoreid :: Maybe Text -- ^ Remote Id of the account, ie Plaid or Teller account id
  , financialConnectionsInvestmentAccountFingerprint :: Maybe Text -- ^ Uniquely identifies this account across all accounts associated with your organization. See more information here: https://letsfuse.readme.io/docs/duplicate-accounts
  , financialConnectionsInvestmentAccountInstitution :: Maybe FinancialConnectionsAccountInstitution -- ^ 
  , financialConnectionsInvestmentAccountLastUnderscorefour :: Maybe Text -- ^ The last four digits of the account number.
  , financialConnectionsInvestmentAccountName :: Maybe Text -- ^ The account's name, ie 'My Checking'
  , financialConnectionsInvestmentAccountType :: Maybe Text -- ^ The account's type e.g depository.
  , financialConnectionsInvestmentAccountSubtype :: Maybe Text -- ^ The account's subtype e.g checking
  , financialConnectionsInvestmentAccountBalance :: Maybe FinancialConnectionsAccountBalance -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsInvestmentAccount where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsInvestmentAccount")
instance ToJSON FinancialConnectionsInvestmentAccount where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsInvestmentAccount")


-- | 
data FinancialConnectionsInvestmentSecurity = FinancialConnectionsInvestmentSecurity
  { financialConnectionsInvestmentSecurityType :: Maybe Text -- ^ The security type of the holding.
  , financialConnectionsInvestmentSecurityCusip :: Maybe Text -- ^ 9-character CUSIP, an identifier assigned to North American securities.
  , financialConnectionsInvestmentSecurityName :: Maybe Text -- ^ A descriptive name for the security, suitable for display.
  , financialConnectionsInvestmentSecurityTickerUnderscoresymbol :: Maybe Text -- ^ The security’s trading symbol for publicly traded securities, and otherwise a short identifier if available.
  , financialConnectionsInvestmentSecurityId :: Maybe Text -- ^ A unique identity for the security
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsInvestmentSecurity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsInvestmentSecurity")
instance ToJSON FinancialConnectionsInvestmentSecurity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsInvestmentSecurity")


-- | 
data FinancialConnectionsInvestmentTransaction = FinancialConnectionsInvestmentTransaction
  { financialConnectionsInvestmentTransactionData :: Maybe [FinancialConnectionsInvestmentTransactionDataInner] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsInvestmentTransaction where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsInvestmentTransaction")
instance ToJSON FinancialConnectionsInvestmentTransaction where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsInvestmentTransaction")


-- | 
data FinancialConnectionsInvestmentTransactionDataInner = FinancialConnectionsInvestmentTransactionDataInner
  { financialConnectionsInvestmentTransactionDataInnerId :: Maybe Text -- ^ Fuse Id of the investment transaction
  , financialConnectionsInvestmentTransactionDataInnerRemoteUnderscoreinvestmentUnderscoretransactionUnderscoreid :: Maybe Text -- ^ The remote ID of the Investment transaction
  , financialConnectionsInvestmentTransactionDataInnerRemoteUnderscoreaccountUnderscoreid :: Maybe Text -- ^ Remote Account Id of the transaction, ie Plaid Account Id
  , financialConnectionsInvestmentTransactionDataInnerAmount :: Maybe Double -- ^ The complete value of the transaction. Positive values when cash is debited, e.g. purchases of stock; negative values when cash is credited, e.g. sales of stock.
  , financialConnectionsInvestmentTransactionDataInnerQuantity :: Maybe Text -- ^ The number of units of the security involved in this transaction. Positive for buy transactions; negative for sell transactions.
  , financialConnectionsInvestmentTransactionDataInnerName :: Maybe Text -- ^ The institution’s description of the transaction.
  , financialConnectionsInvestmentTransactionDataInnerType :: Maybe Text -- ^ Type of the transaction, ie buy, sell
  , financialConnectionsInvestmentTransactionDataInnerDate :: Maybe Text -- ^ Date of the transaction
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsInvestmentTransactionDataInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsInvestmentTransactionDataInner")
instance ToJSON FinancialConnectionsInvestmentTransactionDataInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsInvestmentTransactionDataInner")


-- | 
data FinancialConnectionsOwner = FinancialConnectionsOwner
  { financialConnectionsOwnerAddresses :: Maybe [FinancialConnectionsOwnerAddressesInner] -- ^ 
  , financialConnectionsOwnerEmails :: Maybe [FinancialConnectionsOwnerEmailsInner] -- ^ 
  , financialConnectionsOwnerNames :: Maybe [FinancialConnectionsOwnerNamesInner] -- ^ List of names associated with the owner
  , financialConnectionsOwnerPhoneUnderscorenumbers :: Maybe [FinancialConnectionsOwnerPhoneNumbersInner] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsOwner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsOwner")
instance ToJSON FinancialConnectionsOwner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsOwner")


-- | 
data FinancialConnectionsOwnerAddressesInner = FinancialConnectionsOwnerAddressesInner
  { financialConnectionsOwnerAddressesInnerData :: Maybe FinancialConnectionsOwnerAddressesInnerData -- ^ 
  , financialConnectionsOwnerAddressesInnerPrimary :: Maybe Bool -- ^ Indicating if it is the primary address
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsOwnerAddressesInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsOwnerAddressesInner")
instance ToJSON FinancialConnectionsOwnerAddressesInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsOwnerAddressesInner")


-- | 
data FinancialConnectionsOwnerAddressesInnerData = FinancialConnectionsOwnerAddressesInnerData
  { financialConnectionsOwnerAddressesInnerDataCity :: Maybe Text -- ^ City of the address
  , financialConnectionsOwnerAddressesInnerDataCountry :: Maybe Text -- ^ Country of the address
  , financialConnectionsOwnerAddressesInnerDataPostalUnderscorecode :: Maybe Text -- ^ Postal code of the address
  , financialConnectionsOwnerAddressesInnerDataRegion :: Maybe Text -- ^ Region of the address
  , financialConnectionsOwnerAddressesInnerDataStreet :: Maybe Text -- ^ Street of the address
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsOwnerAddressesInnerData where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsOwnerAddressesInnerData")
instance ToJSON FinancialConnectionsOwnerAddressesInnerData where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsOwnerAddressesInnerData")


-- | 
data FinancialConnectionsOwnerEmailsInner = FinancialConnectionsOwnerEmailsInner
  { financialConnectionsOwnerEmailsInnerData :: Maybe Text -- ^ Email address
  , financialConnectionsOwnerEmailsInnerPrimary :: Maybe Bool -- ^ Indicating if it is the primary email
  , financialConnectionsOwnerEmailsInnerType :: Maybe Text -- ^ Type of the email
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsOwnerEmailsInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsOwnerEmailsInner")
instance ToJSON FinancialConnectionsOwnerEmailsInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsOwnerEmailsInner")


-- | 
data FinancialConnectionsOwnerNamesInner = FinancialConnectionsOwnerNamesInner
  { financialConnectionsOwnerNamesInnerData :: Maybe Text -- ^ Name of the owner
  , financialConnectionsOwnerNamesInnerType :: Maybe Text -- ^ Type of name. Possible values are \"name\" or \"alias\"
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsOwnerNamesInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsOwnerNamesInner")
instance ToJSON FinancialConnectionsOwnerNamesInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsOwnerNamesInner")


-- | 
data FinancialConnectionsOwnerPhoneNumbersInner = FinancialConnectionsOwnerPhoneNumbersInner
  { financialConnectionsOwnerPhoneNumbersInnerData :: Maybe Text -- ^ The phone number
  , financialConnectionsOwnerPhoneNumbersInnerPrimary :: Maybe Bool -- ^ Indicating if it is the primary phone number
  , financialConnectionsOwnerPhoneNumbersInnerType :: Maybe Text -- ^ Type of the phone number
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FinancialConnectionsOwnerPhoneNumbersInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "financialConnectionsOwnerPhoneNumbersInner")
instance ToJSON FinancialConnectionsOwnerPhoneNumbersInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "financialConnectionsOwnerPhoneNumbersInner")


-- | 
data GetAssetReportRequest = GetAssetReportRequest
  { getAssetReportRequestAssetUnderscorereportUnderscoretoken :: Text -- ^ The token associated with the Asset Report to retrieve.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetAssetReportRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getAssetReportRequest")
instance ToJSON GetAssetReportRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getAssetReportRequest")


-- | 
data GetAssetReportResponse = GetAssetReportResponse
  { getAssetReportResponseReport :: Maybe GetAssetReportResponseReport -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetAssetReportResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getAssetReportResponse")
instance ToJSON GetAssetReportResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getAssetReportResponse")


-- | The Asset Report in JSON format.
data GetAssetReportResponseReport = GetAssetReportResponseReport
  { getAssetReportResponseReportAssetUnderscorereportUnderscoreid :: Maybe Text -- ^ A unique ID identifying an Asset Report.
  , getAssetReportResponseReportClientUnderscorereportUnderscoreid :: Maybe Text -- ^ An identifier you determine and submit for the Asset Report. 
  , getAssetReportResponseReportDateUnderscoregenerated :: Maybe Text -- ^ The date and time when the Asset Report was created, in ISO 8601 format
  , getAssetReportResponseReportDaysUnderscorerequested :: Maybe Int -- ^ The duration of transaction history you requested
  , getAssetReportResponseReportAccounts :: Maybe [GetAssetReportResponseReportAccountsInner] -- ^ An array of Asset Reports, one for each account in the Asset Report.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetAssetReportResponseReport where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getAssetReportResponseReport")
instance ToJSON GetAssetReportResponseReport where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getAssetReportResponseReport")


-- | 
data GetAssetReportResponseReportAccountsInner = GetAssetReportResponseReportAccountsInner
  { getAssetReportResponseReportAccountsInnerRemoteUnderscoreaccountUnderscoreid :: Maybe Text -- ^ \"The remote account ID of the account.\",
  , getAssetReportResponseReportAccountsInnerBalances :: Maybe GetAssetReportResponseReportAccountsInnerBalances -- ^ 
  , getAssetReportResponseReportAccountsInnerHistoricalUnderscorebalances :: Maybe [GetAssetReportResponseReportAccountsInnerHistoricalBalancesInner] -- ^ An array of historical balances for the account.
  , getAssetReportResponseReportAccountsInnerOwners :: Maybe [FinancialConnectionsOwner] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetAssetReportResponseReportAccountsInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getAssetReportResponseReportAccountsInner")
instance ToJSON GetAssetReportResponseReportAccountsInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getAssetReportResponseReportAccountsInner")


-- | 
data GetAssetReportResponseReportAccountsInnerBalances = GetAssetReportResponseReportAccountsInnerBalances
  { getAssetReportResponseReportAccountsInnerBalancesAvailable :: Maybe Double -- ^ Amount after factoring in pending balances
  , getAssetReportResponseReportAccountsInnerBalancesCurrent :: Maybe Double -- ^ Amount without factoring in pending balances
  , getAssetReportResponseReportAccountsInnerBalancesIsoUnderscorecurrencyUnderscorecode :: Maybe Text -- ^ The ISO-4217 currency code of the balance.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetAssetReportResponseReportAccountsInnerBalances where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getAssetReportResponseReportAccountsInnerBalances")
instance ToJSON GetAssetReportResponseReportAccountsInnerBalances where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getAssetReportResponseReportAccountsInnerBalances")


-- | 
data GetAssetReportResponseReportAccountsInnerHistoricalBalancesInner = GetAssetReportResponseReportAccountsInnerHistoricalBalancesInner
  { getAssetReportResponseReportAccountsInnerHistoricalBalancesInnerDate :: Maybe Text -- ^ The date of the calculated historical balance, in an ISO 8601 format (YYYY-MM-DD)
  , getAssetReportResponseReportAccountsInnerHistoricalBalancesInnerCurrent :: Maybe Double -- ^ The total amount of funds in the account, calculated from the current balance in the balance object by subtracting inflows and adding back outflows according to the posted date of each transaction.
  , getAssetReportResponseReportAccountsInnerHistoricalBalancesInnerIsoUnderscorecurrencyUnderscorecode :: Maybe Text -- ^ The ISO-4217 currency code of the balance.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetAssetReportResponseReportAccountsInnerHistoricalBalancesInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getAssetReportResponseReportAccountsInnerHistoricalBalancesInner")
instance ToJSON GetAssetReportResponseReportAccountsInnerHistoricalBalancesInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getAssetReportResponseReportAccountsInnerHistoricalBalancesInner")


-- | 
data GetFinancialConnectionsAccountBalanceResponse = GetFinancialConnectionsAccountBalanceResponse
  { getFinancialConnectionsAccountBalanceResponseBalances :: Maybe [FinancialConnectionsAccountBalance] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsAccountBalanceResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsAccountBalanceResponse")
instance ToJSON GetFinancialConnectionsAccountBalanceResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsAccountBalanceResponse")


-- | 
data GetFinancialConnectionsAccountDetailsRequest = GetFinancialConnectionsAccountDetailsRequest
  { getFinancialConnectionsAccountDetailsRequestAccessUnderscoretoken :: Text -- ^ Access token for authentication
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsAccountDetailsRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsAccountDetailsRequest")
instance ToJSON GetFinancialConnectionsAccountDetailsRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsAccountDetailsRequest")


-- | 
data GetFinancialConnectionsAccountDetailsResponse = GetFinancialConnectionsAccountDetailsResponse
  { getFinancialConnectionsAccountDetailsResponseAccountUnderscoredetails :: Maybe [FinancialConnectionsAccountDetails] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsAccountDetailsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsAccountDetailsResponse")
instance ToJSON GetFinancialConnectionsAccountDetailsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsAccountDetailsResponse")


-- | 
data GetFinancialConnectionsAccountsRequest = GetFinancialConnectionsAccountsRequest
  { getFinancialConnectionsAccountsRequestAccessUnderscoretoken :: Text -- ^ Access token for authentication
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsAccountsRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsAccountsRequest")
instance ToJSON GetFinancialConnectionsAccountsRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsAccountsRequest")


-- | 
data GetFinancialConnectionsAccountsResponse = GetFinancialConnectionsAccountsResponse
  { getFinancialConnectionsAccountsResponseAccounts :: Maybe [FinancialConnectionsAccount] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsAccountsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsAccountsResponse")
instance ToJSON GetFinancialConnectionsAccountsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsAccountsResponse")


-- | 
data GetFinancialConnectionsBalanceRequest = GetFinancialConnectionsBalanceRequest
  { getFinancialConnectionsBalanceRequestAccessUnderscoretoken :: Text -- ^ Access token for authentication
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsBalanceRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsBalanceRequest")
instance ToJSON GetFinancialConnectionsBalanceRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsBalanceRequest")


-- | 
data GetFinancialConnectionsOwnersRequest = GetFinancialConnectionsOwnersRequest
  { getFinancialConnectionsOwnersRequestAccessUnderscoretoken :: Text -- ^ Access token for authentication
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsOwnersRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsOwnersRequest")
instance ToJSON GetFinancialConnectionsOwnersRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsOwnersRequest")


-- | 
data GetFinancialConnectionsOwnersResponse = GetFinancialConnectionsOwnersResponse
  { getFinancialConnectionsOwnersResponseAccounts :: Maybe [GetFinancialConnectionsOwnersResponseAccountsInner] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsOwnersResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsOwnersResponse")
instance ToJSON GetFinancialConnectionsOwnersResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsOwnersResponse")


-- | 
data GetFinancialConnectionsOwnersResponseAccountsInner = GetFinancialConnectionsOwnersResponseAccountsInner
  { getFinancialConnectionsOwnersResponseAccountsInnerRemoteUnderscoreaccountUnderscoreid :: Maybe Text -- ^ The remote account id of the account
  , getFinancialConnectionsOwnersResponseAccountsInnerOwners :: Maybe [FinancialConnectionsOwner] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetFinancialConnectionsOwnersResponseAccountsInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getFinancialConnectionsOwnersResponseAccountsInner")
instance ToJSON GetFinancialConnectionsOwnersResponseAccountsInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getFinancialConnectionsOwnersResponseAccountsInner")


-- | 
data GetInvestmentHoldingsRequest = GetInvestmentHoldingsRequest
  { getInvestmentHoldingsRequestAccessUnderscoretoken :: Text -- ^ Access token for authentication
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetInvestmentHoldingsRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getInvestmentHoldingsRequest")
instance ToJSON GetInvestmentHoldingsRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getInvestmentHoldingsRequest")


-- | 
data GetInvestmentHoldingsResponse = GetInvestmentHoldingsResponse
  { getInvestmentHoldingsResponseAccounts :: Maybe [FinancialConnectionsInvestmentAccount] -- ^ 
  , getInvestmentHoldingsResponseHoldings :: Maybe [FinancialConnectionsHolding] -- ^ 
  , getInvestmentHoldingsResponseSecurities :: Maybe [FinancialConnectionsInvestmentSecurity] -- ^ 
  , getInvestmentHoldingsResponseRemoteUnderscoredata :: Maybe Value -- ^ The exact data from the aggregator (ie plaid) that we retrieved the information from.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetInvestmentHoldingsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getInvestmentHoldingsResponse")
instance ToJSON GetInvestmentHoldingsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getInvestmentHoldingsResponse")


-- | 
data GetInvestmentTransactionsRequest = GetInvestmentTransactionsRequest
  { getInvestmentTransactionsRequestAccessUnderscoretoken :: Text -- ^ Access token for authentication
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetInvestmentTransactionsRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getInvestmentTransactionsRequest")
instance ToJSON GetInvestmentTransactionsRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getInvestmentTransactionsRequest")


-- | 
data GetInvestmentTransactionsResponse = GetInvestmentTransactionsResponse
  { getInvestmentTransactionsResponseAccounts :: Maybe [FinancialConnectionsInvestmentAccount] -- ^ 
  , getInvestmentTransactionsResponseInvestmentUnderscoretransactions :: Maybe [FinancialConnectionsInvestmentTransaction] -- ^ 
  , getInvestmentTransactionsResponseSecurities :: Maybe [FinancialConnectionsInvestmentSecurity] -- ^ 
  , getInvestmentTransactionsResponseRemoteUnderscoredata :: Maybe Value -- ^ The exact data from the aggregator (ie plaid) that we retrieved the information from.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetInvestmentTransactionsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getInvestmentTransactionsResponse")
instance ToJSON GetInvestmentTransactionsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getInvestmentTransactionsResponse")


-- | 
data GetTransactionsRequest = GetTransactionsRequest
  { getTransactionsRequestAccessUnderscoretoken :: Text -- ^ Access token for authentication
  , getTransactionsRequestCursor :: Maybe Text -- ^ Cursor for pagination
  , getTransactionsRequestCount :: Maybe Int -- ^ Number of items per page
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetTransactionsRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getTransactionsRequest")
instance ToJSON GetTransactionsRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getTransactionsRequest")


-- | 
data GetTransactionsResponse = GetTransactionsResponse
  { getTransactionsResponseData :: Maybe [GetTransactionsResponseDataInner] -- ^ 
  , getTransactionsResponseCursor :: Maybe Text -- ^ The cursor of the last item returned
  , getTransactionsResponseHasUnderscorenext :: Maybe Bool -- ^ Indicates if there are more pages to navigate through
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetTransactionsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getTransactionsResponse")
instance ToJSON GetTransactionsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getTransactionsResponse")


-- | 
data GetTransactionsResponseDataInner = GetTransactionsResponseDataInner
  { getTransactionsResponseDataInnerId :: Maybe Text -- ^ Fuse Id of the transaction
  , getTransactionsResponseDataInnerRemoteUnderscoreid :: Maybe Text -- ^ Remote Id of the transaction, ie Plaid or Teller Id
  , getTransactionsResponseDataInnerRemoteUnderscoreaccountUnderscoreid :: Maybe Text -- ^ Remote Account Id of the transaction, ie Plaid Account Id
  , getTransactionsResponseDataInnerAmount :: Maybe Double -- ^ Amount in cents associated with the transaction
  , getTransactionsResponseDataInnerDate :: Maybe Text -- ^ Date of the transaction
  , getTransactionsResponseDataInnerDescription :: Maybe Text -- ^ Description of the transaction
  , getTransactionsResponseDataInnerCategory :: Maybe [Text] -- ^ Categories of the transaction, ie Computers and Electronics
  , getTransactionsResponseDataInnerMerchant :: Maybe GetTransactionsResponseDataInnerMerchant -- ^ 
  , getTransactionsResponseDataInnerStatus :: Maybe Text -- ^ The status of the transaction. This will be either POSTED or PENDING.
  , getTransactionsResponseDataInnerType :: Maybe Text -- ^ Type of the transaction, ie adjustment
  , getTransactionsResponseDataInnerRemoteUnderscoredata :: Maybe Value -- ^ The exact data from the aggregator (ie plaid) that we retrieved the information from
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetTransactionsResponseDataInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getTransactionsResponseDataInner")
instance ToJSON GetTransactionsResponseDataInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getTransactionsResponseDataInner")


-- | 
data GetTransactionsResponseDataInnerMerchant = GetTransactionsResponseDataInnerMerchant
  { getTransactionsResponseDataInnerMerchantName :: Maybe Text -- ^ Merchant name
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GetTransactionsResponseDataInnerMerchant where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "getTransactionsResponseDataInnerMerchant")
instance ToJSON GetTransactionsResponseDataInnerMerchant where
  toJSON = genericToJSON (removeFieldLabelPrefix False "getTransactionsResponseDataInnerMerchant")


-- | 
data SyncFinancialConnectionsDataResponse = SyncFinancialConnectionsDataResponse
  { syncFinancialConnectionsDataResponseMessage :: Maybe Text -- ^ Response message
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SyncFinancialConnectionsDataResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "syncFinancialConnectionsDataResponse")
instance ToJSON SyncFinancialConnectionsDataResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "syncFinancialConnectionsDataResponse")


-- | 
data SyncTransactionsRequest = SyncTransactionsRequest
  { syncTransactionsRequestAccessUnderscoretoken :: Text -- ^ The access token of the financial institution connection
  , syncTransactionsRequestCursor :: Maybe Text -- ^ The cursor value represents the last update requested. Providing it will cause the response to only return changes after this update. If omitted, the entire history of updates will be returned, starting with the first-added transactions on the item.
  , syncTransactionsRequestCount :: Maybe Int -- ^ The number of transaction updates to fetch.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SyncTransactionsRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "syncTransactionsRequest")
instance ToJSON SyncTransactionsRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "syncTransactionsRequest")


-- | 
data SyncTransactionsResponse = SyncTransactionsResponse
  { syncTransactionsResponseAdded :: Maybe [TransactionCommonModel] -- ^ Transactions that have been added to the item since `cursor` ordered by ascending last modified time.
  , syncTransactionsResponseModified :: Maybe [TransactionCommonModel] -- ^ Transactions that have been modified on the item since `cursor` ordered by ascending last modified time.
  , syncTransactionsResponseRemoved :: Maybe [SyncTransactionsResponseRemovedInner] -- ^ Transactions that have been removed from the item since `cursor` ordered by ascending last modified time.
  , syncTransactionsResponseNextUnderscorecursor :: Maybe Text -- ^ Cursor used for fetching any future updates after the latest update provided in this response. The cursor obtained after all pages have been pulled (indicated by `has_next` being `false`) will be valid for at least 1 year. This cursor should be persisted for later calls.
  , syncTransactionsResponseHasUnderscorenext :: Maybe Bool -- ^ Represents if more than requested count of transaction updates exist. If true, the additional updates can be fetched by making an additional request with `cursor` set to `next_cursor`. If `has_next` is true, it's important to pull all available pages, to make it less likely for underlying data changes to conflict with pagination.
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SyncTransactionsResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "syncTransactionsResponse")
instance ToJSON SyncTransactionsResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "syncTransactionsResponse")


-- | 
data SyncTransactionsResponseRemovedInner = SyncTransactionsResponseRemovedInner
  { syncTransactionsResponseRemovedInnerTransactionUnderscoreid :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SyncTransactionsResponseRemovedInner where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "syncTransactionsResponseRemovedInner")
instance ToJSON SyncTransactionsResponseRemovedInner where
  toJSON = genericToJSON (removeFieldLabelPrefix False "syncTransactionsResponseRemovedInner")


-- | 
data TransactionCommonModel = TransactionCommonModel
  { transactionCommonModelData :: Maybe [GetTransactionsResponseDataInner] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransactionCommonModel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "transactionCommonModel")
instance ToJSON TransactionCommonModel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "transactionCommonModel")


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("$", "'Dollar")
      , ("^", "'Caret")
      , ("|", "'Pipe")
      , ("=", "'Equal")
      , ("*", "'Star")
      , ("-", "'Dash")
      , ("&", "'Ampersand")
      , ("%", "'Percent")
      , ("#", "'Hash")
      , ("@", "'At")
      , ("!", "'Exclamation")
      , ("+", "'Plus")
      , (":", "'Colon")
      , (";", "'Semicolon")
      , (">", "'GreaterThan")
      , ("<", "'LessThan")
      , (".", "'Period")
      , ("_", "'Underscore")
      , ("?", "'Question_Mark")
      , (",", "'Comma")
      , ("'", "'Quote")
      , ("/", "'Slash")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("{", "'Left_Curly_Bracket")
      , ("}", "'Right_Curly_Bracket")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("~", "'Tilde")
      , ("`", "'Backtick")
      , ("<=", "'Less_Than_Or_Equal_To")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("!=", "'Not_Equal")
      , ("<>", "'Not_Equal")
      , ("~=", "'Tilde_Equal")
      , ("\\", "'Back_Slash")
      , ("\"", "'Double_Quote")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
