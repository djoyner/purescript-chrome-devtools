-- | This module defines data types and functions for invoking the browser's
-- | HTTP DevTools endpoints.
module Chrome.DevTools.HTTP (
    module Affjax.Exports
  , Domain(..)
  , DomainCommand(..)
  , DomainEvent(..)
  , DomainItems(..)
  , DomainParameter(..)
  , DomainProperty(..)
  , DomainType(..)
  , HTTPError(..)
  , Options
  , Protocol(..)
  , ProtocolVersion(..)
  , Target(..)
  , Version(..)
  , activate
  , close
  , defaultOptions
  , list
  , new
  , protocol
  , version
  ) where

import Prelude

import Affjax (Response, URL, get)
import Affjax (URL) as Affjax.Exports
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Control.Plus (empty)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?), (.??), (.?=))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, catchError)
import Effect.Exception (Error)
import Foreign (Foreign, ForeignError)

-- | Browser connection options.
type Options =
  { host :: String
  , port :: Int
  , secure :: Boolean
  }

defaultOptions :: Options
defaultOptions =
  { host: "localhost"
  , port: 9222
  , secure: false
  }

-- | A target (page, iframe, etc.) running in the browser.
newtype Target = Target
  { id :: String
  , parentId :: Maybe String
  , "type" :: String
  , title :: String
  , url :: String
  , faviconUrl :: Maybe String
  , devtoolsFrontendUrl :: String
  , webSocketDebuggerUrl :: String
  }

instance decodeTarget :: DecodeJson Target where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    parentId <- obj .?? "parentId"
    _type <- obj .? "type"
    title <- obj .? "title"
    url <- obj .? "url"
    faviconUrl <- obj .?? "faviconUrl"
    devtoolsFrontendUrl <- obj .? "devtoolsFrontendUrl"
    webSocketDebuggerUrl <- obj .? "webSocketDebuggerUrl"
    pure $ Target
      { id
      , parentId
      , "type": _type
      , title
      , url
      , faviconUrl
      , devtoolsFrontendUrl
      , webSocketDebuggerUrl
      }

derive newtype instance showTarget :: Show Target

-- | Browser version metadata.
newtype Version = Version
  { browser :: String
  , protocolVersion :: String
  , userAgent :: String
  , v8Version :: String
  , webKitVersion :: String
  , webSocketDebuggerUrl :: String
  }

instance decodeVersion :: DecodeJson Version where
  decodeJson json = do
    obj <- decodeJson json
    browser <- obj .? "Browser"
    protocolVersion <- obj .? "Protocol-Version"
    userAgent <- obj .? "User-Agent"
    v8Version <- obj .? "V8-Version"
    webKitVersion <- obj .? "WebKit-Version"
    webSocketDebuggerUrl <- obj .? "webSocketDebuggerUrl"
    pure $ Version
      { browser
      , protocolVersion
      , userAgent
      , v8Version
      , webKitVersion
      , webSocketDebuggerUrl
      }

derive newtype instance showVersion :: Show Version

-- | DevTools protocol description.
newtype Protocol = Protocol
  { version :: ProtocolVersion
  , domains :: Array Domain
  }

instance decodeProtocol :: DecodeJson Protocol where
  decodeJson json = do
    obj <- decodeJson json
    _version <- obj .? "version"
    domains <- obj .? "domains"
    pure $ Protocol
      { version: _version
      , domains
      }

derive newtype instance showProtocol :: Show Protocol

newtype ProtocolVersion = ProtocolVersion
  { major :: String
  , minor :: String
  }

instance decodeProtocolVersion :: DecodeJson ProtocolVersion where
  decodeJson json = do
    obj <- decodeJson json
    major <- obj .? "major"
    minor <- obj .? "minor"
    pure $ ProtocolVersion
      { major
      , minor
      }

derive newtype instance showProtocolVersion :: Show ProtocolVersion

newtype Domain = Domain
  { domain :: String
  , description :: String
  , deprecated :: Boolean
  , dependencies :: Array String
  , types :: Array DomainType
  , commands :: Array DomainCommand
  , events :: Array DomainEvent
  }

instance decodeDomain :: DecodeJson Domain where
  decodeJson json = do
    obj <- decodeJson json
    domain <- obj .? "domain"
    description <- obj .?? "description" .?= ""
    deprecated <- obj .?? "deprecated" .?= false
    dependencies <- obj .?? "dependencies" .?= empty
    types <- obj .?? "types" .?= empty
    commands <- obj .?? "command" .?= empty
    events <- obj .?? "events" .?= empty
    pure $ Domain
      { domain
      , description
      , deprecated
      , dependencies
      , types
      , commands
      , events
      }

derive newtype instance showDomain :: Show Domain

newtype DomainType = DomainType
  { id :: String
  , description :: String
  , "type" :: String
  , properties :: Array DomainProperty
  }

instance decodeDomainType :: DecodeJson DomainType where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    description <- obj .?? "description" .?= ""
    _type <- obj .? "type"
    properties <- obj .?? "properties" .?= empty
    pure $ DomainType
      { id
      , description
      , "type": _type
      , properties
      }

derive newtype instance showDomainType :: Show DomainType

newtype DomainCommand = DomainCommand
  { name :: String
  , description :: String
  , parameters :: Array DomainParameter
  }

instance decodeDomainCommand :: DecodeJson DomainCommand where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    description <- obj .?? "description" .?= ""
    parameters <- obj .?? "parameters" .?= empty
    pure $ DomainCommand
      { name
      , description
      , parameters
      }

derive newtype instance showDomainCommand :: Show DomainCommand

newtype DomainEvent = DomainEvent
  { name :: String
  , description :: String
  , parameters :: Array DomainParameter
  }

instance decodeDomainEvent :: DecodeJson DomainEvent where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    description <- obj .?? "description" .?= ""
    parameters <- obj .?? "parameters" .?= empty
    pure $ DomainEvent
      { name
      , description
      , parameters
      }

derive newtype instance showDomainEvent :: Show DomainEvent

newtype DomainProperty = DomainProperty
  { name :: String
  , description :: String
  , experimental :: Boolean
  , optional :: Boolean
  , "type" :: Maybe String
  , enum :: Maybe ( Array String )
  , items :: Maybe DomainItems
  , "$ref" :: Maybe String
  }

instance decodeDomainProperty :: DecodeJson DomainProperty where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    description <- obj .?? "description" .?= ""
    experimental <- obj .?? "experimental" .?= false
    optional <- obj .?? "optional" .?= false
    _type <- obj .?? "type"
    enum <- obj .?? "enum"
    items <- obj .?? "items"
    ref <- obj .?? "$ref"
    pure $ DomainProperty
      { name
      , description
      , experimental
      , optional
      , "type": _type
      , enum
      , items
      , "$ref": ref
      }

derive newtype instance showDomainProperty :: Show DomainProperty

newtype DomainParameter = DomainParameter
  { name :: String
  , description :: String
  , experimental :: Boolean
  , optional :: Boolean
  , "type" :: Maybe String
  , enum :: Maybe ( Array String )
  , items :: Maybe DomainItems
  , "$ref" :: Maybe String
  }

instance decodeDomainParameter :: DecodeJson DomainParameter where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    description <- obj .?? "description" .?= ""
    experimental <- obj .?? "experimental" .?= false
    optional <- obj .?? "optional" .?= false
    _type <- obj .?? "type"
    enum <- obj .?? "enum"
    items <- obj .?? "items"
    ref <- obj .?? "$ref"
    pure $ DomainParameter
      { name
      , description
      , experimental
      , optional
      , "type": _type
      , enum
      , items
      , "$ref": ref
      }

derive newtype instance showDomainParameter :: Show DomainParameter

newtype DomainItems = DomainItems
  { "type" :: Maybe String
  , "$ref" :: Maybe String
  }

instance decodeDomainItems :: DecodeJson DomainItems where
  decodeJson json = do
    obj <- decodeJson json
    _type <- obj .?? "type"
    ref <- obj .?? "$ref"
    pure $ DomainItems
      { "type": _type
      , "$ref": ref
      }

derive newtype instance showDomainItems :: Show DomainItems

-- | The type of HTTP errors.
data HTTPError = CaughtException Error
               | ResponseStatusError StatusCode String
               | ResponseFormatError ForeignError Foreign
               | ResponseDecodeError String

instance showHTTPError :: Show HTTPError where
  show ( CaughtException err ) = show err
  show ( ResponseStatusError status statusText ) =
    "ResponseStatusError " <> show status <> " " <> statusText
  show ( ResponseFormatError err _ ) =
    "ResponseFormatError " <> show err
  show ( ResponseDecodeError err ) =
    "ResponseDecodeError " <> err

-- | Get a list of all available targets.
list :: Options -> Aff ( Either HTTPError ( Array Target ) )
list opts = do
  let url = ( baseUrl opts ) <> "/json/list"
  resp <- tryRequest ( get RF.json url )
  pure ( resp >>= decodeResponse )

-- | Open a new tab. Returns new target information.
new :: Options -> Maybe URL -> Aff ( Either HTTPError Target )
new opts nav = do
  let url = ( baseUrl opts ) <> "/json/new" <> maybe "" ("?" <> _) nav
  resp <- handleResponse <$> get RF.json url
  pure ( resp >>= decodeResponse )

-- | Brings a tab into the foreground.
activate :: Options -> Target -> Aff ( Maybe HTTPError )
activate opts ( Target { id } ) = do
  let url = ( baseUrl opts ) <> "/json/activate/" <> id
  resp <- handleResponse <$> get RF.ignore url
  pure ( either Just ( const Nothing ) resp )

-- | Closes a tab.
close :: Options -> Target -> Aff ( Maybe HTTPError )
close opts ( Target { id } ) = do
  let url = ( baseUrl opts ) <> "/json/close/" <> id
  resp <- handleResponse <$> get RF.ignore url
  pure ( either Just ( const Nothing ) resp )

-- | Get browser version metadata.
version :: Options -> Aff ( Either HTTPError Version )
version opts = do
  let url = ( baseUrl opts ) <> "/json/version"
  resp <- handleResponse <$> get RF.json url
  pure ( resp >>= decodeResponse )

-- | Get the current DevTools protocol.
protocol :: Options -> Aff ( Either HTTPError Protocol )
protocol opts = do
  let url = ( baseUrl opts ) <> "/json/protocol"
  resp <- handleResponse <$> get RF.json url
  pure ( resp >>= decodeResponse )

baseUrl :: Options -> URL
baseUrl opts =
  method <> "://" <> opts.host <> ":" <> show opts.port
  where
    method = if opts.secure then "https" else "http"

tryRequest
  :: forall a
   . Aff ( Response ( Either RF.ResponseFormatError a ) )
  -> Aff ( Either HTTPError a )
tryRequest req =
  ( handleResponse <$> req )
  `catchError`
  \err -> pure ( Left ( CaughtException err ) )

handleResponse
  :: forall a
   . Response ( Either RF.ResponseFormatError a )
  -> Either HTTPError a
handleResponse
  { status, body: Right body}
  | isSuccessCode status = Right body
handleResponse
  { status, body: Left ( RF.ResponseFormatError err f )}
  | isSuccessCode status = Left ( ResponseFormatError err f )
handleResponse
  { status, statusText }
  | otherwise = Left ( ResponseStatusError status statusText )

decodeResponse
  :: forall a
   . DecodeJson a => Json
  -> Either HTTPError a
decodeResponse json = case decodeJson json of
  Left err -> Left ( ResponseDecodeError err )
  Right x  -> Right x

isSuccessCode :: StatusCode -> Boolean
isSuccessCode (StatusCode status) = status / 100 == 2
