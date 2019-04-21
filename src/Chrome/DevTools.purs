-- | This module defines data types and functions for connecting to Chrome
-- | DevTools.
module Chrome.DevTools
  ( module Chrome.DevTools.HTTP.Exports
  , Command(..)
  , CommandError(..)
  , DevTools
  , Handler
  , execute
  , execute_
  , mkDevTools
  , onEvent
  , shutdown
  ) where

import Prelude

import Chrome.DevTools.HTTP (URL)
import Chrome.DevTools.HTTP (Domain(..), DomainCommand(..), DomainEvent(..), DomainItems(..), DomainParameter(..), DomainProperty(..), DomainType(..), HTTPError(..), Options, Protocol(..), ProtocolVersion(..), Target(..), URL, Version(..), activate, close, defaultOptions, list, new, protocol, version) as Chrome.DevTools.HTTP.Exports
import Chrome.DevTools.WebSocket (WebSocket)
import Chrome.DevTools.WebSocket (WebSocketEvent(..), close, mkWebSocket, recv, send) as WebSocket
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, jsonParser, stringify, (.??), (:=), (~>))
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap (delete, empty, insert, lookup, values) as HashMap
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, forkAff, joinFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, put, take) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref (modify, modify_, new, read) as Ref

-- | A command to be executed in a DevTools domain. Note that `method` is the
-- | domain-qualified method name, e.g. `Console.enable`.
newtype Command = Command
  { method :: String
  , params :: Maybe Json
  }

-- | The type of command errors.
data CommandError = ErrorResponse Json
                  | ConnectionClosed Int ( Maybe String ) Boolean
                  | ConnectionError Error

instance showCommandError :: Show CommandError where
  show ( ErrorResponse err ) =
    "ErrorResponse " <> stringify err
  show ( ConnectionClosed code reason wasClean ) =
    "ConnectionClosed " <> show code <> " " <> show reason <> " " <> show wasClean
  show ( ConnectionError err ) =
    "ConnectionError " <> show err

-- | The type of an event handler.
type Handler = Json -> Effect Unit

-- | An established DevTools connection.
newtype DevTools = DevTools
  { ws :: WebSocket
  , seq :: Ref Int
  , commands :: Ref CommandMap
  , handlers :: Ref HandlerMap
  , service :: Ref ( Fiber Unit )
  }

type CommandMap = HashMap Int ( AVar ( Either CommandError Json ) )

type HandlerMap = HashMap String Handler

newtype Request = Request
  { id :: Int
  , method :: String
  , params :: Json
  }

instance encodeRequest :: EncodeJson Request where
  encodeJson ( Request r ) =
    ( "id" := r.id
    ~> "method" := r.method
    ~> "params" := r.params
    ~> jsonEmptyObject
    )

newtype Response = Response
  {
    -- Command results
    id :: Maybe Int
  , result :: Maybe Json
  , error :: Maybe Json

    -- Event notifications
  , method :: Maybe String
  , params :: Maybe Json
  }

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .?? "id"
    result <- obj .?? "result"
    error <- obj .?? "error"
    method <- obj .?? "method"
    params <- obj .?? "params"
    pure $ Response
      { id
      , result
      , error
      , method
      , params
      }

-- | Establish a new DevTools connection. Note that The URL must use either the
-- | `ws` or `wss` scheme.
mkDevTools :: URL -> Aff ( Either Error DevTools )
mkDevTools url = runExceptT $ do
  -- Establish a new WebSocket connection to DevTools
  ws <- ExceptT ( WebSocket.mkWebSocket url )

  -- Create a few refs to hold mutable state
  seq <- liftEffect ( Ref.new 0 )
  commands <- liftEffect ( Ref.new HashMap.empty )
  handlers <- liftEffect ( Ref.new HashMap.empty )

  -- Fork an async computation to process DevTools messages
  fiber <- liftAff ( forkAff ( run ws commands handlers ) )
  service <- liftEffect ( Ref.new fiber )

  pure $ DevTools
    { ws
    , seq
    , commands
    , handlers
    , service
    }

-- | Execute a command and wait for the response.
execute :: DevTools -> Command -> Aff ( Either CommandError Json )
execute ( DevTools dt ) cmd = do
  -- Encode the command message
  seq <- liftEffect ( nextSeq dt.seq )
  let msg = stringify ( encodeJson ( request cmd seq ) )

  -- Send the command message and wait for the response on an associated AVar
  avar <- AVar.empty
  liftEffect $ do
    Ref.modify_ ( HashMap.insert seq avar ) dt.commands
    WebSocket.send dt.ws msg
  resp <- AVar.take avar
  liftEffect ( Ref.modify_ ( HashMap.delete seq ) dt.commands )
  pure resp

-- | Execute a command but discard (do not wait for) the response.
execute_ :: DevTools -> Command -> Effect Unit
execute_ ( DevTools dt ) cmd = do
  -- Encode and send the command message
  seq <- nextSeq dt.seq
  let msg = stringify ( encodeJson ( request cmd seq ) )
  WebSocket.send dt.ws msg

-- | Register an event handler. Note that `event` is the domain-equalified event
-- | name, e.g. `Console.messageAdded`.
onEvent :: DevTools -> String -> Handler -> Effect Unit
onEvent ( DevTools dt ) event handler =
  Ref.modify_ ( HashMap.insert event handler ) dt.handlers

-- | Shutdown a DevTools connection.
shutdown :: DevTools -> Aff Unit
shutdown ( DevTools { ws, service } ) = do
  liftEffect ( WebSocket.close ws )
  liftEffect ( Ref.read service ) >>= joinFiber

run :: WebSocket -> Ref CommandMap -> Ref HandlerMap -> Aff Unit
run ws commands handlers = do
  -- Receive a message from DevTools
  ev <- WebSocket.recv ws
  case ev of
    WebSocket.Message msg -> do
      case decode msg of
        Right ( Response { id: Just id, error: Just error } ) ->
          complete id ( Left ( ErrorResponse error ) )

        Right ( Response { id: Just id, result } ) ->
          -- NB: interpret the lack of `error` and `result` as command success
          complete id ( Right ( maybe jsonEmptyObject identity result ) )

        Right ( Response { method: Just event, params } ) ->
          dispatch event ( maybe jsonEmptyObject identity params )

        Right _ ->
          -- TODO: log?
          pure unit

        Left err ->
          -- TODO: log?
          pure unit

      -- Recurse
      run ws commands handlers

    WebSocket.ConnectionClosed code reason wasClean ->
      -- WebSocket connection is closed: abort waiting commands
      abort ( ConnectionClosed code reason wasClean )

    WebSocket.ConnectionError err ->
      -- WebSocket connection is errored: abort waiting commands
      abort ( ConnectionError err )

  where
    decode :: String -> Either String Response
    decode msg = jsonParser msg >>= decodeJson

    complete :: Int -> Either CommandError Json -> Aff Unit
    complete id val = do
      x <- liftEffect ( HashMap.lookup id <$> Ref.read commands )
      case x of
        Just avar -> AVar.put val avar
        Nothing -> pure unit

    abort :: CommandError -> Aff Unit
    abort err = do
      avars <- liftEffect ( HashMap.values <$> Ref.read commands )
      traverse_ ( AVar.put ( Left err ) ) avars

    dispatch :: String -> Json -> Aff Unit
    dispatch event params = do
      x <- liftEffect ( HashMap.lookup event <$> Ref.read handlers )
      case x of
        Just handler -> liftEffect ( handler params )
        Nothing -> pure unit

request :: Command -> Int -> Request
request ( Command { method, params } ) seq =
  Request
    { id: seq
    , method
    , params: maybe jsonEmptyObject identity params
    }

nextSeq :: Ref Int -> Effect Int
nextSeq = Ref.modify ( _ + 1 )
