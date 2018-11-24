-- | This module defines data types and functions for connecting to WebSocket
-- | endpoints. It hides details of message queuing and exposes simple `send`
-- | and `recv` functions.
module Chrome.DevTools.WebSocket
  ( URL
  , WebSocket
  , WebSocketEvent(..)
  , close
  , mkWebSocket
  , recv
  , send
  ) where

import Prelude

import Control.Parallel (parOneOf)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Queue.One (Queue)
import Queue.One (draw, new, put) as Queue
import Queue.Types (READ, WRITE)
import WebSocket (Capabilities, newWebSocket)

-- | Type alias for URL strings.
type URL = String

-- | Events emitted by established WebSocket connections.
data WebSocketEvent = Message String
                    | ConnectionClosed Int ( Maybe String ) Boolean
                    | ConnectionError Error

instance showWebSocketEvent :: Show WebSocketEvent where
  show ( Message msg ) =
    "Message " <> msg
  show ( ConnectionClosed code reason wasClean ) =
    "ConnectionClosed " <> show code <> " " <> show reason <> " " <> show wasClean
  show ( ConnectionError err ) =
    "ConnectionError " <> show err

-- | A WebSocket connection.
newtype WebSocket = WebSocket
  { caps :: Capabilities Effect
  , events :: Queue (read :: READ, write :: WRITE) WebSocketEvent
  , closed :: Ref Boolean
  }

-- | Establish a new WebSocket connection.
mkWebSocket :: URL -> Aff (Either Error WebSocket)
mkWebSocket url = do
  -- Create a couple of queues:
  --
  --   "opened" to receive capabilities on open (used here and discarded)
  --   "events" to receive events (used throughout the life of the connection)
  --
  opened <- liftEffect Queue.new
  events <- liftEffect Queue.new

  -- Create a ref to hold WebSocket state
  closed <- liftEffect ( Ref.new false )

  -- Create the WebSocket
  let params =
        { url
        , protocols: empty
        , continue: \_ ->
          { onclose: onClose events closed
          , onerror: onError events closed
          , onmessage: onMessage events
          , onopen: Queue.put opened
          }
        }
  liftEffect ( newWebSocket params )

  -- Wait for the WS connection to open, or for an event (indicating an error)
  one <- parOneOf
    [ Right <$> Queue.draw opened
    , Left <$> Queue.draw events
    ]

  case one of
    Right caps -> do
      pure ( Right ( WebSocket { caps, events, closed } ) )
    Left ( Message msg ) ->
      pure ( Left ( error ( "unexpected message during connection: " <> msg ) ) )
    Left ( ConnectionClosed code reason _ ) ->
      pure ( Left ( error ( "connection closed: " <> show code <> " " <> show reason ) ) )
    Left ( ConnectionError err ) ->
      pure ( Left err )

-- | Receive an event from a WebSocket connection. Note that once a connection
-- | is closed, then no further events can be received.
recv :: WebSocket -> Aff WebSocketEvent
recv ( WebSocket { events } ) = Queue.draw events

-- | Send a message on a WebSocket connection. Note that once a connection is
-- | closed, then no further messages can be sent.
send :: WebSocket -> String -> Effect Unit
send ( WebSocket { caps, closed } ) msg = do
  val <- Ref.read closed
  unless val ( caps.send msg )

-- | Close a WebSocket connection.
close :: WebSocket -> Effect Unit
close ( WebSocket { caps } ) = caps.close

onClose :: forall r
         . Queue (write :: WRITE | r) WebSocketEvent
        -> Ref Boolean
        -> { code :: Int, reason :: Maybe String, wasClean :: Boolean }
        -> Effect Unit
onClose events closed { code, reason, wasClean } = do
  Ref.write true closed
  Queue.put events ( ConnectionClosed code reason wasClean )

onError :: forall r
         . Queue (write :: WRITE | r) WebSocketEvent
        -> Ref Boolean
        -> Error
        -> Effect Unit
onError events closed err = do
  Ref.write true closed
  Queue.put events ( ConnectionError err )

onMessage :: forall r
           . Queue (write :: WRITE | r) WebSocketEvent
          -> Capabilities Effect
          -> String
          -> Effect Unit
onMessage  events _ msg = do
  Queue.put events ( Message msg )
