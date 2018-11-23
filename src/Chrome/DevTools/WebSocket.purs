module Chrome.DevTools.WebSocket
  ( Event(..)
  , WebSocket
  , close
  , mkWebSocket
  , recv
  , send
  ) where

import Prelude

import Chrome.DevTools.HTTP (Options, Target(..))
import Control.Parallel (parOneOf)
import Control.Plus (empty)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags as RF
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Queue.One (Queue)
import Queue.One as Q
import Queue.Types (READ, WRITE)
import WebSocket as WS

-- | Events emitted by established WebSocket connections
data Event = Message String
           | Closed Int ( Maybe String ) Boolean
           | Error String

instance showEvent :: Show Event where
  show ( Message msg ) = "Message " <> msg
  show ( Closed code reason wasClean ) =
    "Closed " <> show code <> " " <> show reason <> " " <> show wasClean
  show ( Error err ) = "Error " <> err

-- | An established WebSocket connection
newtype WebSocket = WebSocket
  { caps :: WS.Capabilities Effect
  , events :: Queue (read :: READ, write :: WRITE) Event
  }

mkWebSocket :: Options -> Target -> Aff (Either String WebSocket)
mkWebSocket opts targ = do
  -- Create a couple of queues:
  --
  --   "opened" to receive capabilities on open (used here and discarded)
  --   "events" to receive events (used throughout the life of the connection)
  --
  opened <- liftEffect Q.new
  events <- liftEffect Q.new

  -- Create the WebSocket
  let params =
        { url: wsUrl opts targ
        , protocols: empty
        , continue: \_ ->
          { onclose: \{ code, reason, wasClean } ->
             Q.put events ( Closed code reason wasClean )
          , onerror: \err -> Q.put events ( Error err )
          , onmessage: \_ msg -> Q.put events ( Message msg )
          , onopen: \caps -> Q.put opened caps
          }
        }
  liftEffect ( WS.newWebSocket params )

  -- Wait for the WS connection to open, or for an event (indicating an error)
  one <- parOneOf
    [ Right <$> Q.draw opened
    , Left <$> Q.draw events
    ]

  pure $ case one of
    Right caps -> Right ( WebSocket { caps, events } )
    Left ( Message msg ) ->
      Left ( "unexpected message during connection: " <> msg )
    Left ( Closed code reason _ ) ->
      Left ( "connection closed: " <> show code <> " " <> show reason )
    Left ( Error err ) -> Left err

recv :: WebSocket -> Aff Event
recv ( WebSocket { events } ) = Q.draw events

send :: WebSocket -> String -> Effect Unit
send ( WebSocket { caps } ) = caps.send

close :: WebSocket -> Effect Unit
close ( WebSocket { caps } ) = caps.close

wsUrl :: Options -> Target -> String
wsUrl opts ( Target { webSocketDebuggerUrl } ) =
  if opts.secure
    then replace ws "wss:" webSocketDebuggerUrl
    else webSocketDebuggerUrl
  where
    ws = unsafePartial ( fromRight ( regex "^ws:" RF.ignoreCase ) )
