module Threading exposing
  ( ThreadId
  , Threaded
  , Msg (Call)
  , Model
  , init
  , update
  , subscriptions
  )

{-|
@docs ThreadId, Threaded

@docs Msg, Model, init

@docs update

@docs subscriptions
-}


import IntDict exposing (IntDict)


{-| -}
type alias ThreadId = Int

{-|
The API that your ports should understand: there's no need
to manipulate the `threadId` field; we can just pass it to our output:

```js
app.ports.getFoo.subscribe(function(threadedInput) {
  var threadedOutput = {
    threadId = threadedInput.threadId,
    payload  = doSomething(threadedInput.payload)
  };
  app.ports.gotFoo.send(threadedOutput);
});
```
-}
type alias Threaded x =
  { threadId : ThreadId
  , payload  : x
  }

mkThreaded : x -> Model o a -> (Threaded x, Model o a)
mkThreaded x model =
  ( { threadId = model.threadId, payload = x }
  , { model | threadId = model.threadId + 1 }
  )

{-|
To invoke your threaded port from Elm, just use `Call` with a continuation
for handling the output:

```elm
-- a simple session verifier
type Msg
  = HashMsg (Threading.Msg String String Msg) -- from strings to strings
  -- | ...

type alias Model =
  { hasher : Threading.Model String Msg
  -- , ...
  }

update action model =
  case action of
    -- ...
    SecurePOST data ->
      ( -- ...
      , Task.perform Debug.crash HashMsg <|
          Task.succeed <| Threading.Call data <| \hashedData ->
            -- do something with the hashed data, where this
               expression here should have a type `Cmd Msg`.
      )
```
-}
type Msg i o a
  = Call i (o -> Cmd a)
  | GotIncoming (Threaded o)

{-| -}
type alias Model o a =
  { threadId : ThreadId
  , threads  : IntDict (o -> Cmd a)
  }

{-| -}
init : Model o a
init = { threadId = 0, threads = IntDict.empty }

{-|
Given an outgoing port that can accept `Threaded` data, I'll build you a
traditional Elm update function, except where the command returned may
either be another `Msg` for this threading mechanism, or an end-user `a`
message; the type of the message you wish to use this component in.

**Note**: this mechanism allows for multiple asynchronous javascript ports to
be issued with this component, but we still expect the threaded component
to be a singleton in your entire app; if duplicate uses of a `Sub` port
exist in your Elm app, then **all** instances will be called whenever
it's called from javascript, thus breaking isolation. Ports should be treated
as global variables in Elm.
-}
update : (Threaded i -> Cmd (Msg i o a))
      -> Msg i o a
      -> Model o a
      -> (Model o a, Cmd (Result (Msg i o a) a))
update outgoingPort action model =
  case action of
    Call x onComplete ->
      let (x', model') = mkThreaded x model
      in  ( { model' | threads = IntDict.insert x'.threadId onComplete model'.threads }
          , Cmd.map Err <| outgoingPort x'
          )
    GotIncoming x ->
      case IntDict.get x.threadId model.threads of
        Nothing -> (model, Cmd.none) -- silent failure
        Just onComplete ->
          ( { model | threads = IntDict.remove x.threadId model.threads }
          , Cmd.map Ok <| onComplete x.payload
          )


{-|
Given a subscription that sends `Threaded` data, we'll turn it into a subscription
the threading mechanism can understand. Again, as stated in `update`, you cannot
use the same port in multiple threading mechanisms and expect them to be isolated.
Every port must be unique, and should be treated as shared state in Elm.
-}
subscriptions : Sub (Threaded o) -> Sub (Msg i o a)
subscriptions incomingPort = Sub.map GotIncoming incomingPort
