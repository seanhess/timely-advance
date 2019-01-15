module Platform.Updates exposing (Event, Evt(..), Model(..), Msg(..), Updates, base, command, event, modify, set, updates)


type Msg
    = Msg


type Model
    = Model


type Evt
    = Evnt


type alias Updates model msg event =
    ( model, Cmd msg, Event event )


type alias Event a =
    Maybe a


base : model -> Updates model msg event
base mod =
    ( mod, Cmd.none, Nothing )


updates : model -> Updates model msg event
updates =
    base


set : model -> Updates model msg event -> Updates model msg event
set mod ( _, msg, ev ) =
    ( mod, msg, ev )


modify : (model -> model) -> Updates model msg event -> Updates model msg event
modify up ( mod, cmd, ev ) =
    ( up mod, cmd, ev )


command : Cmd msg -> Updates model msg event -> Updates model msg event
command cmd2 ( mod, cmd1, ev ) =
    ( mod, Cmd.batch [ cmd1, cmd2 ], ev )


event : evt -> Updates model msg evt -> Updates model msg evt
event evt ( mod, cmd, _ ) =
    ( mod, cmd, Just evt )



-- update : Msg -> Model -> Updates Model Msg Evt
-- update msg model =
--     let
--         updates =
--             base model
--     in
--     case msg of
--         Update a ->
--             updates
--                 |> set { model | account = a }
--         Submit ->
--             updates
--                 |> set { model | status = Loading }
--                 |> event PlaidLinkOpen
--         CompletedSignup (Err e) ->
--             updates
--                 |> set { model | status = Complete [ "Signup server error" ] }
--         CompletedSignup (Ok a) ->
--             -- TODO redirect to accounts page
--             updates
--                 |> set { model | status = Complete [] }
--                 |> command (Nav.pushUrl model.key (Route.url Route.Accounts))
