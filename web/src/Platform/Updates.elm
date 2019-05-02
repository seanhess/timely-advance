module Platform.Updates exposing (Event, Model(..), Msg(..), Updates, base, command, event, initWith, modify, set, updates)


type Msg
    = Msg


type Model
    = Model


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


initWith : (subModel -> parentModel) -> (subMsg -> parentMsg) -> ( subModel, Cmd subMsg ) -> ( parentModel, Cmd parentMsg )
initWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
