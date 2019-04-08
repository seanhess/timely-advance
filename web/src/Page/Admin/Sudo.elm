module Page.Admin.Sudo exposing (Model, Msg(..), init, label, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Html
import Http
import Json.Encode as Encode
import Platform.Updates exposing (Updates, command, set, updates)
import Route exposing (Admin(..), Route(..))
import Timely.Api as Api exposing (AccountCustomer, AccountInfo, Application, Auth, AuthCode, Bank, Customer, Id(..), Phone, Session, Token, idValue)
import Timely.Components as Components exposing (loadingButton)
import Timely.Resource exposing (Resource(..))
import Timely.Style as Style



-- TODO click to see
--   TODO recent advances (click to see customer info?)
--   TODO recent customers (Ctrl-F to search)
--   TODO recent transactions (click to see customer info?)
--   TODO admin view of individual customer, with recent transactions
-- Or just show all of everything?
-- Everything links to the


type alias Model =
    { secret : String
    , key : Nav.Key
    , session : Resource (Maybe Session)
    , customers : Resource (List AccountCustomer)
    }


type Msg
    = EditSecret String
    | Submit
    | Logout
    | OnBack
    | OnSession (Result Http.Error Session)
    | OnLogin (Result Http.Error Session)
    | OnLogout (Result Http.Error ())
    | OnCustomers (Result Http.Error (List AccountCustomer))
    | GoCustomers
    | GoAdvances
    | GoTransactions


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { secret = ""
      , key = key
      , session = Loading
      , customers = Loading
      }
    , Api.sessionsGet OnSession
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> Updates Model Msg ()
update msg model =
    -- Debug.log (Debug.toString msg) <|
    case msg of
        EditSecret s ->
            updates { model | secret = s }

        Submit ->
            updates model
                |> command (Api.sessionsAuthAdmin OnLogin model.secret)

        OnBack ->
            updates model
                |> command (Route.pushUrl model.key <| Route.Onboard Route.Landing)

        OnLogin (Err e) ->
            updates { model | session = Failed e }

        OnLogin (Ok session) ->
            updates { model | session = Ready (Just session) }
                |> command (Api.getCustomers OnCustomers)

        OnSession (Err _) ->
            updates { model | session = Ready Nothing }

        OnSession (Ok session) ->
            updates { model | session = Ready (Just session) }
                |> command (Api.getCustomers OnCustomers)

        Logout ->
            updates model |> command (Api.sessionsLogout OnLogout)

        OnLogout _ ->
            updates { model | session = Ready Nothing }

        GoCustomers ->
            updates model
                |> command (Api.getCustomers OnCustomers)

        GoTransactions ->
            updates model

        GoAdvances ->
            updates model

        OnCustomers (Err e) ->
            updates { model | customers = Failed e }

        OnCustomers (Ok cs) ->
            updates { model | customers = Ready cs }


view : Model -> Element Msg
view model =
    column Style.page <|
        case model.session of
            Ready (Just s) ->
                if s.isAdmin then
                    [ viewSuperuser model s ]

                else
                    [ viewLogin model ]

            Loading ->
                [ Components.spinner ]

            Failed e ->
                [ viewLogin model, viewProblems e ]

            _ ->
                [ viewLogin model ]


viewLogin : Model -> Element Msg
viewLogin model =
    column Style.section
        [ row [ spacing 15, width fill ]
            [ el Style.header (text "Sudo")
            , row [ alignRight ] [ Components.close OnBack ]
            ]
        , Input.text []
            { text = model.secret
            , placeholder = Nothing
            , onChange = EditSecret
            , label = label "Passphrase"
            }
        , Input.button (Style.button Style.primary)
            { onPress = Just Submit
            , label = Element.text "Go"
            }
        ]


viewSuperuser : Model -> Session -> Element Msg
viewSuperuser model session =
    column Style.section
        [ wrappedRow [ width fill ]
            [ el Style.header (text "Welcome! ")
            , Input.button [ Style.link, alignRight ] { onPress = Just Logout, label = text "Logout" }
            ]
        , paragraph [] [ text "You are as handsome as you are intelligent" ]
        , wrappedRow [ spacing 10 ]
            [ Input.button [ Style.link ] { onPress = Just GoCustomers, label = text "Customers" }
            , Input.button [ Style.link ] { onPress = Just GoTransactions, label = text "Transactions" }
            , Input.button [ Style.link ] { onPress = Just GoAdvances, label = text "Advances" }
            ]
        , resource viewCustomers model.customers
        ]


resource : (a -> Element Msg) -> Resource a -> Element Msg
resource f res =
    case res of
        Ready a ->
            f a

        _ ->
            text ""


viewCustomers : List AccountCustomer -> Element Msg
viewCustomers cs =
    column [ spacing 10 ] (List.map viewCustomer cs)



-- shoot I need their phone number too


viewCustomer : AccountCustomer -> Element Msg
viewCustomer ac =
    column [ spacing 10, Background.color Style.gray, padding 10 ]
        [ link []
            { url = Route.url (Route.Account ac.account.accountId Route.AccountMain)
            , label =
                row [ spacing 5, Font.bold ]
                    [ text ac.customer.firstName
                    , text (Maybe.withDefault "" ac.customer.middleName)
                    , text ac.customer.lastName
                    ]
            }
        , text ac.customer.email
        , text ac.account.phone
        ]


label : String -> Input.Label Msg
label t =
    Input.labelAbove [ Font.size 14 ] (text t)


viewProblems : Http.Error -> Element Msg
viewProblems _ =
    el [ Style.error ] (text "Nope")
