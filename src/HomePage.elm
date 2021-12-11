module HomePage exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


view model =
    if model.state == Started then
        gameview model

    else
        div [ class "jumbotron" ]
            [ h1 [] [ text "Welcome to PokeType Battle!" ]
            , p [] [ text "Defeat as many as you can in 30 seconds" ]
            , a [ href "https://www.pinterest.ca/pin/144889312992375493/" ] [ text "Click for Pokemon Effectiveness Chart" ]
            , p []
                [ button [ onClick Start ] [ text "Start" ]
                ]
            ]


gameview model =
    div []
        [ text "Game has started"
        ]


type Msg
    = Start


type State
    = Initial
    | Started


type alias Model =
    { state : State
    }


initialModel : Model
initialModel =
    { state = Initial
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Start ->
            ( { model | state = Started }
            ,Cmd.none
            )


subscriptions : model -> Sub msg
subscriptions model =
    Sub.none
