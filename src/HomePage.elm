module HomePage exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import PokeTypes exposing (..)


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


gameview : Model -> Html Msg
gameview model =
    let
        ( leftCounter, centerCounter, rightCounter ) =
            model.counterWith
    in
    div [ class "game-container" ]
        [ div [ class "title-section" ]
            [ h1 [ class "center" ] [ text "PokeType Battle" ]
            , h2 [ class "center" ] [ text ("Score: " ++ String.fromInt model.score) ]
            , p [ class "center" ] [ text ("Time Remaining: " ++ String.fromInt model.remainingseconds) ]
            , div
                [ style "display" "flex"
                , style "justify-content" "center"
                ]
                [ if model.remainingseconds == 0 then
                    button
                        [ style "width" "300px"
                        , style "height" "40px"
                        , onClick Start
                        ]
                        [ text "Play Again?" ]

                  else
                    case model.lastRoundSummary of
                        Just summary ->
                            div
                                [ class "center"
                                , style "background-color" summary.color
                                , style "min-width" "300px"
                                , style "min-height" "40px"
                                ]
                                [ text summary.message ]

                        Nothing ->
                            div [] []
                ]
            ]
        , if model.remainingseconds == 0 then
            div [] []

          else
            div
                [ class "opponent center"
                , style "margin" "30px"
                , style "background-color" (getColor model.opponent)
                ]
                [ div []
                    [ p [] [ text "Opponent is:" ]
                    , p [ class "type-name" ] [ text (getName model.opponent) ]
                    ]
                ]
        , counterDiv "verses1 center" leftCounter model
        , counterDiv "verses2 center" centerCounter model
        , counterDiv "verses3 center" rightCounter model
        ]


counterDiv : String -> PokeType -> Model -> Html Msg
counterDiv classStr pokeType model =
    if model.remainingseconds == 0 then
        div [] []

    else
        button
            [ class classStr
            , style "background-color" (getColor pokeType)
            --, onClick (CounterWith pokeType)
            ]
            [ div []
                [ p [] [ text "Counter with:" ]
                , p [ class "type-name" ] [ text (getName pokeType) ]
                ]
            ]



type Msg
    = Start


type State
    = Initial
    | Started


type alias LastRoundSummary =
    { message : String
    , color : String
    , scoreDelta : Int
    }


type alias Model =
    { state : State
    , opponent : PokeType
    , score : Int
    , remainingseconds : Int
    , lastRoundSummary : Maybe LastRoundSummary
    , nextOpponentList : List PokeType
    , strongAgainst : PokeType
    , neutralAgainst : PokeType
    , weakAgainst : PokeType
    , counterWith : ( PokeType, PokeType, PokeType )
    }


gameDuration =
    30


initialModel : Model
initialModel =
    { state = Initial
    , opponent = Grass
    , score = 0
    , remainingseconds = gameDuration
    , lastRoundSummary = Nothing
    , nextOpponentList = allTypes
    , strongAgainst = ShouldNeverOccur
    , neutralAgainst = ShouldNeverOccur
    , weakAgainst = ShouldNeverOccur
    , counterWith = ( ShouldNeverOccur, ShouldNeverOccur, ShouldNeverOccur )
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
            , Cmd.none
            )


subscriptions : model -> Sub msg
subscriptions model =
    Sub.none
