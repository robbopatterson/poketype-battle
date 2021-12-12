module HomePage exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import PokeTypes exposing (..)
import Random exposing (..)
import Random.List exposing (..)


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
            , onClick (CounterWith pokeType)
            ]
            [ div []
                [ p [] [ text "Counter with:" ]
                , p [ class "type-name" ] [ text (getName pokeType) ]
                ]
            ]


type Msg
    = Start
    | CounterWith PokeType
    | RandomizedOpponents (List PokeType)
    | RandomizedWeakAgainst ( Maybe PokeType, List PokeType )
    | RandomizedStrongAgainst ( Maybe PokeType, List PokeType )
    | RandomizedNeutralAgainst ( Maybe PokeType, List PokeType )


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


startOpponentSelection : Cmd Msg
startOpponentSelection =
    generate RandomizedOpponents (Random.List.shuffle allTypes)


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, startOpponentSelection )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getScoreDelta : PokeType -> PokeType -> Int
getScoreDelta opponentType myCounterType =
    let
        isCounterStrong =
            List.member myCounterType (listStrongAgainst opponentType)

        isCounterWeak =
            List.member myCounterType (listWeakAgainst opponentType)
    in
        case (isCounterStrong, isCounterWeak) of
           (True, True) ->
                0
           (False, False) ->
                0
           (True, False) ->
                -10
           (False, True) ->
                10


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | state = Started }
            , Cmd.none
            )

        CounterWith pokeType ->
            let
                scoreDelta =
                    getScoreDelta model.opponent pokeType

                ( newOpponent, newOpponentList ) =
                    rotateOpponents model.nextOpponentList
            in
            ( { model
                | opponent = newOpponent
                , nextOpponentList = newOpponentList
                , score = model.score + scoreDelta
              }
            , startWeakAgainstSelection newOpponent
            )

        RandomizedOpponents pokeTypes ->
            let
                ( newOpponent, newOpponentList ) =
                    rotateOpponents pokeTypes
            in
            ( { model | opponent = newOpponent, nextOpponentList = newOpponentList }
            , startWeakAgainstSelection newOpponent
            )

        RandomizedWeakAgainst ( maybePokeType, _ ) ->
            let
                pokeType =
                    case maybePokeType of
                        Just pt ->
                            pt

                        Nothing ->
                            model.opponent
            in
            ( { model | weakAgainst = pokeType }
            , startStrongAgainstSelection model.opponent
            )

        RandomizedStrongAgainst ( maybePokeType, _ ) ->
            let
                pokeType =
                    case maybePokeType of
                        Just pt ->
                            pt

                        Nothing ->
                            model.opponent
            in
            ( { model | strongAgainst = pokeType }
            , startNeutralAgainstSelection model.opponent
            )

        RandomizedNeutralAgainst ( maybePokeType, _ ) ->
            let
                pokeType =
                    case maybePokeType of
                        Just pt ->
                            pt

                        Nothing ->
                            model.opponent
            in
            ( { model | neutralAgainst = pokeType, counterWith = ( model.strongAgainst, pokeType, model.weakAgainst ) }
            , Cmd.none
            )


startWeakAgainstSelection : PokeType -> Cmd Msg
startWeakAgainstSelection opponent =
    let
        weakAgainstList =
            listWeakAgainst opponent
    in
    generate RandomizedWeakAgainst (Random.List.choose weakAgainstList)


startStrongAgainstSelection : PokeType -> Cmd Msg
startStrongAgainstSelection opponent =
    let
        strongAgainstList =
            listStrongAgainst opponent
    in
    generate RandomizedStrongAgainst (Random.List.choose strongAgainstList)


startNeutralAgainstSelection : PokeType -> Cmd Msg
startNeutralAgainstSelection opponent =
    let
        neutralAgainstList =
            listNeutralAgainst opponent
    in
    generate RandomizedNeutralAgainst (Random.List.choose neutralAgainstList)


rotateOpponents : List PokeType -> ( PokeType, List PokeType )
rotateOpponents pokeTypes =
    let
        head =
            case List.head pokeTypes of
                Just pokeType ->
                    pokeType

                Nothing ->
                    ShouldNeverOccur

        tail =
            case List.tail pokeTypes of
                Just t ->
                    t

                Nothing ->
                    pokeTypes
    in
    ( head, tail ++ [ head ] )


subscriptions : model -> Sub msg
subscriptions model =
    Sub.none
