module HomePage exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)

view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Welcome to PokeType Battle!" ]
        ,p [] [ text "Defeat as many as you can in 30 seconds"]
        ,a [href "https://www.pinterest.ca/pin/144889312992375493/"] [ text "Click for Pokemon Effectiveness Chart"]
        , p [] [
            button [] [text "Start"]
        ]
        
        ]

main : Html msg
main =
    view "dummy model"
    