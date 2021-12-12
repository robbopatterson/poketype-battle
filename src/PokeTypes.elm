module PokeTypes exposing (..)

type PokeType
    = Fairy
    | Steel
    | Dark
    | Dragon
    | Grass
    | Water
    | Ground
    | Rock
    | Fire
    | Ice
    | Poison
    | Flying
    | Bug
    | Psychic
    | Fighting
    | Ghost
    | Electric
    | Normal
    | ShouldNeverOccur

allTypes : List PokeType
allTypes =
    [ Fairy
    , Steel
    , Dark
    , Dragon
    , Grass
    , Water
    , Ground
    , Rock
    , Fire
    , Ice
    , Poison
    , Flying
    , Bug
    , Psychic
    , Fighting
    , Ghost
    , Electric
    , Normal
    ]
-- NOTE: ShouldNeverOccur is not listed in allTypes

getName : PokeType -> String
getName pokeType = 
    case pokeType of
        Grass ->
            "Grass"

        Fairy ->
            "Fairy"

        Steel ->
            "Steel"

        Dark ->
            "Dark"

        Dragon ->
            "Dragon"

        Water ->
            "Water"

        Ground ->
            "Ground"

        Rock ->
            "Rock"

        Fire ->
            "Fire"

        Ice ->
            "Ice"

        Poison ->
            "Poison"

        Flying ->
            "Flying"

        Bug ->
            "Bug"

        Psychic ->
            "Psychic"

        Fighting ->
            "Fighting"

        Ghost ->
            "Ghost"

        Electric ->
            "Electric"

        Normal ->
            "Normal"

        ShouldNeverOccur ->
            "INTERNAL Error. This should not be used here."

getColor : PokeType -> String
getColor pokeType =
    case pokeType of
        Grass ->
            "#77C850"

        Fairy ->
            "#DEA4DF"

        Steel ->
            "#B8B8D0"

        Dark ->
            "#705848"

        Dragon ->
            "#7039F9"

        Water ->
            "#6890EF"

        Ground ->
            "#E1BF68"

        Rock ->
            "#B8A037"

        Fire ->
            "#F1802F"

        Ice ->
            "#98D8D8"

        Poison ->
            "#A0409F"

        Flying ->
            "#A891F0"

        Bug ->
            "#A8B820"

        Psychic ->
            "#F85888"

        Fighting ->
            "#C03028"

        Ghost ->
            "#715698"

        Electric ->
            "#F8D030"

        Normal ->
            "#A7A878"

        ShouldNeverOccur ->
            ""
