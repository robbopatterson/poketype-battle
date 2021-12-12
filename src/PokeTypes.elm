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

listWeakAgainst : PokeType -> List PokeType
listWeakAgainst pokeType =
    case pokeType of
        Grass ->
            [ Fire, Ice, Poison, Flying, Bug ]

        Rock ->
            [Water, Grass, Fighting, Ground, Steel]
            
        Ice ->
            [Fire, Fighting, Rock, Steel]
            
        Dragon ->
            [Ice, Dragon, Fairy]
            
        Dark ->
            [Fighting, Bug, Fairy]
            
        Psychic ->
            [Bug,Ghost,Dark]
            
        Bug ->
            [Fire,Flying,Rock]
            
        Flying ->
            [Electric,Ice,Rock]
            
        Steel ->
            [Fire,Fighting,Ground]
            
        Fire ->
            [Water,Rock,Ground]
            
        Fighting ->
            [Flying,Psychic,Fairy]
            
        Ground ->
            [Water,Grass,Ice]
            
        Ghost ->
            [Ghost,Dark]
            
        Poison ->
            [Ground,Psychic]
            
        Water ->
            [Electric,Grass]
            
        Fairy ->
            [Poison,Steel]
            
        Electric ->
            [Ground]

        Normal ->
            [Fighting]

        ShouldNeverOccur ->
            []

listNeutralAgainst : PokeType -> List PokeType
listNeutralAgainst pokeType =
    let
        allWeakAndStrongAndSelf =
            pokeType :: listWeakAgainst pokeType ++ listStrongAgainst pokeType
    in
    List.filter (\pt -> not (List.member pt allWeakAndStrongAndSelf)) allTypes

listStrongAgainst : PokeType -> List PokeType
listStrongAgainst pokeType =
    List.map
        (\( _, strongAgainst ) -> strongAgainst)
        (listStrongAgainstAndWhy pokeType)


listStrongAgainstAndWhy : PokeType -> List ( String, PokeType )
listStrongAgainstAndWhy pokeType =
    case pokeType of
        Grass ->
            [ ( "soaks up", Water )
            , ( "grows over", Ground )
            , ( "covers", Rock )
            ]

        Rock ->
            [ ( "impervious to", Fire )
            , ( "smashs", Ice )
            , ( "smashs", Flying )
            , ( "smashs", Bug )
            ]

        Ice ->
            [ ( "covers", Grass )
            , ( "covers", Ground )
            , ( "impars", Flying )
            , ( "freeze", Dragon )
            ]

        Dragon ->
            [ ( "matches", Dragon )
            ]

        Dark ->
            [ ( "scares", Psychic )
            ]

        Psychic ->
            [ ( "remotely defeats", Fighting )
            , ( "remotely defeats", Poison )
            ]

        Bug ->
            [ ( "eat", Grass )
            , ( "distracts", Psychic )
            , ( "swarms", Dark )
            ]

        Flying ->
            [ ( "poops on", Grass )
            , ( "swoops over", Fighting )
            , ( "eats", Bug )
            ]

        Steel ->
            [ ( "cuts", Ice )
            , ( "crushs", Rock )
            , ( "destroys", Fairy )
            ]

        Fire ->
            [ ( "burns", Grass )
            , ( "melts", Ice )
            , ( "attracts", Bug )
            , ( "melts", Steel )
            ]

        Fighting ->
            [ ( "beat up", Normal )
            , ( "cracks", Ice )
            , ( "chops", Rock )
            , ( "punchs out", Dark )
            , ( "out manovers", Steel )
            ]

        Ground ->
            [ ( "covers", Fire )
            , ( "insolates", Electric )
            , ( "soaks up", Poison )
            , ( "supports", Rock )
            , ( "unscathed by", Steel )
            ]

        Ghost ->
            [ ( "scares", Psychic )
            , ( "scares", Ghost )
            ]

        Poison ->
            [ ( "kills", Grass )
            , ( "kills", Fairy )
            ]

        Water ->
            [ ( "puts out", Fire )
            , ( "erodes", Ground )
            , ( "erodes", Rock )
            ]

        Fairy ->
            [ ( "evades", Fighting )
            , ( "evades", Dragon )
            , ( "always defeats", Dark )
            ]

        Electric ->
            [ ( "flows through", Water )
            , ( "strikes", Flying )
            ]

        Normal ->
            []

        ShouldNeverOccur ->
            []
