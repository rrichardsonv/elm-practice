module Main exposing (..)

import Dict exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Random exposing (..)
import Time exposing (..)


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Maybe v
    = Just Int
    | Nothing



-- Probably the DRYest way to do this would be to store them as nested tupples


type alias Roll =
    Int


type alias Model =
    Dict Int Int


class : String -> Attribute msg
class name =
    attribute "class" name


init : ( Model, Cmd Msg )
init =
    ( Dict.fromList
        [ ( 4, 0 )
        , ( 6, 0 )
        , ( 8, 0 )
        , ( 10, 0 )
        , ( 12, 0 )
        , ( 20, 0 )
        ]
    , Cmd.none
    )



-- TODO: Need to figure out if its even possible to access a key/field/whatever
-- using an accessor held in a variable
-- Update n, val ->
--   List.map n model


stylesheet filename =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" ("./css/" ++ filename)
            ]

        children =
            []
    in
        node tag attrs children


updateDie : Maybe Int -> Maybe Int
updateDie i =
    case i of
        Nothing ->
            Just 0

        Just i ->
            i


type Msg
    = Roll



-- | Roll
-- | Update
-- | RollSix
-- | RollEight
-- | RollTen
-- | RollTwelve
-- | RollTwenty
-- | UpdateFour Int
-- | UpdateSix Int
-- | UpdateEight Int
-- | UpdateTen Int
-- | UpdateTwelve Int
-- | UpdateTwenty Int


diceRng : Int -> Int
diceRng m =
    Random.int 1 m


whateverToInt whatever =
    String.toInt <| toString <| whatever


rngSeed : Seed
rngSeed =
    Random.initialSeed <| whateverToInt <| Time.now


rollEm msg =
    Random.step ( rngSeed, diceRng <| whateverToInt msg )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( Dict.update ((whateverToInt <| msg), (\_ -> rollEm msg), model), Cmd.none )


getDiceFace model n =
    toString (Dict.get n model)


divDot : String -> List (Html msg) -> Html msg
divDot klass children =
    div [ class klass ] children


rollBtn : Msg -> Int -> Html Msg
rollBtn cmd n =
    button [ class "btn", onClick (cmd n) ] [ text "Roll" ]


dieWrapper : String -> String -> Html msg
dieWrapper dieClass val =
    divDot "wrap"
        [ divDot "shape"
            [ divDot dieClass []
            ]
        , divDot "overlay"
            [ divDot "text"
                [ span [] [ text val ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "item" ]
            [ dieWrapper "triangle-up" (getDiceFace model 4)
            , rollBtn Roll 4
            ]
        , div [ class "item" ]
            [ dieWrapper "square" (getDiceFace model 6)
            , rollBtn Roll 6
            ]
        , div [ class "item" ]
            [ dieWrapper "diamond" (getDiceFace model 8)
            , rollBtn Roll 8
            ]
        , div [ class "item" ]
            [ dieWrapper "hexagon" (getDiceFace model 10)
            , rollBtn Roll 10
            ]
        , div [ class "item" ]
            [ dieWrapper "octagon" (getDiceFace model 12)
            , rollBtn Roll 12
            ]
        , div [ class "item" ]
            [ divDot "wrap"
                [ divDot "shape"
                    [ divDot "hexagon"
                        [ divDot "triangle-down" [] ]
                    ]
                , divDot "overlay"
                    [ divDot "text"
                        [ span [] [ text (getDiceFace model 20) ]
                        ]
                    ]
                ]
            , rollBtn Roll 20
            ]
        , stylesheet "Main.css"
        , stylesheet "Shapes.css"
        ]
