module Main exposing (..)

import Browser
import Dice
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Random



-- MODEL


type Player
    = Player1
    | Player2


type alias Board =
    List (List Int)


type alias Model =
    { boardDimensions : Int
    , dieSize : Int
    , board1 : Board
    , board2 : Board
    , dieRoll : Int
    , currentPlayer : Player
    }


initialModel : Model
initialModel =
    let
        boardDimensions =
            3
    in
    { boardDimensions = boardDimensions
    , dieSize = 6
    , dieRoll = 1
    , board1 = List.repeat boardDimensions []
    , board2 = List.repeat boardDimensions []
    , currentPlayer = Player1
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Random.generate Roll <| Random.int 1 initialModel.dieSize )



-- UPDATE


type Msg
    = Roll Int
    | AddToColumn Int
    | Reset


addToGameBoard : Board -> Int -> Int -> Board
addToGameBoard gameBoard value laneNumber =
    List.indexedMap
        (\index laneValues ->
            if index == laneNumber then
                laneValues ++ [ value ]

            else
                laneValues
        )
        gameBoard


removeValueFromLane : Int -> List Int -> List Int
removeValueFromLane value =
    List.filterMap
        (\n ->
            if value == n then
                Nothing

            else
                Just n
        )


removeValueFromBoard : Board -> Int -> Int -> Board
removeValueFromBoard gameBoard value laneNumber =
    List.indexedMap
        (\index laneValues ->
            if index == laneNumber then
                removeValueFromLane value laneValues

            else
                laneValues
        )
        gameBoard


isBoardFull : Int -> Board -> Bool
isBoardFull boardDimensions gameBoard =
    (List.length <| List.concat gameBoard) == boardDimensions ^ 2


isLaneFull : Int -> List Int -> Bool
isLaneFull boardDimensions laneValues =
    List.length laneValues == boardDimensions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialModel
            , Random.generate Roll <| Random.int 1 model.dieSize
            )

        Roll value ->
            ( { model | dieRoll = value }, Cmd.none )

        AddToColumn laneNumber ->
            case model.currentPlayer of
                Player1 ->
                    let
                        newBoard1 =
                            addToGameBoard model.board1 model.dieRoll laneNumber

                        newBoard2 =
                            removeValueFromBoard model.board2 model.dieRoll laneNumber
                    in
                    ( { model | board1 = newBoard1, board2 = newBoard2, currentPlayer = Player2 }
                    , Random.generate Roll <| Random.int 1 model.dieSize
                    )

                Player2 ->
                    let
                        newBoard1 =
                            removeValueFromBoard model.board1 model.dieRoll laneNumber

                        newBoard2 =
                            addToGameBoard model.board2 model.dieRoll laneNumber
                    in
                    ( { model | board1 = newBoard1, board2 = newBoard2, currentPlayer = Player1 }
                    , Random.generate Roll <| Random.int 1 model.dieSize
                    )



-- VIEW


centralizedText : List (Attribute Msg) -> String -> Element Msg
centralizedText attributes message =
    column
        ([ width fill, height fill, Font.center ]
            ++ attributes
        )
        [ el [ height fill ] none
        , el [ height fill, width fill ] <| text message
        , el [ height fill ] none
        ]


view : Model -> Html Msg
view model =
    Element.layout [] <| myRowOfStuff model


dieSlot : Color -> Int -> Element Msg
dieSlot color_ value =
    column
        [ width fill, height fill ]
        [ el [ height fill ] none
        , row [ width fill, height fill ]
            [ el [ width fill ] none
            , el [ width fill ] <| showDieRoll color_ value
            , el [ width fill ] none
            ]
        , el [ height fill ] none
        ]


maybeDieSlot : Color -> Maybe Int -> Element Msg
maybeDieSlot color_ value =
    value
        |> Maybe.map (dieSlot color_)
        |> Maybe.withDefault (el [ width fill, height fill ] none)


fillLane : Int -> List Int -> List (Maybe Int)
fillLane boardDimensions laneValues =
    List.map Just laneValues ++ List.repeat (boardDimensions - List.length laneValues) Nothing


lane : Bool -> Int -> Int -> List Int -> Element Msg
lane isTurn boardDimensions laneNumber laneValues =
    let
        maybeValues =
            laneValues |> fillLane boardDimensions |> List.reverse

        canSelect =
            List.length laneValues < boardDimensions && isTurn
    in
    column
        ([ width fill, height fill ]
            ++ condAttrs canSelect
                [ onClick (AddToColumn laneNumber)
                , mouseOver [ Background.color color.lightBlue ]
                ]
        )
    <|
        List.map (maybeDieSlot color.white) maybeValues


board : Bool -> Int -> Board -> Element Msg
board isTurn boardDimensions gameBoard =
    row [ width fill, height fill ] <|
        List.indexedMap (lane isTurn boardDimensions) gameBoard


lane2 : Bool -> Int -> Int -> List Int -> Element Msg
lane2 isTurn boardDimensions laneNumber laneValues =
    let
        maybeValues =
            laneValues |> fillLane boardDimensions

        canSelect =
            List.length laneValues < boardDimensions && isTurn
    in
    column
        ([ width fill, height fill ]
            ++ condAttrs canSelect
                [ onClick (AddToColumn laneNumber)
                , mouseOver [ Background.color color.lightBlue ]
                ]
        )
    <|
        List.map (maybeDieSlot color.white) maybeValues


board2 : Bool -> Int -> Board -> Element Msg
board2 isTurn boardDimensions gameBoard =
    row [ width fill, height fill ] <|
        List.indexedMap (lane2 isTurn boardDimensions) gameBoard


foldLane : Int -> Dict Int (List Int) -> Dict Int (List Int)
foldLane value acc =
    let
        values =
            Dict.get value acc |> Maybe.withDefault []
    in
    Dict.insert value (value :: values) acc


laneTotal : List Int -> Int
laneTotal values =
    let
        values_ =
            List.foldl foldLane Dict.empty values
    in
    values_
        |> Dict.values
        |> List.map (\v_ -> List.sum v_ * List.length v_)
        |> List.sum


boardTotal : Board -> Int
boardTotal boardValues =
    List.sum <| List.map laneTotal boardValues


condAttrs : Bool -> List (Attribute msg) -> List (Attribute msg)
condAttrs cond attrs =
    if cond then
        attrs

    else
        []


showDieRoll : Color -> Int -> Element msg
showDieRoll color_ roll =
    case roll of
        1 ->
            Element.html <| Dice.side1 color_

        2 ->
            Element.html <| Dice.side2 color_

        3 ->
            Element.html <| Dice.side3 color_

        4 ->
            Element.html <| Dice.side4 color_

        5 ->
            Element.html <| Dice.side5 color_

        6 ->
            Element.html <| Dice.side6 color_

        _ ->
            Element.html <| Dice.side0 color_


myRowOfStuff : Model -> Element Msg
myRowOfStuff model =
    let
        isPlayer1Turn =
            model.currentPlayer == Player1

        isPlayer2Turn =
            model.currentPlayer == Player2
    in
    row [ width fill, height fill ]
        [ column [ width fill, height fill, Font.size 50, Background.color color.lightGrey ]
            [ centralizedText (condAttrs isPlayer1Turn [ Font.color color.blue ]) "Jogador 1"
            , centralizedText (condAttrs isPlayer2Turn [ Font.color color.blue ]) "Jogador 2"
            ]
        , column [ width <| fillPortion 4, height fill ]
            [ row [ width fill, height fill ]
                [ el [ width fill, height fill, Background.color color.darkCharcoal ] <|
                    board isPlayer1Turn model.boardDimensions model.board1
                , el [ width fill, height fill, Background.color color.white ] <|
                    centralizedText [ Font.size 100 ] <|
                        String.fromInt <|
                            boardTotal model.board1
                ]
            , row [ width fill, height fill ]
                [ el [ width fill, height fill, Background.color color.darkCharcoal ] <|
                    board2 isPlayer2Turn model.boardDimensions model.board2
                , el [ width fill, height fill, Background.color color.white ] <|
                    centralizedText [ Font.size 100 ] <|
                        String.fromInt <|
                            boardTotal model.board2
                ]
            ]
        , column [ width fill, height fill, Background.color color.lightGrey ]
            [ dieSlot color.black model.dieRoll
            , Input.button
                [ width fill
                , height fill
                , Font.center
                , Font.size 50
                , mouseOver [ Background.color color.lightBlue ]
                ]
                { onPress = Just Reset
                , label = text "Reset"
                }
            ]
        ]


color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    , black = rgb255 0x00 0x00 0x00
    , red = rgb255 0xFF 0x00 0x00
    }



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
