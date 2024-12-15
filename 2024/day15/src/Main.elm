module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import String

-- MAIN
main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
    { board : Board
    , input : String
    , step : Int
    , operations : List Char
    }

type alias Board =
  {
    grid : Grid,
    width : Int,
    height : Int,
    player : (Int, Int)
  }

type alias Grid =
    Array (Array Char)

init : Model
init =
  { board = {
      grid = Array.empty, width = 0, height = 0, player = (0, 0)
  }
    , input = ""
    , step = 0
    , operations = String.toList ">>>>>"
  }

-- UPDATE
type Msg
    = UpdateInput String | UpdateStep (Int -> Int)

isValidOperation : Char -> Bool
isValidOperation c =
    c == '>' || c == '<' || c == '^' || c == 'v'

updateInput : String -> Model -> Model
updateInput newInput model =
    let
        (gridLines, operationString) = splitBlocks newInput

        lines = String.lines gridLines
        operations = List.filter isValidOperation (String.toList operationString)

        height =
            List.length lines

        width =
            Maybe.withDefault 0 (List.maximum (List.map String.length lines))

        grid =
            lines
                |> List.map String.toList
                |> List.map Array.fromList
                |> Array.fromList
    in
    { model
        | input = newInput
        , board = {
          grid = grid
          , width = width
          , height = height
          , player = findPlayer grid width height
        },
        operations = operations
    }

splitBlocks : String -> (String, String)
splitBlocks input =
  case String.split "\n\n" input of
    [gridLines, operationString] ->
      (gridLines, operationString)
    [gridLines] ->
      (gridLines, "")
    _ ->
      ("", "")


updateStep : (Int -> Int) -> Model -> Model
updateStep f model =
    { model | step = Basics.max (f model.step) 0 }

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput newInput -> updateInput newInput model
        UpdateStep f -> updateStep f model

-- VIEW
view : Model -> Html Msg
view model =
  let
      currentBoard = applyOperations model.board (List.take model.step model.operations)
  in
    div 
        [ style "padding" "20px"
        , style "font-family" "Arial, sans-serif"
        ]
        [ div 
            [ style "margin-bottom" "20px" ]
            [ textarea
                [ value model.input
                , onInput UpdateInput
                , rows 20
                , cols 50
                , style "padding" "10px"
                , style "font-family" "monospace"
                , style "font-size" "16px"
                , style "border" "2px solid #ccc"
                , style "border-radius" "4px"
                ]
                []
            ]
        , div 
            [ style "margin-bottom" "10px"
            , style "color" "#333"
            ]
            [ text ("Grid dimensions: " ++ String.fromInt model.board.width ++ "x" ++ String.fromInt model.board.height) ]
        , div 
            [ style "margin-bottom" "10px"
            , style "color" "#333"
            ]
            [ text ("Operations: " ++ (List.length model.operations |> String.fromInt)) ]
        , pre
            [ style "font-family" "monospace"
            , style "font-size" "16px"
            , style "border" "2px solid #ccc"
            , style "border-radius" "4px"
            , style "padding" "10px"
            ]
            [ text (Array.toList currentBoard.grid |> List.map Array.toList |> List.map String.fromList |> String.join "\n") ]
        , div 
            [ style "color" "#333" ]
            [ text ("Character at (1,1): " ++ getElementAsString model.board.grid 1 1) ]
        , div 
            [ style "color" "#333" ]
            [ text ("Player position: " ++ (currentBoard.player |> formatTuple)) ]
        , div 
            [ style "color" "#333" ]
            [ text ("Score: " ++ (score currentBoard.grid |> String.fromInt)) ]
        , div []
          [
            button [ onClick (UpdateStep (\x -> 0)) ] [ text "Reset"],
            button [ onClick (UpdateStep ((+) 1)) ] [ text "Next step"],
            div [] [ text (String.fromInt model.step) ],
            button [ onClick (UpdateStep (\x -> x - 1)) ] [ text "Previous step"],
            button [ onClick (UpdateStep (\x -> List.length model.operations)) ] [ text "Last step"]
          ]]

-- HELPERS

formatTuple : (Int, Int) -> String
formatTuple (x, y) =
    "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"

getElement : Array (Array Char) -> Int -> Int -> Maybe Char
getElement grid x y =
    Array.get y grid
        |> Maybe.andThen (Array.get x)

setElement : Array (Array Char) -> Int -> Int -> Char -> Array (Array Char)
setElement grid x y char =
    Array.set y (Array.set x char (Array.get y grid |> Maybe.withDefault Array.empty)) grid

getElementAsString : Array (Array Char) -> Int -> Int -> String
getElementAsString grid x y =
    case getElement grid x y of
        Just char ->
            String.fromChar char

        Nothing ->
            "Invalid position"

-- Simulation
applyOperations : Board -> List Char -> Board
applyOperations board operations =
  List.foldl stepGrid board operations

stepGrid : Char -> Board -> Board
stepGrid operation board =
  let
    (dx, dy) = direction operation
    (x, y) = board.player
  in
    case move board.grid '@' (dx, dy) (x, y) of
      Just newGrid ->
        { board |
          grid = newGrid,
          player = (x + dx, y + dy)
        }
      Nothing ->
        board

move : Grid -> Char -> (Int, Int) -> (Int, Int) -> Maybe Grid
move grid element (dx, dy) (x, y) =
  let
    (newX, newY) = (x + dx, y + dy)
    atNewPosition = getElement grid newX newY
  in
    case atNewPosition of
      Just '.' ->
        Just (setElement (setElement grid x y '.') newX newY element)
      Just 'O' ->
        case move grid 'O' (dx, dy) (newX, newY) of
          Just newGrid ->
            Just (setElement (setElement newGrid x y '.') newX newY element)
          Nothing ->
            Nothing
      _ ->
        Nothing

-- The player is represented by the character '@'
findPlayer : Grid -> Int -> Int -> (Int, Int)
findPlayer grid width height =
    List.range 0 height
      |> List.map (\y -> List.range 0 width |> List.map (\x -> (x, y)))
      |> List.concat
      |> List.filter (\(x, y) -> getElement grid x y == Just '@')
      |> List.head
      |> Maybe.withDefault (0, 0)

direction : Char -> (Int, Int)
direction c =
  case c of
    '>' -> (1, 0)
    '<' -> (-1, 0)
    '^' -> (0, -1)
    'v' -> (0, 1)
    _ -> (0, 0)

score : Grid -> Int
score grid =
  crateIndexes grid
    |> List.map gpsScore
    |> List.sum

gpsScore : (Int, Int) -> Int
gpsScore (x, y) =
  x + 100 * y

crateIndexes : Grid -> List (Int, Int)
crateIndexes grid =
    grid
        |> Array.toIndexedList
        |> List.concatMap
            (\(y, row) ->
                Array.toIndexedList row
                    |> List.filter (\(_, c) -> c == 'O')
                    |> List.map (\(x, _) -> (x, y))
            )
