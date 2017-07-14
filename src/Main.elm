module Main exposing (main)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Html.Events
import Array exposing (Array)
import Time exposing (every, millisecond)


type Msg
    = NoOp
    | ToggleAt Int Int
    | ToggleRunning
    | NextGeneration


type Cell
    = Alive
    | Dead


type alias Grid a =
    { items : Array a, width : Int }


type alias Model =
    { grid : Grid Cell
    , running : Bool
    }


makeGrid : Int -> Int -> a -> Grid a
makeGrid width height default =
    { items = Array.repeat (width * height) default
    , width = width
    }


flattenGrid : (Int -> Int -> a -> b) -> Grid a -> List b
flattenGrid f g =
    List.map (\( i, c ) -> f (i % g.width) (i // g.width) c) (Array.toIndexedList g.items)


gridGetAt : Int -> Int -> Grid a -> Maybe a
gridGetAt x y g =
    let
        i =
            (x + y * g.width)
    in
        Array.get i g.items


gridSetAt : Int -> Int -> a -> Grid a -> Grid a
gridSetAt x y a g =
    let
        i =
            (x + y * g.width)
    in
        { g | items = Array.set i a g.items }


mapAt : Grid a -> Int -> Int -> (a -> a) -> Grid a
mapAt g x y f =
    case gridGetAt x y g of
        Just cell ->
            gridSetAt x y (f cell) g

        Nothing ->
            g


nextGeneration : Grid Cell -> Grid Cell
nextGeneration g =
    { g | items = Array.indexedMap (nextGenerationAt g) g.items }


nextGenerationAt : Grid Cell -> Int -> Cell -> Cell
nextGenerationAt g i cur =
    let
        liveNeighbours =
            countLiveNeighbours i g
    in
        case cur of
            Alive ->
                if liveNeighbours < 2 || liveNeighbours > 3 then
                    Dead
                else
                    Alive

            Dead ->
                if liveNeighbours == 3 then
                    Alive
                else
                    Dead


countLiveNeighbours : Int -> Grid Cell -> Int
countLiveNeighbours i grid =
    let
        above =
            i - grid.width

        below =
            i + grid.width

        neighbourCoords =
            [ above - 1, above, above + 1, i - 1, i + 1, below - 1, below, below + 1 ]

        neighbours =
            List.map (\pos -> Array.get pos grid.items) neighbourCoords
    in
        List.length (List.filter (\n -> n == Just Alive) neighbours)


toggle : Cell -> Cell
toggle c =
    case c of
        Alive ->
            Dead

        Dead ->
            Alive


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { grid = makeGrid 50 50 Dead, running = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleAt x y ->
            ( { model | grid = mapAt model.grid x y toggle }, Cmd.none )

        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        NextGeneration ->
            let
                nextGrid =
                    nextGeneration model.grid

                changed =
                    nextGrid /= model.grid
            in
                ( { model | grid = nextGrid, running = (model.running && changed) }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ toggleRunningLink model, nextGenerationLink model ]
        , svg [ width "50%", height "50%", viewBox "0 0 500 500" ]
            (flattenGrid cellToSvg model.grid)
        ]


toggleRunningLink : Model -> Html Msg
toggleRunningLink model =
    Html.button [ Html.Events.onClick ToggleRunning ]
        [ Html.text
            (if model.running then
                "Stop"
             else
                "Start"
            )
        ]


nextGenerationLink : Model -> Html Msg
nextGenerationLink model =
    Html.button [ Html.Events.onClick NextGeneration ]
        [ Html.text "Next generation"
        ]


cellToSvg : Int -> Int -> Cell -> Svg Msg
cellToSvg x_ y_ c =
    let
        colour =
            case c of
                Alive ->
                    "black"

                Dead ->
                    "white"
    in
        rect [ x (toString (10 * x_)), y (toString (10 * y_)), width "10", height "10", fill colour, stroke "#ddd", onClick (ToggleAt x_ y_) ] []


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        every (100 * millisecond) (always NextGeneration)
    else
        Sub.none
