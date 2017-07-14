module Main exposing (main)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Array exposing (Array)


type Msg
    = NoOp
    | ToggleAt Int Int


type Cell
    = Alive
    | Empty


type alias Grid a =
    { items : Array a, width : Int }


type alias Model =
    { grid : Grid Cell }


makeGrid : Int -> Int -> a -> Grid a
makeGrid width height default =
    { items = Array.repeat (width * height) default
    , width = width
    }


flattenGrid : (Int -> Int -> a -> b) -> Grid a -> List b
flattenGrid f g =
    List.map (\( i, c ) -> f (i % g.width) (i // g.width) c) (Array.toIndexedList g.items)


mapAt : Grid a -> Int -> Int -> (a -> a) -> Grid a
mapAt g x y f =
    let
        i =
            (x + y * g.width)
    in
        case Array.get i g.items of
            Just cell ->
                { g | items = Array.set i (f cell) g.items }

            Nothing ->
                g


toggle : Cell -> Cell
toggle c =
    case c of
        Alive ->
            Empty

        Empty ->
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
    ( { grid = makeGrid 50 50 Empty }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleAt x y ->
            ( { model | grid = mapAt model.grid x y toggle }, Cmd.none )


view : Model -> Html Msg
view model =
    svg [ width "100%", height "100%", viewBox "0 0 500 500" ]
        (flattenGrid cellToSvg model.grid)


cellToSvg : Int -> Int -> Cell -> Svg Msg
cellToSvg x_ y_ c =
    let
        colour =
            case c of
                Alive ->
                    "black"

                Empty ->
                    "white"
    in
        rect [ x (toString (10 * x_)), y (toString (10 * y_)), width "10", height "10", fill colour, stroke "grey", onClick (ToggleAt x_ y_) ] []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
