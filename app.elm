module Main exposing (..)

import Html exposing (Html,div)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (..)
import List exposing (foldr,intersperse,map)

main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }

type alias Model = {
        positions : List Mouse.Position
    }

type Msg =
    NoOp
        | MouseMsg Mouse.Position

init : (Model, Cmd Msg)
init = (Model [], Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseMsg pos ->
            ({model
                 | positions = model.positions ++ [pos]
             }
            , Cmd.none)
        NoOp ->
            (model
            , Cmd.none)

subs : Model -> Sub Msg
subs _ = Sub.batch [Mouse.clicks MouseMsg]

view : Model -> Html Msg
view model =
 svg
  [ width "500", height "500", viewBox "0 0 500 500" ]
    [ polygon [ fill "#F0AD00", points (pointsToSvgString model.positions) ] []
    ]

pointsToSvgString : List Mouse.Position -> String
pointsToSvgString positions =
    let
        stringMapper = (\n -> (toString n.x) ++ "," ++ (toString n.y))
        listOfStrings = List.map stringMapper positions
        interspersedStrings = List.intersperse " " listOfStrings
    in
        foldr (++) "" interspersedStrings
