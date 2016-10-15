module Main exposing (..)

import Html exposing (Html,div,fieldset,label,input,button)
import Html.App as App
import Html.Events exposing (onClick)
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


type Action =
    CircleCenter
        | CircleRadius
        | DrawPolygon

type alias CircleSpec = {
        center : Mouse.Position
      , radius : Int
    }

type alias Model = {
        positions : List Mouse.Position
      , currentAction : Action
      , currentCircleSpec : CircleSpec
      , circles : List CircleSpec
    }

type ObjectType =
    Polygon
        | Circle

type Msg =
    NoOp
        | MouseMsg Mouse.Position
        | SwitchTo ObjectType
        | Clear

init : (Model, Cmd Msg)
init = (Model [] DrawPolygon (CircleSpec {x=0,y=0} 0) [], Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseMsg pos ->
            case model.currentAction of
                CircleCenter ->
                    if (pos.x <= 500 && pos.y <= 500)
                    then
                      let
                          newCircleSpec = CircleSpec pos 0
                          m = { model | currentCircleSpec = newCircleSpec } 
                      in
                        ({ m
                             | currentAction = CircleRadius
                         }
                        , Cmd.none)
                    else
                      (model, Cmd.none)

                CircleRadius ->
                    let
                        c = model.currentCircleSpec
                        newCircleSpec = { c | radius = (distance pos c.center) }
                        m = { model | currentAction = CircleCenter }
                    in
                      ({ m
                         | circles = model.circles ++ [newCircleSpec]
                       }
                      , Cmd.none)

                DrawPolygon ->
                    ({ model
                         | positions = model.positions ++ [pos]
                     }
                    , Cmd.none)

        SwitchTo Circle ->
            ({ model
                 | currentAction = CircleCenter
             }
            , Cmd.none)

        SwitchTo Polygon ->
            ({ model
                 | currentAction = DrawPolygon
             }
            , Cmd.none)

        Clear ->
            let
                m = { model | positions = [] }
                m2 = { m | circles = [] }
            in
                ( m2
                , Cmd.none
                )

        NoOp ->
            (model
            , Cmd.none)

distance : Mouse.Position -> Mouse.Position -> Int
distance pos1 pos2 =
    let
        xd = abs ((toFloat pos1.x) - (toFloat pos2.x))
        yd = abs ((toFloat pos1.y) - (toFloat pos2.y))
    in
        truncate (sqrt (((*) xd xd) + ((*) yd yd)))


subs : Model -> Sub Msg
subs _ = Sub.batch [Mouse.clicks MouseMsg]

view : Model -> Html Msg
view model =
  let
    drawBlackPoint = drawPoint "#000000"
    drawOrangePolygon = drawPolygon "#F0AD00"
    middlePoint = case model.currentAction of
                      CircleRadius ->
                          let
                              point = model.currentCircleSpec.center
                              x' = (/) (toFloat point.x) 5
                              y' = (/) (toFloat point.y) 5
                          in
                              [drawPoint "#FF0000" (truncate x') (truncate y')]
                      _ -> []
  in
    div
      []
      [ svg
        [ width "500"
        , height "500"
        , viewBox "0 0 500 500"
        ]
        (
          [ drawOrangePolygon model.positions
          ]
         ++
          (map (\c -> drawCircle c) model.circles)
         ++
          middlePoint
        )
       , fieldset
          []
          [ radio "Polygon" (SwitchTo Polygon)
          , radio "Circle" (SwitchTo Circle)
          , button [onClick Clear] [text "Clear"]
          ]

      ]

drawCircle : CircleSpec -> Svg msg
drawCircle circle' =
    circle
        [ cx (toString circle'.center.x)
        , cy (toString circle'.center.y)
        , r (toString circle'.radius)
        ]
        [
        ]

radio : String -> msg -> Html msg
radio value msg =
  label
    [
    ]
    [ input [ type' "radio", name "font-size", onClick msg ] []
    , text value
    ]

drawPolygon : String -> List Mouse.Position -> Svg msg
drawPolygon color' positions =
  polygon
    [ fill color'
    , points (pointsToSvgString positions)
    ]
    []

drawPoint : String -> Int -> Int -> Svg msg
drawPoint color' x' y' =
  rect
    [fill color'
    , x (toString ((*) x' 5))
    , y (toString ((*) y' 5))
    , width "5", height "5"
    ]
    []

pointsToSvgString : List Mouse.Position -> String
pointsToSvgString positions =
    let
        stringMapper = (\n -> (toString n.x) ++ "," ++ (toString n.y))
        listOfStrings = List.map stringMapper positions
        interspersedStrings = List.intersperse " " listOfStrings
    in
        foldr (++) "" interspersedStrings
