module Main exposing (..)

import Browser.Navigation exposing (back)
import Html exposing (label)
import Maybe exposing (withDefault)
import Playground exposing (..)
import Tuple exposing (first, second)
import Vec exposing (..)
import View exposing (..)


type alias Vec3 =
    { r : Float
    , g : Float
    , b : Float
    }


type alias Geci =
    { position : Vec
    , angle : Number
    }


type alias Golyo =
    { size : Float
    }


type alias Model =
    { playerPos : Vec
    , gecis : List Geci
    , golyo : Golyo
    }


vecMove : Vec -> Shape -> Shape
vecMove { x, y } =
    move x y


initModel : Model
initModel =
    { playerPos = Vec 0 0
    , gecis = []
    , golyo = { size = 2 }
    }


squareSegments : Number -> List Segment
squareSegments s =
    [ Segment (Vec -s s) (Vec s s)
    , Segment (Vec s s) (Vec s -s)
    , Segment (Vec s -s) (Vec -s -s)
    , Segment (Vec -s -s) (Vec -s s)
    ]



-- polygonSegments :  -> List Segment
-- polygonSegments p=


intGrid : Int -> Int -> List (List ( Int, Int ))
intGrid w h =
    List.range 0 (w - 1)
        |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))


m : List (List ( Int, Int ))
m =
    [ [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
    , [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
    , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
    ]


faszAngle : Time -> Number
faszAngle x =
    spin 10 x


viewGecis : List Geci -> Shape
viewGecis x =
    group
        (List.map (\{ position } -> circle white 5 |> vecMove position) x)


main =
    game view update initModel


view : Computer -> Model -> List Shape
view computer { playerPos, gecis, golyo } =
    [ image 1500 1500 "https://thumbs.dreamstime.com/b/transparent-grid-vector-background-transparent-grid-modern-illustration-transparent-grid-vector-background-transparent-grid-modern-129498878.jpg"
    , area
    , haz |> move 200 -50
    , ag 5 (wave 10 180 20 computer.time) |> moveDown 200 |> scale 0.85
    , torpe |> vecMove playerPos |> scale 0.5
    , felho |> move 100 190
    , viewGecis gecis
    , fasz golyo
        |> move computer.mouse.x computer.mouse.y
        |> rotate (faszAngle computer.time)
    , ruler

    -- , background 0
    ]


updateGeci : Geci -> Geci
updateGeci { position, angle } =
    { position =
        let
            newPosition =
                add position
                    (rotated (degrees angle) (Vec 0 -10))

            displacement =
                Segment newPosition position

            square =
                squareSegments 350

            intersections =
                List.filterMap (intersect displacement) square
        in
        intersections |> List.head |> withDefault newPosition
    , angle = angle
    }


update : Computer -> Model -> Model
update computer { playerPos, gecis, golyo } =
    { playerPos =
        add playerPos
            (Vec (toX computer.keyboard) (toY computer.keyboard))
    , gecis =
        let
            newGecis =
                List.map
                    updateGeci
                    gecis
        in
        if computer.mouse.down && golyo.size > 1 then
            { position = Vec computer.mouse.x computer.mouse.y
            , angle = faszAngle computer.time
            }
                :: newGecis

        else
            newGecis
    , golyo =
        let
            minimumSize =
                1

            maximumSize =
                2

            scalingRate =
                0.1
        in
        { size =
            case ( computer.mouse.down, golyo.size > minimumSize, golyo.size < maximumSize ) of
                ( True, True, _ ) ->
                    golyo.size - scalingRate

                ( False, _, True ) ->
                    golyo.size + (scalingRate / 8)

                _ ->
                    golyo.size
        }
    }
