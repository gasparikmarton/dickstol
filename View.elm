module View exposing (..)

import Playground exposing (..)
import Vec exposing (..)


type alias Golyo =
    { size : Float
    }


vecMove : Vec -> Shape -> Shape
vecMove { x, y } =
    move x y


bor : Color
bor =
    rgb 220 200 150


cockColor : Float -> Color
cockColor x =
    rgb 255 (x * 128) (x * 128)


background : Number -> Shape
background p =
    if p < 10 then
        group
            [ square lightGray 20
            , square darkGray 20 |> move 20 0
            , background (p - 1)
            ]

    else
        square white 20


ruler : Shape
ruler =
    let
        label f x =
            words red (String.fromInt x) |> f (toFloat x)
    in
    List.range -10 10
        |> List.map (\x -> x * 100)
        |> List.map (\x -> group [ label moveRight x, label moveUp x ])
        |> Playground.group


area : Shape
area =
    square black 700


hazteto : Shape
hazteto =
    polygon darkOrange [ ( -50, 50 ), ( 0, 200 ), ( 50, 50 ) ]


alap : Shape
alap =
    square darkYellow 100


ajto : Shape
ajto =
    rectangle lightPurple 30 50


ablak : Shape
ablak =
    octagon lightGray 20


haz : Shape
haz =
    group
        [ hazteto
        , alap
        , ajto |> move 20 -25
        , ablak |> move -20 20
        ]


kalap : Shape
kalap =
    polygon purple [ ( -40, 40 ), ( 0, 200 ), ( 40, 40 ) ]


fej : Shape
fej =
    group
        [ circle bor 10 |> move 40 30
        , circle bor 10 |> move -40 30
        , circle bor 50
        , circle black 10 |> move 20 20
        , circle black 10 |> move -20 20
        , circle red 20 |> moveY -20
        ]


ruha : Shape
ruha =
    group
        [ polygon yellow [ ( -50, -200 ), ( 0, 0 ), ( 50, -200 ) ]
        ]


labkez : Shape
labkez =
    group
        [ rectangle bor 10 100 |> move 30 -225 |> rotate 30
        , rectangle bor 10 100 |> move -30 -225 |> rotate -30
        , rectangle bor 10 100 |> move 30 -120 |> rotate -30
        , rectangle bor 10 100 |> move -30 -120 |> rotate 30
        ]


torpe : Shape
torpe =
    group
        [ labkez
        , ruha
        , fej
        , kalap
        ]


felho : Shape
felho =
    group
        [ circle lightBlue 40
        , circle lightBlue 42 |> move 40 10
        , circle lightBlue 38 |> move -40 10
        ]


fasz : Golyo -> Shape
fasz golyo =
    group
        [ circle (cockColor golyo.size) 10 |> move -10 35 |> scale golyo.size
        , circle (cockColor golyo.size) 10 |> move 10 35 |> scale golyo.size
        , oval lightCharcoal 20 (80 / golyo.size)
        ]


ag : Int -> Number -> Shape
ag n angle =
    if n > 0 then
        let
            scaling =
                0.65

            l =
                100

            displacement =
                Vec 0 l
                    |> rotated (degrees angle)
                    |> scaled scaling
                    |> add (Vec 0 l)

            subAg dir =
                ag (n - 1) angle
                    |> rotate (angle * dir)
                    |> vecMove (displacement |> (\{ x, y } -> Vec (dir * x) y))
                    |> scale scaling
        in
        group
            [ rectangle darkBrown 15 (2 * l)
            , subAg 1
            , subAg -1
            ]

    else
        circle green 20
