module Vec exposing (..)

import Expect exposing (Expectation, pass)
import Fuzz exposing (float)
import Html exposing (..)
import Playground exposing (Shape, circle)
import Random
import Test exposing (Test, describe, test)
import Test.Runner.Html exposing (..)
import Tuple exposing (first, second)


type alias Vec =
    { x : Float
    , y : Float
    }


type alias Segment =
    { p1 : Vec
    , p2 : Vec
    }


type alias Geci =
    { position : Vec
    , angle : Float
    }


toString : Vec -> String
toString { x, y } =
    "{ x = " ++ String.fromFloat x ++ ", y = " ++ String.fromFloat y ++ " }"


add : Vec -> Vec -> Vec
add a b =
    Vec (a.x + b.x) (a.y + b.y)


reverse : Vec -> Vec
reverse { x, y } =
    Vec -x -y


scaled : Float -> Vec -> Vec
scaled s { x, y } =
    Vec (s * x) (s * y)


rotated : Float -> Vec -> Vec
rotated r { x, y } =
    Vec (cos r * x - sin r * y)
        (sin r * x + cos r * y)


intersect : Segment -> Segment -> Maybe Vec
intersect s1 s2 =
    let
        divisor =
            ((s1.p1.x - s1.p2.x) * (s2.p1.y - s2.p2.y))
                - ((s1.p1.y - s1.p2.y) * (s2.p1.x - s2.p2.x))

        dividendT =
            ((s1.p1.x - s2.p1.x) * (s2.p1.y - s2.p2.y))
                - ((s1.p1.y - s2.p1.y) * (s2.p1.x - s2.p2.x))

        dividendU =
            -(((s1.p1.x - s1.p2.x) * (s1.p1.y - s2.p1.y))
                - ((s1.p1.y - s1.p2.y) * (s1.p1.x - s2.p1.x))
             )
    in
    if abs divisor < 0.000000001 then
        Nothing

    else
        let
            t =
                dividendT / divisor

            u =
                dividendU / divisor
        in
        if 0 <= t && t <= 1 && 0 <= u && u <= 1 then
            Just (Vec (s1.p1.x + t * (s1.p2.x - s1.p1.x)) (s1.p1.y + t * (s1.p2.y - s1.p1.y)))

        else
            Nothing


sgn : Float -> Float
sgn x =
    if x < 0 then
        -1

    else
        1


intersectCircle : Float -> Vec -> Segment -> Maybe Vec
intersectCircle r c s =
    let
        dx =
            s.p2.x - s.p1.x

        dy =
            s.p2.y - s.p1.y

        dr =
            sqrt (dx ^ 2 + dy ^ 2)

        d =
            s.p1.x * s.p2.y - s.p2.x * s.p1.y

        discriminant =
            r ^ 2 * dr ^ 2 - d ^ 2

        orientation =
            sgn -dy
    in
    if discriminant < 0 then
        Nothing

    else
        Just
            (Vec ((d * dy + orientation * sgn dy * dx * sqrt (r ^ 2 * dr ^ 2 - d ^ 2)) / dr ^ 2)
                (-d * dx + orientation * abs dy * sqrt (r ^ 2 * dr ^ 2 - d ^ 2) / dr ^ 2)
            )


approx : Float -> Float -> Expect.Expectation
approx =
    Expect.within (Expect.Absolute 0.00001)


approxVec : Vec -> Vec -> Expect.Expectation
approxVec v1 v2 =
    let
        d =
            0.0001
    in
    if v1.x >= v2.x - d && v1.x <= v2.x + d && v1.y >= v2.y - d && v1.y <= v2.y + d then
        Expect.pass

    else
        Expect.fail ("Expected: ~ " ++ toString v1 ++ ", Got: " ++ toString v2 ++ ".")


just : (a -> Expect.Expectation) -> Maybe a -> Expect.Expectation
just f m =
    case m of
        Just x ->
            f x

        Nothing ->
            Expect.fail "Expected Just, found Nothing."


suite : Test
suite =
    describe "Vec Tests"
        [ describe "Add Tests"
            [ test "1" <|
                \_ ->
                    add (Vec 3 4) (Vec 7 6)
                        |> approxVec (Vec 10 10)
            ]
        , describe "Reverse Tests"
            [ test "1" <|
                \_ ->
                    reverse (Vec 5 5)
                        |> approxVec (Vec -5 -5)
            ]
        , describe "Scaled Tests"
            [ test "1" <|
                \_ ->
                    scaled 5 (Vec 5 5)
                        |> approxVec (Vec 25 25)
            , test "2" <|
                \_ ->
                    scaled 3 (Vec 5 5)
                        |> approxVec (Vec 15 15)
            ]
        , describe "Rotated Tests"
            [ test "90 degrees" <|
                \_ ->
                    rotated (degrees 90) (Vec 5 5)
                        |> approxVec (Vec -5 5)
            , test "180 degrees" <|
                \_ ->
                    rotated (degrees 180) (Vec 5 5)
                        |> approxVec (Vec -5 -5)
            ]
        , describe "Intersect Tests"
            [ test "Example Pass" <|
                \_ ->
                    intersect (Segment (Vec 1 1) (Vec 3 3)) (Segment (Vec 3 1) (Vec 1 3))
                        |> just (approxVec (Vec 2 2))
            , test "Example Parallel" <|
                \_ ->
                    intersect (Segment (Vec 0 0) (Vec 0 3)) (Segment (Vec 1 0) (Vec 1 4))
                        |> Expect.equal Nothing
            , test "Same Segment" <|
                \_ ->
                    intersect (Segment (Vec 0 0) (Vec 0 3)) (Segment (Vec 0 0) (Vec 0 3))
                        |> Expect.equal Nothing
            , test "Shall not Pass" <|
                \_ ->
                    intersect (Segment (Vec 0 0) (Vec 0 3)) (Segment (Vec 1 0) (Vec 1 3))
                        |> Expect.equal Nothing
            ]
        , describe "Circle - Line Intersection Tests"
            [ test "Example Pass" <|
                \_ ->
                    intersectCircle 1 (Vec 0 0) (Segment (Vec -10 -10) (Vec 10 10))
                        |> just (approxVec (Vec (sqrt 2 / -2) (sqrt 2 / -2)))
            , test "Example Pass2" <|
                \_ ->
                    intersectCircle 1 (Vec 0 0) (Segment (Vec 10 10) (Vec -10 -10))
                        |> just (approxVec (Vec (sqrt 2 / 2) (sqrt 2 / 2)))
            , test "Example Pass3" <|
                \_ ->
                    intersectCircle 1 (Vec 0 0) (Segment (Vec 10 -10) (Vec -10 10))
                        |> just (approxVec (Vec (sqrt 2 / 2) (sqrt 2 / -2)))
            , test "Example Pass4" <|
                \_ ->
                    intersectCircle 1 (Vec 0 0) (Segment (Vec -10 10) (Vec 10 -10))
                        |> just (approxVec (Vec (sqrt 2 / -2) (sqrt 2 / 2)))
            , test "Example Fail" <|
                \_ ->
                    intersectCircle 1 (Vec 0 0) (Segment (Vec -10 10) (Vec -10 11))
                        |> Expect.equal Nothing
            ]
        ]


main : Html msg
main =
    viewResults (defaultConfig (Random.initialSeed 0)) suite
