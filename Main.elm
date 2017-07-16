module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Set


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Player
    = X
    | O


playerToCode : Player -> Int
playerToCode player =
    case player of
        X ->
            0

        O ->
            1


playerFromCode : Int -> Player
playerFromCode player =
    case player of
        0 ->
            X

        1 ->
            O

        _ ->
            Debug.crash "invalid player code"


other : Player -> Player
other player =
    case player of
        X ->
            O

        O ->
            X


type alias Model =
    { squares : Array (Array (Maybe Player))
    , turn : Player
    }


init : ( Model, Cmd Msg )
init =
    { squares =
        Array.initialize 3
            (\_ -> Array.repeat 3 Nothing)
    , turn = X
    }
        ! []


type Msg
    = Move Int Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


matrixSet : Int -> Int -> a -> Array (Array (Maybe a)) -> Maybe (Array (Array (Maybe a)))
matrixSet x y v a =
    let
        oldRow =
            Array.get x a

        newRow =
            case oldRow of
                Nothing ->
                    Nothing

                Just row ->
                    case Array.get y row of
                        Just Nothing ->
                            Just (Array.set y (Just v) row)

                        _ ->
                            Nothing
    in
        case newRow of
            Nothing ->
                Nothing

            Just row ->
                Just (Array.set x row a)


makeMove : Int -> Int -> Model -> Model
makeMove x y model =
    let
        newSquares =
            matrixSet x y model.turn model.squares
    in
        case newSquares of
            Nothing ->
                model

            Just ns ->
                { model
                    | squares = ns
                    , turn = other model.turn
                }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move x y ->
            case winner model.squares of
                Nothing ->
                    makeMove x y model ! []

                Just _ ->
                    model ! []


allSame : List (Maybe Player) -> Maybe Player
allSame elts =
    if List.all ((==) (Just X)) elts then
        Just X
    else if List.all ((==) (Just O)) elts then
        Just O
    else
        Nothing


winner : Array (Array (Maybe Player)) -> Maybe Player
winner squares =
    let
        n =
            Array.length squares

        indexes =
            List.range 0 (n - 1)

        rows : List (List (Maybe Player))
        rows =
            indexes
                |> List.filterMap
                    (\i ->
                        squares
                            |> Array.get i
                            |> Maybe.map Array.toList
                    )

        columns : List (List (Maybe Player))
        columns =
            indexes
                |> List.map
                    (\j ->
                        indexes
                            |> List.filterMap
                                (\i ->
                                    squares
                                        |> Array.get i
                                        |> Maybe.andThen (Array.get j)
                                )
                    )

        leftDiag : List (Maybe Player)
        leftDiag =
            indexes
                |> List.filterMap
                    (\i ->
                        squares
                            |> Array.get i
                            |> Maybe.andThen (Array.get i)
                    )

        rightDiag : List (Maybe Player)
        rightDiag =
            indexes
                |> List.filterMap
                    (\i ->
                        squares
                            |> Array.get i
                            |> Maybe.andThen (Array.get (n - i - 1))
                    )

        winners =
            List.concat
                [ List.filterMap allSame rows
                , List.filterMap allSame columns
                , List.filterMap allSame [ leftDiag, rightDiag ]
                ]
    in
        case
            winners
                |> List.map playerToCode
                |> Set.fromList
                |> Set.toList
                |> List.map playerFromCode
        of
            [] ->
                Nothing

            [ w ] ->
                Just w

            _ ->
                Debug.crash "multiple winners"


viewSquare : Int -> Int -> Maybe Player -> Html Msg
viewSquare x y player =
    let
        text =
            case player of
                Nothing ->
                    " "

                Just X ->
                    "X"

                Just O ->
                    "O"
    in
        Html.td
            [ Attr.height 30
            , Attr.width 30
            , Attr.style
                [ ( "border", "1px solid black" )
                , ( "text-align", "center" )
                , ( "vertical-align", "center" )
                ]
            , Ev.onClick (Move x y)
            ]
            [ Html.text text ]


view : Model -> Html Msg
view model =
    let
        viewRow : Int -> Array (Maybe Player) -> Html Msg
        viewRow i squares =
            Html.tr [] <|
                Array.toList <|
                    Array.indexedMap (viewSquare i) squares

        rows =
            Array.toList <| Array.indexedMap viewRow <| model.squares

        toPlay =
            case winner model.squares of
                Nothing ->
                    toString model.turn ++ " to play"

                Just w ->
                    toString w ++ " wins!"
    in
        Html.div
            [ Attr.style
                [ ( "margin", "30px" )
                ]
            ]
            [ Html.text toPlay
            , Html.table
                [ Attr.style
                    [ ( "border-collapse", "collapse" )
                    ]
                ]
                rows
            ]
