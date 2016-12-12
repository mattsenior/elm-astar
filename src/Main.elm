module Main exposing (..)

import Debug exposing (log)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import AStar
import Matrix
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- TYPES


type ClickAction
    = ToggleWall
    | SetStart
    | SetGoal


type alias Model =
    { board : Board
    , start : Maybe Matrix.Location
    , goal : Maybe Matrix.Location
    , path : Maybe (List Matrix.Location)
    , clickAction : ClickAction
    }


type Msg
    = NoOp
    | ClickTile Matrix.Location
    | SetClickAction ClickAction
    | UpdatePath



-- MODEL


type Tile
    = Wall
    | Corridor


type alias Board =
    Matrix.Matrix Tile


initBoard : Board
initBoard =
    let
        blankBoard =
            Matrix.square 20 (always Corridor)

        addWall board =
            List.range 2 17
                |> List.map (\n -> ( n, 10 ))
                |> List.foldr (\loc board -> Matrix.set loc Wall board) board
    in
        blankBoard |> addWall


init : ( Model, Cmd Msg )
init =
    update UpdatePath
        { board = initBoard
        , start = Just ( 2, 2 )
        , goal = Just ( 17, 17 )
        , path = Nothing
        , clickAction = ToggleWall
        }



-- UPDATE


validWall : Matrix.Location -> Model -> Bool
validWall loc model =
    if Just loc == model.start then
        False
    else if Just loc == model.goal then
        False
    else
        True


toggleWall : Matrix.Location -> Model -> Model
toggleWall loc model =
    let
        update tile =
            case tile of
                Wall ->
                    Corridor

                Corridor ->
                    Wall
    in
        if validWall loc model then
            { model | board = Matrix.update loc update model.board }
        else
            model


trySetStart : Matrix.Location -> Model -> Model
trySetStart loc model =
    case Matrix.get loc model.board of
        Just Wall ->
            model

        Just _ ->
            { model | start = Just loc }

        _ ->
            model


trySetGoal : Matrix.Location -> Model -> Model
trySetGoal loc model =
    case Matrix.get loc model.board of
        Just Wall ->
            model

        Just _ ->
            { model | goal = Just loc }

        _ ->
            model


neighbours : Matrix.Location -> Model -> Set Matrix.Location
neighbours loc model =
    let
        c =
            Matrix.col loc

        r =
            Matrix.row loc
    in
        [ ( r - 1, c )
        , ( r, c - 1 )
        , ( r, c + 1 )
        , ( r + 1, c )
        ]
            |> List.filterMap
                (\loc2 ->
                    Matrix.get loc2 model.board
                        |> Maybe.andThen
                            (\tile ->
                                case tile of
                                    Corridor ->
                                        Just loc2

                                    Wall ->
                                        Nothing
                            )
                )
            |> Set.fromList


distance : Matrix.Location -> Matrix.Location -> number
distance x y =
    -- On this simple grid each neighbour is only 1 away
    1


heuristicDistance : Matrix.Location -> Matrix.Location -> Float
heuristicDistance goal x =
    let
        dist x y =
            sqrt (toFloat x ^ 2 + toFloat y ^ 2)
    in
        dist (Matrix.row goal - Matrix.row x) (Matrix.col goal - Matrix.col x)


updatePath : Model -> Model
updatePath model =
    case ( model.start, model.goal ) of
        ( Just start, Just goal ) ->
            let
                maybePath =
                    AStar.aStar
                        (flip neighbours <| model)
                        distance
                        (heuristicDistance goal)
                        ((==) goal)
                        start
            in
                { model | path = maybePath }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "update/msg" msg of
        ClickTile loc ->
            let
                newModel =
                    case model.clickAction of
                        ToggleWall ->
                            toggleWall loc model

                        SetStart ->
                            trySetStart loc model

                        SetGoal ->
                            trySetGoal loc model
            in
                update UpdatePath newModel

        SetClickAction clickAction ->
            ( { model | clickAction = clickAction }, Cmd.none )

        UpdatePath ->
            ( updatePath model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


inPath : Matrix.Location -> Maybe (List Matrix.Location) -> Bool
inPath loc maybePath =
    maybePath
        |> Maybe.map (List.member loc)
        |> Maybe.withDefault False


viewTile : Model -> Int -> Int -> Tile -> Html Msg
viewTile model row col tile =
    Html.div
        [ HtmlA.classList
            [ ( "board__tile tile", True )
            , ( "tile--wall", tile == Wall )
            , ( "tile--corridor", tile == Corridor )
            , ( "tile--start", model.start == Just ( row, col ) )
            , ( "tile--goal", model.goal == Just ( row, col ) )
            , ( "tile--path", inPath ( row, col ) model.path )
            ]
        , HtmlE.onClick <| ClickTile ( row, col )
        ]
        [ Html.div [ HtmlA.class "tile__coords" ]
            [ Html.text <| toString row ++ "," ++ toString col ]
        ]


viewBoardRow : Model -> Int -> List Tile -> Html Msg
viewBoardRow model row tiles =
    tiles
        |> List.indexedMap (viewTile model row)
        |> Html.div [ HtmlA.class "board__row" ]


view : Model -> Html Msg
view model =
    Html.div []
        [ model.board
            |> Matrix.toList
            |> List.indexedMap (viewBoardRow model)
            |> Html.div [ HtmlA.class "board" ]
        , [ ToggleWall, SetStart, SetGoal ]
            |> List.map
                (\clickAction ->
                    Html.label
                        [ HtmlA.class "toggle"
                        ]
                        [ Html.input
                            [ HtmlA.class "toggle__input"
                            , HtmlA.type_ "radio"
                            , HtmlA.name "clickType"
                            , HtmlA.checked (model.clickAction == clickAction)
                            , HtmlE.onCheck
                                (\checked ->
                                    if checked then
                                        SetClickAction clickAction
                                    else
                                        NoOp
                                )
                            ]
                            []
                        , Html.text (toString clickAction)
                        ]
                )
            |> Html.div [ HtmlA.class "toggles" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
