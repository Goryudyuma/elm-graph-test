port module Main exposing (..)

import Browser
import Graph
import Graph.DOT
import Html exposing (Html, div, h1, iframe, img, text)
import Html.Attributes exposing (sandbox, src, srcdoc, style)
import Html.Events exposing (onClick)
import IntDict exposing (IntDict)
import Markdown exposing (defaultOptions)
import Svg
import Svg.Attributes
import Task exposing (Task)



---- PORT ----


port dot : String -> Cmd msg


port updateSVG : (String -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { svg : String
    , nowGraph : GraphType
    , lastInsertNodeID : Int
    , lastInsertEdgeID : Int
    }


type alias GraphType =
    Graph.Graph String EdgeType


type alias NodeType =
    String


type alias EdgeType =
    String


initialModel : Model
initialModel =
    { svg = "", nowGraph = Graph.empty, lastInsertNodeID = 0, lastInsertEdgeID = 0 }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateDot String
    | UpdateSVG String
    | UpdateGraph
    | InsertNode String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDot s ->
            ( model, dot s )

        UpdateSVG svg ->
            ( { model | svg = svg }, Cmd.none )

        InsertNode label ->
            ( { model
                | nowGraph =
                    Graph.insert
                        { node = Graph.Node (model.lastInsertNodeID + 1) label
                        , incoming = IntDict.empty
                        , outgoing = IntDict.empty
                        }
                        model.nowGraph
                , lastInsertNodeID = model.lastInsertNodeID + 1
              }
            , Task.perform (\_ -> UpdateGraph) (Task.succeed ())
            )

        UpdateGraph ->
            ( model
            , Task.perform
                (\_ ->
                    UpdateDot <|
                        Graph.DOT.output Just Just model.nowGraph
                )
                (Task.succeed ())
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        []
        [ img
            [ src "/logo.svg"
            , onClick <| InsertNode "a"
            ]
            []
        , iframe
            [ style "width" "100%"
            , srcdoc model.svg
            ]
            []
        ]



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions model =
    updateSVG UpdateSVG



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
