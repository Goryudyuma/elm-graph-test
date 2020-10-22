port module Main exposing (..)

import Browser
import Graph
import Graph.DOT
import Html exposing (Attribute, Html, button, div, h1, iframe, img, input, option, select, text)
import Html.Attributes exposing (sandbox, src, srcdoc, style, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Html.Events.Extra exposing (onChange)
import IntDict exposing (IntDict)
import Json.Decode as Json
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
    , insertEdgeCandidate : InsertEdgeIDType
    , insertNodeCandidate : InsertNodeType
    }


type alias GraphType =
    Graph.Graph String EdgeType


type alias NodeType =
    String


type alias EdgeType =
    String


type alias InsertEdgeIDType =
    { a : Maybe Int
    , b : Maybe Int
    , label : String
    }


type alias InsertNodeType =
    { label : String
    }


initialModel : Model
initialModel =
    { svg = ""
    , nowGraph = Graph.empty
    , lastInsertNodeID = 0
    , lastInsertEdgeID = 0
    , insertEdgeCandidate = { a = Nothing, b = Nothing, label = "" }
    , insertNodeCandidate = { label = "" }
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateDot String
    | UpdateSVG String
    | UpdateGraph
    | UpdateInsertEdgeA String
    | UpdateInsertEdgeB String
    | UpdateInsertEdgeLabel String
    | UpdateInsertNodeLabel String
    | InsertNode String
    | InsertEdge Int Int String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDot s ->
            ( model, dot s )

        UpdateSVG svg ->
            ( { model | svg = svg }, Cmd.none )

        UpdateGraph ->
            ( model
            , Task.perform
                (\_ ->
                    UpdateDot <|
                        Graph.DOT.output Just Just model.nowGraph
                )
                (Task.succeed ())
            )

        UpdateInsertEdgeA a ->
            let
                oldInsertEdgeCandidate =
                    model.insertEdgeCandidate

                newInsertEdgeCandidate =
                    { oldInsertEdgeCandidate | a = String.toInt a }
            in
            ( { model | insertEdgeCandidate = newInsertEdgeCandidate }, Cmd.none )

        UpdateInsertEdgeB b ->
            let
                oldInsertEdgeCandidate =
                    model.insertEdgeCandidate

                newInsertEdgeCandidate =
                    { oldInsertEdgeCandidate | b = String.toInt b }
            in
            ( { model | insertEdgeCandidate = newInsertEdgeCandidate }, Cmd.none )

        UpdateInsertEdgeLabel label ->
            let
                oldInsertEdgeCandidate =
                    model.insertEdgeCandidate

                newInsertEdgeCandidate =
                    { oldInsertEdgeCandidate | label = label }
            in
            ( { model | insertEdgeCandidate = newInsertEdgeCandidate }, Cmd.none )

        UpdateInsertNodeLabel label ->
            let
                oldInsertNodeCandidate =
                    model.insertNodeCandidate

                newInsertNodeCandidate =
                    { oldInsertNodeCandidate | label = label }
            in
            ( { model | insertNodeCandidate = newInsertNodeCandidate }, Cmd.none )

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

        InsertEdge a b label ->
            let
                newGraph =
                    Graph.update a
                        (\contextMaybe ->
                            case contextMaybe of
                                Just context ->
                                    Just { context | outgoing = IntDict.insert b label context.outgoing }

                                Nothing ->
                                    Nothing
                        )
                        model.nowGraph
            in
            ( { model | nowGraph = newGraph }
            , Task.perform (\_ -> UpdateGraph) (Task.succeed ())
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        []
        [ viewAddNode model.nowGraph model.insertNodeCandidate
        , viewAddEdge model.nowGraph model.insertEdgeCandidate
        , iframe
            [ style "width" "100%"
            , style "height" "100vh"
            , srcdoc model.svg
            ]
            []
        ]


viewAddNode : GraphType -> InsertNodeType -> Html Msg
viewAddNode graph insertNodeCandidate =
    div []
        [ input [ onInput UpdateInsertNodeLabel ] []
        , if Graph.nodes graph |> List.all (\one -> one.label /= insertNodeCandidate.label) then
            button [ onClick <| InsertNode insertNodeCandidate.label ] [ text "add node" ]

          else
            div [] []
        ]


viewAddEdge : GraphType -> InsertEdgeIDType -> Html Msg
viewAddEdge graph insertEdgeID =
    div []
        [ select [ onChange UpdateInsertEdgeA ]
            (Graph.nodes graph
                |> List.map (\l -> option [ value <| String.fromInt l.id ] [ text l.label ])
                |> List.append (List.singleton (option [ value "" ] [ text "" ]))
            )
        , text "->"
        , select [ onChange UpdateInsertEdgeB ]
            (Graph.nodes graph
                |> List.map (\l -> option [ value <| String.fromInt l.id ] [ text l.label ])
                |> List.append (List.singleton (option [ value "" ] [ text "" ]))
            )
        , input [ onInput UpdateInsertEdgeLabel ] []
        , case ( insertEdgeID.a, insertEdgeID.b ) of
            ( Just a, Just b ) ->
                button [ onClick <| InsertEdge a b insertEdgeID.label ] [ text "add edge" ]

            _ ->
                div [] []
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
