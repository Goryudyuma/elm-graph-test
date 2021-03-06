port module Main exposing (..)

import Browser
import Graph
import Graph.DOT
import Html exposing (Attribute, Html, a, button, div, h1, iframe, img, input, option, select, text)
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
    , insertEdgeCandidate : InsertEdgeType
    , insertNodeCandidate : InsertNodeType
    }


type alias GraphType =
    Graph.Graph NodeLabelType EdgeType


type alias NodeLabelType =
    String


type alias EdgeType =
    String


type alias InsertEdgeType =
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
    | RemoveNode Int
    | RemoveEdge Int Int


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

        RemoveNode nodeID ->
            let
                oldInsertEdgeCandidate =
                    model.insertEdgeCandidate

                newA =
                    oldInsertEdgeCandidate.a
                        |> Maybe.andThen
                            (\a ->
                                if a == nodeID then
                                    Nothing

                                else
                                    Just a
                            )

                newB =
                    oldInsertEdgeCandidate.b
                        |> Maybe.andThen
                            (\b ->
                                if b == nodeID then
                                    Nothing

                                else
                                    Just b
                            )

                newInsertEdgeCandidate =
                    { oldInsertEdgeCandidate | a = newA, b = newB }
            in
            ( { model | nowGraph = Graph.remove nodeID model.nowGraph, insertEdgeCandidate = newInsertEdgeCandidate }
            , Task.perform (\_ -> UpdateGraph) (Task.succeed ())
            )

        RemoveEdge a b ->
            let
                newGraph =
                    Graph.update a
                        (\contextMaybe ->
                            case contextMaybe of
                                Just context ->
                                    Just { context | outgoing = IntDict.remove b context.outgoing }

                                Nothing ->
                                    Nothing
                        )
                        model.nowGraph
            in
            ( { model | nowGraph = newGraph }
            , Task.perform (\_ -> UpdateGraph) (Task.succeed ())
            )



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
        , let
            target =
                Graph.nodes graph
                    |> List.filter (\one -> one.label == insertNodeCandidate.label)
                    |> List.head
          in
          case target of
            Just node ->
                button [ onClick <| RemoveNode node.id ] [ text "delete node" ]

            Nothing ->
                button [ onClick <| InsertNode insertNodeCandidate.label ] [ text "add node" ]
        ]


type AddDeleteOptionType
    = AddOption Int Int
    | DeleteOption Int Int
    | OtherOption


viewAddEdge : GraphType -> InsertEdgeType -> Html Msg
viewAddEdge graph insertEdge =
    let
        addOrDelete : AddDeleteOptionType
        addOrDelete =
            case ( insertEdge.a, insertEdge.b ) of
                ( Just a, Just b ) ->
                    case Graph.get a graph of
                        Just node ->
                            if node.outgoing |> IntDict.member b then
                                DeleteOption a b

                            else
                                AddOption a b

                        Nothing ->
                            OtherOption

                _ ->
                    OtherOption
    in
    div []
        [ viewAddEdgeSelectNode (Graph.nodes graph) UpdateInsertEdgeA
        , text "->"
        , viewAddEdgeSelectNode (Graph.nodes graph) UpdateInsertEdgeB
        , case addOrDelete of
            AddOption _ _ ->
                input [ onInput UpdateInsertEdgeLabel ] []

            _ ->
                a [] []
        , case addOrDelete of
            AddOption a b ->
                button [ onClick <| InsertEdge a b insertEdge.label ] [ text "add edge" ]

            DeleteOption a b ->
                button [ onClick <| RemoveEdge a b ] [ text "delete edge" ]

            OtherOption ->
                a [] []
        ]


viewAddEdgeSelectNode : List (Graph.Node NodeLabelType) -> (String -> Msg) -> Html Msg
viewAddEdgeSelectNode nodes msg =
    select [ onChange msg ]
        (nodes
            |> List.map (\l -> option [ value <| String.fromInt l.id ] [ text l.label ])
            |> List.append (List.singleton (option [ value "" ] [ text "" ]))
        )



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
