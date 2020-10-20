port module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, iframe, img, text)
import Html.Attributes exposing (sandbox, src, srcdoc, style)
import Html.Events exposing (onClick)
import Markdown exposing (defaultOptions)
import Svg
import Svg.Attributes



---- PORT ----


port dot : String -> Cmd msg


port updateSVG : (String -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { svg : String }


initialModel : Model
initialModel =
    { svg = "" }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateDot String
    | UpdateSVG String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDot s ->
            ( model, dot s )

        UpdateSVG svg ->
            ( { model | svg = svg }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        []
        [ img
            [ src "/logo.svg"
            , onClick <| UpdateDot "digraph {a -> b}"
            ]
            []
        , iframe
            [ srcdoc model.svg
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
