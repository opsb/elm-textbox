module TextBox exposing (init, update, view, Model, Msg, OutMsg(..))

import Html exposing (Html, Attribute, div, input, text, button, ul, li, h1, h2, h3, h4, p, img, textarea, em, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, on, targetValue, onWithOptions)
import Json.Decode as JD exposing ((:=))


-- MODEL


type alias Model =
    { value : String
    , height : Int
    , rows : Int
    , scrollHeight : Maybe Int
    }


init : Model
init =
    { value = ""
    , height = 30
    , rows = 1
    , scrollHeight = Nothing
    }



-- UPDATE


type Msg
    = OnInput InputEvent
    | OnEnter
    | OnNoOp


type OutMsg
    = Change String
    | NoOp


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        OnInput { value, scrollHeight, clientHeight } ->
            let
                rows' =
                    case model.scrollHeight of
                        Nothing ->
                            model.rows

                        Just modelScrollHeight ->
                            if scrollHeight > modelScrollHeight then
                                model.rows + 1
                            else if model.rows > 1 && scrollHeight < modelScrollHeight then
                                model.rows - 1
                            else
                                model.rows
            in
                ( { model | value = value, rows = rows', scrollHeight = Just scrollHeight }, NoOp )

        OnEnter ->
            ( { model | value = "" }, Change model.value )

        OnNoOp ->
            ( model, NoOp )



-- VIEW


view : Model -> Html Msg
view model =
    textarea
        [ value model.value
        , onInput' OnInput
        , onEnter OnEnter
        , preventDefaultOnEnter
        , rows model.rows
        ]
        []


isEnter code =
    code == 13


type alias InputEvent =
    { value : String
    , scrollHeight : Int
    , clientHeight : Int
    }


inputDecoder =
    let
        decoder =
            JD.object3 InputEvent
                ("value" := JD.string)
                ("scrollHeight" := JD.int)
                ("clientHeight" := JD.int)
    in
        JD.at [ "target" ] decoder


onInput' : (InputEvent -> msg) -> Attribute msg
onInput' tagger =
    on "input" (JD.map tagger inputDecoder)


keyExtractor =
    JD.object2 (,)
        ("keyCode" := JD.int)
        ("shiftKey" := JD.bool)


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        filterKey ( code, shift ) =
            if (isEnter code) && (not shift) then
                Ok "triggering"
            else
                Err "not triggering"

        decoder =
            JD.customDecoder keyExtractor filterKey
                |> JD.map (always msg)
    in
        on "keydown" decoder


preventDefaultOnEnter : Attribute Msg
preventDefaultOnEnter =
    let
        eventOptions =
            { stopPropagation = False, preventDefault = True }

        filterKey ( code, shift ) =
            if (isEnter code) && (not shift) then
                Ok "preventing default"
            else
                Err "allowing default"

        decoder =
            JD.customDecoder keyExtractor filterKey
                |> JD.map (always OnNoOp)
    in
        onWithOptions "keypress" eventOptions decoder
