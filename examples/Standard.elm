module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import TextBox exposing (..)


type alias Model =
    { textBox : TextBox.Model
    , submittedValue : String
    }


model =
    { textBox = TextBox.init
    , submittedValue = ""
    }


main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type Msg
    = TextBoxMsg TextBox.Msg


update : Msg -> Model -> Model
update msg model =
    case (Debug.log "main" msg) of
        TextBoxMsg msg ->
            let
                ( textBox', outMsg ) =
                    TextBox.update msg model.textBox

                submittedValue' =
                    case outMsg of
                        TextBox.NoOp ->
                            model.submittedValue

                        TextBox.Change text ->
                            text
            in
                { model | textBox = textBox', submittedValue = submittedValue' }


view : Model -> Html Msg
view model =
    div []
        [ App.map TextBoxMsg (TextBox.view model.textBox)
        , text model.submittedValue
        ]
