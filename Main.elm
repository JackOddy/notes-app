module Main exposing (..)

import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as App


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }



-- model


type alias Model =
    { notes : List Note, focusNoteId : Maybe Int, input : String }


type alias Note =
    { id : Int, body : String }


initModel : Model
initModel =
    Model [] Nothing ""



-- update


type Msg
    = AddNote
    | DeleteNote Int
    | ViewNote Note
    | Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        AddNote ->
            newNote model

        ViewNote note ->
            displayNote model note

        _ ->
            model


displayNote : Model -> Note -> Model
displayNote model note =
    { model | focusNoteId = Just note.id }


newNote : Model -> Model
newNote model =
    let
        note =
            Note (List.length model.notes) model.input
    in
        { model | notes = note :: model.notes, input = "" }



-- view


view : Model -> Html Msg
view model =
    div []
        [ notesHeader
        , listNotes model
        , notesForm model
        , focusNote model
        ]


notesHeader : Html Msg
notesHeader =
    header [] [ text "Notelm" ]


listNotes : Model -> Html Msg
listNotes model =
    model.notes
        |> List.map
            (\note ->
                li [ onClick (ViewNote note) ]
                    [ div []
                        [ text ((String.slice 0 20 note.body) ++ "...")
                        ]
                    ]
            )
        |> ul []


notesForm : Model -> Html Msg
notesForm model =
    Html.form [ onSubmit AddNote ]
        [ input [ type' "text", onInput Input, value model.input ]
            []
        , button [ type' "submit" ] [ text "Add Note" ]
        ]


focusNote : Model -> Html Msg
focusNote model =
    model.notes
        |> List.filter
            (\note ->
                Just note.id == model.focusNoteId
            )
        |> List.map
            (\noteToView ->
                div [] [ text noteToView.body ]
            )
        |> div []
