module NewTask exposing (..)

import DateTime exposing (DateTime, updateDate, updateHour, updateMinute)
import Html exposing (Html, button, div, h3, input, label, text, textarea)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { name : String
    , description : String
    , due_date : Maybe DateTime
    }


type alias NewTask =
    { name : String
    , description : String
    , due_date : Int
    }



---- UPDATE ----


type Msg
    = UpdateName String
    | UpdateDescription String
    | UpdateDate String
    | UpdateHour String
    | UpdateMinute String
    | ValidateRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }
            , Cmd.none
            )

        UpdateDescription description ->
            ( { model | description = description }
            , Cmd.none
            )

        UpdateDate dateString ->
            ( { model | due_date = updateDate dateString model.due_date }
            , Cmd.none
            )

        UpdateHour hourString ->
            ( { model | due_date = updateHour hourString model.due_date }
            , Cmd.none
            )

        UpdateMinute minuteString ->
            ( { model | due_date = updateMinute minuteString model.due_date }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "ADD A NEW TASK" ]
        , label [] [ text "Task Name" ]
        , input
            [ type_ "text"
            , placeholder "New Task Name"
            , value model.name
            , onInput UpdateName
            ]
            []
        , label [] [ text "Task Description" ]
        , textarea
            [ placeholder "A description of a new task"
            , value model.description
            , onInput UpdateDescription
            ]
            []
        , label [] [ text "Due Date (optional)" ]
        , input
            [ type_ "date"
            , value (dueDateToString model.due_date)
            , onInput UpdateDate
            ]
            []
        , button [] [ text "Submit" ]
        ]


dueDateToString : Maybe DateTime -> String
dueDateToString maybeDateTime =
    case maybeDateTime of
        Nothing ->
            ""

        Just dateTime ->
            let
                year =
                    String.fromInt dateTime.date.year

                month =
                    String.fromInt dateTime.date.month

                day =
                    String.fromInt dateTime.date.day
            in
            year ++ "-" ++ month ++ "-" ++ day
