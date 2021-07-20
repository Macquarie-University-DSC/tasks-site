port module NewTask exposing (..)

import DateTime exposing (DateTime, updateDate, updateHour, updateMinute)
import Html exposing (Html, button, div, h3, input, label, p, text, textarea)
import Html.Attributes as Attr exposing (for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode



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



---- PORTS ----


port sendDateTime : Encode.Value -> Cmd msg


port receivePosixTime : (Int -> msg) -> Sub msg



---- UPDATE ----


type Msg
    = UpdateName String
    | UpdateDescription String
    | UpdateDate String
    | UpdateHour String
    | UpdateMinute String
    | SubmitModel
    | ValidateRequest Int


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

        SubmitModel ->
            case model.due_date of
                Nothing ->
                    ( model, Cmd.none )

                Just dateTime ->
                    ( model, sendDateTime (encodeDateTime dateTime) )

        ValidateRequest _ ->
            ( model, Cmd.none )


encodeDateTime : DateTime -> Encode.Value
encodeDateTime dateTime =
    Encode.object
        [ ( "year", Encode.int dateTime.date.year )
        , ( "month", Encode.int dateTime.date.month )
        , ( "day", Encode.int dateTime.date.day )
        , ( "hour", Encode.int dateTime.hour )
        , ( "minute", Encode.int dateTime.minute )
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    receivePosixTime ValidateRequest



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "ADD A NEW TASK" ]
        , label [ for "taskName" ] [ text "Task Name" ]
        , input
            [ id "taskName"
            , type_ "text"
            , placeholder "New Task Name"
            , value model.name
            , onInput UpdateName
            ]
            []
        , label [ for "taskDescription" ] [ text "Task Description" ]
        , textarea
            [ id "taskDescription"
            , placeholder "A description of a new task"
            , value model.description
            , onInput UpdateDescription
            ]
            []
        , label [ for "taskDate" ] [ text "Due Date (optional)" ]
        , input
            [ id "taskDate"
            , type_ "date"
            , value (dueDateToString model.due_date)
            , onInput UpdateDate
            ]
            []
        , viewTimeInput model.due_date
        , button [ onClick SubmitModel ] [ text "Submit" ]
        ]


viewTimeInput : Maybe DateTime -> Html Msg
viewTimeInput maybeDateTime =
    case maybeDateTime of
        Nothing ->
            p [] [ text "enter date to add time information" ]

        Just dateTime ->
            let
                hour =
                    String.fromInt dateTime.hour

                minute =
                    String.fromInt dateTime.minute
            in
            div []
                [ label [ for "taskHour" ] [ text "Due Hour" ]
                , input
                    [ type_ "number"
                    , id "taskHour"
                    , value hour
                    , Attr.min "0"
                    , Attr.max "23"
                    , onInput UpdateHour
                    ]
                    []
                , label [ for "taskMinute" ] [ text "Due Minute" ]
                , input
                    [ type_ "number"
                    , id "taskMinute"
                    , value minute
                    , Attr.min "0"
                    , Attr.max "59"
                    , onInput UpdateMinute
                    ]
                    []
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
                        |> String.padLeft 4 '0'

                month =
                    String.fromInt dateTime.date.month
                        |> String.padLeft 2 '0'

                day =
                    String.fromInt dateTime.date.day
                        |> String.padLeft 2 '0'
            in
            year ++ "-" ++ month ++ "-" ++ day
