module Tasks exposing (Model, Msg, TaskType, update, view)

import Html exposing (Html, button, div, h4, i, input, p, text)
import Html.Attributes exposing (checked, class, hidden, type_)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Time



-- MODEL --


type alias Model =
    { displayExtra : Bool
    , task : TaskType
    }


type alias TaskType =
    { id : Int
    , name : String
    , description : String
    , dueDate : Maybe Time.Posix
    , isComplete : Bool
    }



-- UPDATE --


type Msg
    = ToggleDisplayExtra Int Bool
    | ToggleComplete Int Bool


applyToTask : (TaskType -> TaskType) -> Model -> Model
applyToTask f model =
    { model | task = f model.task }


update : Msg -> List Model -> ( List Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDisplayExtra id displayExtra ->
            let
                toggleDisplayExtra m =
                    if m.task.id == id then
                        { m | displayExtra = displayExtra }

                    else
                        m
            in
            ( List.map toggleDisplayExtra model
            , Cmd.none
            )

        ToggleComplete id isComplete ->
            let
                toggleComplete t =
                    if t.id == id then
                        { t | isComplete = isComplete }

                    else
                        t
            in
            ( List.map (applyToTask toggleComplete) model
            , Cmd.none
            )



-- VIEW --


view : Time.Zone -> List Model -> Html Msg
view zone tasks =
    Keyed.node "ul" [] (List.map (viewKeyedTasks zone) tasks)


viewKeyedTasks : Time.Zone -> Model -> ( String, Html Msg )
viewKeyedTasks zone model =
    ( String.fromInt model.task.id
    , lazy3 viewTask zone model.task model.displayExtra
    )


viewTask : Time.Zone -> TaskType -> Bool -> Html Msg
viewTask zone task displayExtra =
    div [ class "task-content" ]
        [ div [ class "flex-row" ]
            [ input
                [ type_ "checkbox"
                , checked task.isComplete
                , onClick (ToggleComplete task.id (not task.isComplete))
                ]
                []
            , h4 [] [ text task.name ]
            , button
                [ class "button button-clear down-button"
                , onClick (ToggleDisplayExtra task.id (not displayExtra))
                ]
                [ i [ class "fas fa-level-down-alt" ] [] ]
            ]
        , div [ hidden displayExtra ]
            [ p [] [ text task.description ]
            , viewDueDate zone task.dueDate
            ]
        ]


viewDueDate : Time.Zone -> Maybe Time.Posix -> Html Msg
viewDueDate timezone maybeTime =
    case maybeTime of
        Just time ->
            let
                year =
                    String.fromInt (Time.toYear timezone time)

                month =
                    displayMonth (Time.toMonth timezone time)

                day =
                    String.fromInt (Time.toDay timezone time)

                hour =
                    String.fromInt (Time.toHour timezone time)

                minute =
                    String.fromInt (Time.toMinute timezone time)
            in
            p []
                [ text
                    ("Due Date: "
                        ++ day
                        ++ "/"
                        ++ month
                        ++ "/"
                        ++ year
                        ++ " "
                        ++ hour
                        ++ ":"
                        ++ minute
                    )
                ]

        Nothing ->
            p [] [ text "Due Date not set" ]


displayMonth : Time.Month -> String
displayMonth month =
    case month of
        Time.Jan ->
            "1"

        Time.Feb ->
            "2"

        Time.Mar ->
            "3"

        Time.Apr ->
            "4"

        Time.May ->
            "5"

        Time.Jun ->
            "6"

        Time.Jul ->
            "7"

        Time.Aug ->
            "8"

        Time.Sep ->
            "9"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"
