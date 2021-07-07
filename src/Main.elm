module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, i, input, label, p, text, textarea)
import Html.Attributes exposing (checked, class, hidden, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy3)
import Http
import Json.Decode as Decode
import Task
import Time



-- MODEL --


type alias Model =
    { status : Status
    , zone : Time.Zone
    , new_task : NewTaskType
    }


type Status
    = Failure
    | Loading
    | Success (List TaskModel)


type alias TaskModel =
    { display_extra : Bool
    , task : TaskType
    }


type alias NewTaskType =
    { name : String
    , description : String
    , due_date : Maybe Time.Posix
    }


type alias TaskType =
    { id : Int
    , name : String
    , description : String
    , due_date : Maybe Time.Posix
    , is_complete : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        getTasksMsg =
            Cmd.map HttpMsgs getAllTasks
    in
    ( Model Loading Time.utc (NewTaskType "" "" Nothing)
    , Cmd.batch [ setTimezone, getTasksMsg ]
    )


setTimezone : Cmd Msg
setTimezone =
    Task.perform AdjustTimeZone Time.here



-- UPDATE --


type Msg
    = AdjustTimeZone Time.Zone
    | HttpMsgs HttpMsg
    | TaskMsgs TaskMsg
    | NewTaskMsgs NewTaskMsg


type HttpMsg
    = Received (Result Http.Error (List TaskType))
    | Waiting


type TaskMsg
    = ToggleDisplayExtra Int Bool
    | ToggleComplete Int Bool


type NewTaskMsg
    = UpdateName String
    | UpdateDescription String


mapUpdate : (msg -> Msg) -> ( Model, Cmd msg ) -> ( Model, Cmd Msg )
mapUpdate toMsg ( model, cmd_msg ) =
    ( model, Cmd.map toMsg cmd_msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        HttpMsgs http_msgs ->
            mapUpdate HttpMsgs (updateHttp http_msgs model)

        TaskMsgs task_msgs ->
            mapUpdate TaskMsgs (updateTask task_msgs model)

        NewTaskMsgs new_task_msgs ->
            let
                ( new_task, cmd_msgs ) =
                    updateNewTask new_task_msgs model.new_task
            in
            ( { model | new_task = new_task }, cmd_msgs )


updateHttp : HttpMsg -> Model -> ( Model, Cmd HttpMsg )
updateHttp msg model =
    case msg of
        Waiting ->
            ( { model | status = Loading }, getAllTasks )

        Received result ->
            case result of
                Ok tasks ->
                    let
                        tasks_model =
                            List.map (TaskModel True) tasks
                    in
                    ( { model | status = Success tasks_model }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )


updateTask : TaskMsg -> Model -> ( Model, Cmd TaskMsg )
updateTask msg model =
    case model.status of
        Success tasks ->
            let
                ( updated_tasks, cmd_msg ) =
                    updateTaskModels msg tasks
            in
            ( { model | status = Success updated_tasks }
            , cmd_msg
            )

        _ ->
            ( model, Cmd.none )


updateTaskModels : TaskMsg -> List TaskModel -> ( List TaskModel, Cmd TaskMsg )
updateTaskModels msg model =
    let
        applyToModel func task_model =
            { task_model | task = func task_model.task }
    in
    case msg of
        ToggleDisplayExtra id display_extra ->
            let
                toggleDisplayExtra task_model =
                    if task_model.task.id == id then
                        { task_model | display_extra = display_extra }

                    else
                        task_model
            in
            ( List.map toggleDisplayExtra model
            , Cmd.none
            )

        ToggleComplete id is_complete ->
            let
                toggleComplete task =
                    if task.id == id then
                        { task | is_complete = is_complete }

                    else
                        task
            in
            ( List.map (applyToModel toggleComplete) model
            , Cmd.none
            )


updateNewTask : NewTaskMsg -> NewTaskType -> ( NewTaskType, Cmd Msg )
updateNewTask msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateDescription description ->
            ( { model | description = description }, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ viewTitle
        , lazy viewTasks model
        , viewNewTaskForm model.new_task
        ]


viewTitle : Html Msg
viewTitle =
    h1 [] [ text "TRACK YOUR TASKS!" ]


viewNewTaskForm : NewTaskType -> Html Msg
viewNewTaskForm new_task =
    div []
        [ h3 [] [ text "ADD A NEW TASK" ]
        , input [ type_ "text", placeholder "Name", value new_task.name, onInput updateName ] []
        , textarea [ placeholder "Description", value new_task.description, onInput updateDescription ] []
        , label [] [ text "Due Date (optional)" ]
        , input [ type_ "date" ] []
        , button [] [ text "Submit" ]
        ]


updateName : String -> Msg
updateName name =
    NewTaskMsgs (UpdateName name)


updateDescription : String -> Msg
updateDescription description =
    NewTaskMsgs (UpdateDescription description)


viewTasks : Model -> Html Msg
viewTasks model =
    case model.status of
        Failure ->
            h3 [] [ text "Error, could not connect to api" ]

        Loading ->
            h3 [] [ text "Loading..." ]

        Success tasks ->
            Keyed.node "ul" [] (List.map (viewKeyedTask model.zone) tasks)


viewKeyedTask : Time.Zone -> TaskModel -> ( String, Html Msg )
viewKeyedTask zone model =
    ( String.fromInt model.task.id, lazy3 viewTask zone model.task model.display_extra )


viewTask : Time.Zone -> TaskType -> Bool -> Html Msg
viewTask zone task display_extra =
    div [ class "task-content" ]
        [ div [ class "flex-row" ]
            [ input [ type_ "checkbox", checked task.is_complete, onClick (TaskMsgs (ToggleComplete task.id (not task.is_complete))) ] []
            , h4 [] [ text task.name ]
            , button [ class "button button-clear down-button", onClick (TaskMsgs (ToggleDisplayExtra task.id (not display_extra))) ]
                [ i [ class "fas fa-level-down-alt" ] [] ]
            ]
        , div [ hidden display_extra ]
            [ p [] [ text task.description ]
            , viewDueDate zone task.due_date
            ]
        ]


viewDueDate : Time.Zone -> Maybe Time.Posix -> Html Msg
viewDueDate timezone maybe_time =
    case maybe_time of
        Just time ->
            let
                year =
                    String.fromInt (Time.toYear timezone time)

                month =
                    toMonthInt (Time.toMonth timezone time)

                day =
                    String.fromInt (Time.toDay timezone time)

                hour =
                    String.fromInt (Time.toHour timezone time)

                minute =
                    String.fromInt (Time.toMinute timezone time)
            in
            p [] [ text ("Due Date: " ++ day ++ "/" ++ month ++ "/" ++ year ++ " " ++ hour ++ ":" ++ minute) ]

        Nothing ->
            p [] [ text "Due Date not set" ]


toMonthInt : Time.Month -> String
toMonthInt month =
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



-- HTTP --


apiURL : String
apiURL =
    "https://api.howgood.me"


getAllTasks : Cmd HttpMsg
getAllTasks =
    Http.get
        { url = apiURL ++ "/tasks"
        , expect = Http.expectJson Received (Decode.list taskDecoder)
        }



-- JSON --


taskDecoder : Decode.Decoder TaskType
taskDecoder =
    Decode.map5 TaskType
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "due_date" (Decode.maybe timeDecoder))
        (Decode.field "is_complete" Decode.bool)


timeDecoder : Decode.Decoder Time.Posix
timeDecoder =
    Decode.map Time.millisToPosix Decode.int



-- MAIN --


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
