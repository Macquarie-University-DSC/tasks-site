module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, input, label, text, textarea)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onInput)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode
import Task
import Tasks exposing (TaskType)
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
    | Success (List Tasks.Model)


type alias NewTaskType =
    { name : String
    , description : String
    , due_date : String
    , due_hour : String
    , due_minute : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        getTasksMsg =
            Cmd.map HttpMsgs getAllTasks
    in
    ( Model Loading Time.utc (NewTaskType "" "" "" "" "")
    , Cmd.batch [ setTimezone, getTasksMsg ]
    )


setTimezone : Cmd Msg
setTimezone =
    Task.perform AdjustTimeZone Time.here



-- UPDATE --


type Msg
    = AdjustTimeZone Time.Zone
    | HttpMsgs HttpMsg
    | TaskMsgs Tasks.Msg
    | NewTaskMsgs NewTaskMsg


type HttpMsg
    = Received (Result Http.Error (List TaskType))
    | Waiting


type NewTaskMsg
    = UpdateName String
    | UpdateDescription String
    | UpdateDueDate String
    | UpdateDueHour String
    | UpdateDueMinute String


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
                        taskModel =
                            List.map (Tasks.Model True) tasks
                    in
                    ( { model | status = Success taskModel }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )


updateTask : Tasks.Msg -> Model -> ( Model, Cmd Tasks.Msg )
updateTask msg model =
    case model.status of
        Success tasks ->
            let
                ( updated_tasks, cmd_msg ) =
                    Tasks.update msg tasks
            in
            ( { model | status = Success updated_tasks }
            , cmd_msg
            )

        _ ->
            ( model, Cmd.none )


updateNewTask : NewTaskMsg -> NewTaskType -> ( NewTaskType, Cmd Msg )
updateNewTask msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateDescription description ->
            ( { model | description = description }, Cmd.none )

        UpdateDueDate due_date ->
            ( { model | due_date = due_date }, Cmd.none )

        UpdateDueHour due_hour ->
            ( { model | due_hour = due_hour }, Cmd.none )

        UpdateDueMinute due_minute ->
            ( { model | due_minute = due_minute }, Cmd.none )



-- VIEW --


toView : (msg -> Msg) -> Html msg -> Html Msg
toView toMsg viewer =
    Html.map toMsg viewer


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
        , input [ type_ "date", value new_task.due_date, onInput updateDueDate ] []
        , button [] [ text "Submit" ]
        ]


updateName : String -> Msg
updateName name =
    NewTaskMsgs (UpdateName name)


updateDescription : String -> Msg
updateDescription description =
    NewTaskMsgs (UpdateDescription description)


updateDueDate : String -> Msg
updateDueDate due_date =
    NewTaskMsgs (UpdateDueDate due_date)


viewTasks : Model -> Html Msg
viewTasks model =
    case model.status of
        Failure ->
            h3 [] [ text "Error, could not connect to api" ]

        Loading ->
            h3 [] [ text "Loading..." ]

        Success tasks ->
            toView TaskMsgs (Tasks.view model.zone tasks)



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
        , subscriptions = \_ -> Sub.none
        }
