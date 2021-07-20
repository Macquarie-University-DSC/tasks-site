module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h3, text)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode
import NewTask
import Task
import Tasks exposing (TaskType)
import Time



-- MODEL --


type alias Model =
    { status : Status
    , zone : Time.Zone
    , new_task : NewTask.Model
    }


type Status
    = Failure
    | Loading
    | Success (List Tasks.Model)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        getTasksMsg =
            Cmd.map HttpMsgs getAllTasks
    in
    ( Model Loading Time.utc (NewTask.Model "" "" Nothing)
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
    | NewTaskMsgs NewTask.Msg


type HttpMsg
    = Received (Result Http.Error (List TaskType))
    | Waiting


mapUpdate : (msg -> Msg) -> ( m, Cmd msg ) -> ( m, Cmd Msg )
mapUpdate toMsg ( model, cmd_msg ) =
    ( model, Cmd.map toMsg cmd_msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        HttpMsgs httpMsgs ->
            mapUpdate HttpMsgs (updateHttp httpMsgs model)

        TaskMsgs taskMsgs ->
            mapUpdate TaskMsgs (updateTask taskMsgs model)

        NewTaskMsgs newTaskMsgs ->
            let
                ( newTask, cmdMsgs ) =
                    mapUpdate
                        NewTaskMsgs
                        (NewTask.update newTaskMsgs model.new_task)
            in
            ( { model | new_task = newTask }, cmdMsgs )


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



-- VIEW --


toView : (msg -> Msg) -> Html msg -> Html Msg
toView toMsg viewer =
    Html.map toMsg viewer


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ viewTitle
        , lazy viewTasks model
        , toView NewTaskMsgs (NewTask.view model.new_task)
        ]


viewTitle : Html Msg
viewTitle =
    h1 [] [ text "TRACK YOUR TASKS!" ]


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
