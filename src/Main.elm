module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h4, text, input, i)
import Html.Attributes exposing (class, type_, checked)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http
import Task
import Time

-- MODEL --

apiURL : String
apiURL = "localhost/api"

type alias Model =
  { tasks : Status 
  , zone  : Time.Zone
  }

type Status
  = Failure
  | Loading
  | Success (List TaskModel)

type alias TaskModel =
  { id            : Int
  , name          : String
  , description   : String
  , due_date      : Maybe Time.Posix
  , is_complete   : Bool
  , display_extra : Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Loading Time.utc
  , Cmd.batch [ setTimezone ]
  )

setTimezone : Cmd Msg
setTimezone = Task.perform AdjustTimeZone Time.here



-- UPDATE --


type Msg 
  = AdjustTimeZone Time.Zone
  | HttpMsgs HttpMsg
  | TaskMsgs TaskMsg

type HttpMsg
  = Received (Result Http.Error (List TaskModel))
  | Waiting

type TaskMsg = ToggleComplete Int Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    HttpMsgs http_msgs ->
      updateHttp http_msgs model
        
    TaskMsgs task_msg ->
      case model.tasks of
        Success tasks ->
          let
            (updated_tasks, cmd_msg) = updateTask task_msg tasks 
          in
            ( { model | tasks = Success updated_tasks }
            , cmd_msg
            )
        
        _ ->
          (model, Cmd.none)

updateHttp : HttpMsg -> Model -> (Model, Cmd Msg)
updateHttp msg model =
  case msg of
    Waiting ->
      ({model | tasks = Loading}, Cmd.none)

    Received result ->
      case result of
        Ok tasks ->
          ({model | tasks = Success tasks}, Cmd.none)
        
        Err _ ->
          ({model | tasks = Failure}, Cmd.none)

updateTask : TaskMsg -> List TaskModel -> (List TaskModel, Cmd Msg)
updateTask msg tasks =
  case msg of
    ToggleComplete id is_complete ->
      let
        updateTaskModel task =
          if task.id == id then
            { task | is_complete = is_complete }
          else
            task
        in 
          ( List.map updateTaskModel tasks
          , Cmd.none
          )



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
    ]

viewTitle : Html Msg
viewTitle =
  h1 [] [ text "TRACK YOUR TASKS!" ]

viewTasks : Model -> Html Msg
viewTasks model =
  Keyed.node "ul" [] (List.map (viewKeyedTask model.zone) model.tasks)

viewKeyedTask : Time.Zone -> TaskModel -> (String, Html Msg)
viewKeyedTask zone task =
  ( (String.fromInt task.id), lazy2 viewTask zone task)
viewTask : Time.Zone -> TaskModel -> Html Msg
viewTask _ task =
  div [ class "task-content" ]
    [ div [ class "flex-row" ]
        [ input [ type_ "checkbox", checked task.is_complete, onClick (ToggleComplete task.id (not task.is_complete)) ] []
        , h4 [] [ text task.name ]
        , button [ class "button button-clear down-button"] [ i [ class "fas fa-level-down-alt"] [] ]
        ]
      , div [] []
    ]
  


-- HTTP --


getAllTasks : Cmd Msg
getAllTasks =
  Http.get
    { url = apiURL ++ "/tasks"
    , expect = Http.expectJson Received taskDecoder
    }



-- MAIN --


main : Program () Model Msg
main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

