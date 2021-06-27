module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h4, text, input, i)
import Html.Attributes exposing (class, type_, checked)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Task
import Time
import List exposing ((::))

-- MODEL --


testTasks : List TaskType
testTasks =
  [ TaskType 1 "Test Task 1" "Test Task 1 Description" Nothing False False
  , TaskType 2 "Test Task 2" "Test Task 2 Description" Nothing True False
  , TaskType 3 "Test Task 3" "Test Task 3 Description" (Just (Time.millisToPosix 1625097600000)) False False
  , TaskType 4 "Test Task 4" "Test Task 4 Description" (Just (Time.millisToPosix 1625097600000)) True False
  ]

type alias Model =
  { tasks : List TaskType
  , zone  : Time.Zone
  }

type alias TaskType =
  { id            : Int
  , name          : String
  , description   : String
  , due_date      : Maybe Time.Posix
  , is_complete   : Bool
  , display_extra : Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model testTasks Time.utc
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE --



type Msg = AdjustTimeZone Time.Zone
         | ToggleComplete Int Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    ToggleComplete id is_complete ->
      let
        updateTask task =
          if task.id == id then
            { task | is_complete = is_complete }
          else
            task
        in 
          ( { model | tasks = List.map updateTask model.tasks }
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

viewKeyedTask : Time.Zone -> TaskType -> (String, Html Msg)
viewKeyedTask zone task =
  ( (String.fromInt task.id), lazy2 viewTask zone task)
viewTask : Time.Zone -> TaskType -> Html Msg
viewTask _ task =
  div [ class "task-content" ]
    [ div [ class "flex-row" ]
        [ input [ type_ "checkbox", checked task.is_complete, onClick (ToggleComplete task.id (not task.is_complete)) ] []
        , h4 [] [ text task.name ]
        , button [ class "button button-clear down-button"] [ i [ class "fas fa-level-down-alt"] [] ]
        ]
      , div [] []
    ]
  



-- MAIN --


main : Program () Model Msg
main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

