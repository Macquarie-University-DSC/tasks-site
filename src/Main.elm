module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h4, text, input)
import Html.Attributes exposing (class, type_, checked)
import Html.Events exposing (onClick)
import Task
import Time


-- MODEL --


type alias Model =
  { tasks : List TaskType
  , zone  : Time.Zone
  }

type alias TaskType =
  { name        : String
  , description : String
  , due_date    : Maybe Time.Posix
  , is_complete : Bool
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model testTasks Time.utc
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE --



type Msg = AdjustTimeZone Time.Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW --


testTasks : List TaskType
testTasks =
  [ TaskType "Test Task 1" "Test Task 1 Description" Nothing False
  , TaskType "Test Task 2" "Test Task 2 Description" Nothing True
  , TaskType "Test Task 3" "Test Task 3 Description" (Just (Time.millisToPosix 1625097600000)) False
  , TaskType "Test Task 4" "Test Task 4 Description" (Just (Time.millisToPosix 1625097600000)) True
  ]

view : Model -> Html Msg
view model =
  div [ class "flex-main" ]
    (List.append 
      ([ h1 [] [ text "TRACK YOUR TASKS!" ]
      ]) (viewTasks model))

viewTasks : Model -> List (Html Msg)
viewTasks model =
  List.map (viewTask model) testTasks

viewTask : Model -> TaskType -> Html Msg
viewTask _ task =
  div [ class "flex-row" ]
    [ input [ type_ "checkbox", checked task.is_complete ] []
    , h4 [] [ text task.name ]
    ]



-- MAIN --


main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


