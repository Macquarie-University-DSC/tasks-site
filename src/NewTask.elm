module NewTask exposing (..)

import DateTime exposing (DateTime)



---- MODEL ----


type alias Model =
    { name : String
    , description : String
    , due_date : Maybe DateTime
    }



---- UPDATE ----


type Msg
    = UpdateName String
    | UpdateDescription String
    | UpdateDate String
    | UpdateHour String
    | UpdateMinute String
    | ValidateRequest
