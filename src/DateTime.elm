module DateTime exposing (..)

import Parser exposing ((|.), (|=), Parser, int, succeed, symbol)
import Time


type alias DateTime =
    { date : Date
    , hour : Int
    , minute : Int
    }


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


toDate : Parser Date
toDate =
    succeed Date
        |= int
        |. symbol "-"
        |= int
        |. symbol "-"
        |= int
