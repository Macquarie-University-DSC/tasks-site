module DateTime exposing (..)

import Parser exposing ((|.), (|=), Parser, andThen, chompWhile, getChompedString, problem, run, succeed, symbol)



---- MODEL ----


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



---- UPDATE ----


updateDate : String -> Maybe DateTime -> Maybe DateTime
updateDate dateString dateModel =
    case dateModel of
        Nothing ->
            dateStringTo dateString (DateTime (Date 0 0 0) 0 0)

        Just date ->
            dateStringTo dateString date


toDate : Parser Date
toDate =
    succeed Date
        |= paddedIntParser
        |. symbol "-"
        |= paddedIntParser
        |. symbol "-"
        |= paddedIntParser


paddedIntParser : Parser Int
paddedIntParser =
    getChompedString (chompWhile Char.isDigit)
        |> andThen checkPaddedInt


checkPaddedInt : String -> Parser Int
checkPaddedInt token =
    case String.toInt token of
        Just val ->
            succeed val

        Nothing ->
            problem "Invalid integer"


dateStringTo : String -> DateTime -> Maybe DateTime
dateStringTo dateString dateTime =
    let
        dateTimeResult =
            run toDate dateString
    in
    case dateTimeResult of
        Ok date ->
            Just { dateTime | date = date }

        Err _ ->
            Nothing


updateHour : String -> Maybe DateTime -> Maybe DateTime
updateHour hourString dateModel =
    Maybe.andThen (toHour hourString) dateModel


toHour : String -> DateTime -> Maybe DateTime
toHour hourString dateTime =
    Maybe.map (validateHour dateTime) (String.toInt hourString)


validateHour : DateTime -> Int -> DateTime
validateHour dateTime hour =
    if hour > 23 then
        { dateTime | hour = 23 }

    else if hour < 0 then
        { dateTime | hour = 0 }

    else
        { dateTime | hour = hour }


updateMinute : String -> Maybe DateTime -> Maybe DateTime
updateMinute minuteString dateModel =
    Maybe.andThen (toMinute minuteString) dateModel


toMinute : String -> DateTime -> Maybe DateTime
toMinute minuteString dateTime =
    Maybe.map (validateMinute dateTime) (String.toInt minuteString)


validateMinute : DateTime -> Int -> DateTime
validateMinute dateTime minute =
    if minute > 60 then
        { dateTime | minute = 59 }

    else if minute < 0 then
        { dateTime | minute = 0 }

    else
        { dateTime | minute = minute }
