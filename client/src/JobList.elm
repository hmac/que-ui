module JobList exposing (..)

import Html exposing (Html, div, text, span, tr, td, th, table, thead, tbody, section, code, header, h1, nav, a)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Http exposing (Error, get, send)
import Json.Decode exposing (int, string, bool, list, value, Value, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import String exposing (join)
import Navigation


type alias Job =
    { priority : Int
    , id : Int
    , class : String
    , args : Value
    , failed : Bool
    , errorCount : Int
    , queue : String
    , retryable : Bool
    , scheduledForFuture : Bool
    }


decodeJob : Decoder Job
decodeJob =
    decode Job
        |> required "priority" int
        |> required "job_id" int
        |> required "job_class" string
        |> required "args" value
        |> required "failed" bool
        |> required "error_count" int
        |> required "queue" string
        |> required "retryable" bool
        |> required "scheduled_for_future" bool


type Status
    = Scheduled
    | Queued
    | Inactive
    | Destroyed


status : Job -> Status
status { retryable, scheduledForFuture } =
    case ( retryable, scheduledForFuture ) of
        ( True, True ) ->
            Scheduled

        ( True, False ) ->
            Queued

        _ ->
            Inactive


renderJobList : List Job -> (String -> msg) -> Html msg
renderJobList jobs changePage =
    case jobs of
        [] ->
            div [ class "cleanscreen-message" ] [ text "No workers currently active" ]

        js ->
            let
                row class_ label =
                    th [ class (join " " [ "table-heading", class_ ]) ] [ text label ]
            in
                table [ class "table" ]
                    [ thead []
                        [ tr [ class "table-row" ]
                            [ row "job-list-id" "Job ID"
                            , row "job-list-class" "Job Class"
                            , row "job-list-args" "Arguments"
                            , row "job-list-queue" "Queue"
                            , row "job-list-priority" "Priority"
                            , row "job-list-errors" "Errors"
                            ]
                        ]
                    , tbody [] (List.map (renderJob changePage) js)
                    ]


renderJob : (String -> msg) -> Job -> Html msg
renderJob changePage j =
    tr
        [ class "table-row table-row--link"
        , onClick (changePage ("#/jobs/" ++ toString j.id))
        ]
        [ td
            [ classList
                [ ( "table-cell", True )
                , ( "u-center", True )
                , ( "job-list-id--queued", status j == Queued )
                , ( "job-list-id--scheduled", status j == Scheduled )
                , ( "job-list-id--inactive", status j == Inactive )
                ]
            ]
            [ text (toString j.id) ]
        , td [ class "table-cell" ] [ text j.class ]
        , td [ class "table-cell" ] [ code [] [ text (toString j.args) ] ]
        , td [ class "table-cell" ] [ text (formatQueue j.queue) ]
        , td [ class "table-cell u-center" ] [ text (toString j.priority) ]
        , td [ class "table-cell u-center" ] [ text (toString j.errorCount) ]
        ]


formatQueue : String -> String
formatQueue q =
    case q of
        "" ->
            "(default)"

        q ->
            q


getJobs : (Result Error (List Job) -> msg) -> Cmd msg
getJobs cb =
    let
        url =
            "http://localhost:8080/jobs"

        request =
            Http.get url (list decodeJob)
    in
        Http.send cb request
