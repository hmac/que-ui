module WorkerList exposing (..)

import Date exposing (Date)
import Json.Decode exposing (int, string, bool, list, value, Value, Decoder)
import Http exposing (Error, get, send)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode.Extra exposing (date)
import Html exposing (Html, div, text, span, tr, td, th, table, thead, tbody, section, code, header, h1, nav, a)
import Html.Attributes exposing (class, classList, id)


type alias Worker =
    { class : String
    , id : Int
    , queue : String
    , pid : Int
    , startedAt : Date
    , processingTime : String
    , jobId : Int
    }


decodeWorker : Decoder Worker
decodeWorker =
    decode Worker
        |> required "job_class" string
        |> required "job_id" int
        |> required "queue" string
        |> required "pid" int
        |> required "started_at" date
        |> required "processing_time" string
        |> required "job_id" int


renderWorkerList : List Worker -> Html msg
renderWorkerList workers =
    case workers of
        [] ->
            div [ class "cleanscreen-message" ] [ text "No workers currently active" ]

        ws ->
            div [ class "grid" ] (List.map renderWorker ws)


renderWorker : Worker -> Html msg
renderWorker w =
    let
        labelledCell : a -> String -> Html msg
        labelledCell val label =
            span [ class "two columns" ]
                [ span [ class "grid-cell-value" ] [ text (toString val) ]
                , span [ class "grid-cell-label" ] [ text label ]
                ]
    in
        div [ class "grid-row row" ]
            [ a [ class "grid-row-link twelve columns" ]
                [ span [ class "grid-cell-text four columns" ] [ text w.class ]
                , labelledCell w.pid "pid"
                , labelledCell (formatQueue w.queue) "queue"
                , labelledCell w.jobId "job id"
                , labelledCell w.processingTime "time"
                ]
            ]


formatQueue : String -> String
formatQueue q =
    case q of
        "" ->
            "(default)"

        q ->
            q


getWorkers : (Result Error (List Worker) -> msg) -> Cmd msg
getWorkers cb =
    let
        url =
            "http://localhost:8080/workers"

        request =
            Http.get url (list decodeWorker)
    in
        Http.send cb request
