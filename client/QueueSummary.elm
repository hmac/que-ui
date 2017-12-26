module QueueSummary exposing (..)

import Json.Decode exposing (int, string, bool, list, value, Value, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Http exposing (Error, get, send)
import Html exposing (Html, div, text, span, tr, td, th, table, thead, tbody, section, code, header, h1, nav, a)
import Html.Attributes exposing (class, classList)
import String exposing (join)


type alias Summary =
    { class : String
    , priority : Int
    , count : Int
    , working : Int
    }


decodeSummary : Decoder Summary
decodeSummary =
    decode Summary
        |> required "job_class" string
        |> required "priority" int
        |> required "count" int
        |> required "count_working" int


getSummaries : (Result Error (List Summary) -> msg) -> Cmd msg
getSummaries cb =
    let
        url =
            "http://localhost:8080/queue-summary"

        request =
            Http.get url (list decodeSummary)
    in
        Http.send cb request


renderQueueSummary : List Summary -> Html msg
renderQueueSummary summaries =
    case summaries of
        [] ->
            div [ class "cleanscreen-message" ] [ text "No jobs in the queue!" ]

        ss ->
            div [ class "grid" ] (List.map renderSummary ss)


renderSummary : Summary -> Html msg
renderSummary s =
    div [ class "grid-row row" ]
        [ a [ class "grid-row-link twelve columns" ]
            [ span [ class "seven columns grid-cell-text" ] [ text s.class ]
            , span [ class "two columns" ]
                [ span [ class "grid-cell-value" ] [ text (toString s.count) ]
                , span [ class "grid-cell-label" ] [ text "count" ]
                ]
            , span [ class "two columns" ]
                [ span [ class "grid-cell-value" ] [ text (toString s.priority) ]
                , span [ class "grid-cell-label" ] [ text "priority" ]
                ]
            , span [ class "one columns" ]
                [ if (s.working > 0) then
                    (div [ class "spinner" ] [ text "Working" ])
                  else
                    (div [] [])
                ]
            ]
        ]
