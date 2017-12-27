module JobView exposing (..)

import JobList exposing (Job, decodeJob, status, Status(..), formatQueue)
import Http exposing (Error, get, send)
import Html exposing (Html, div, text, span, button, code, pre)
import Html.Attributes exposing (class, classList, title, style)


renderJobView : Job -> Html msg
renderJobView j =
    let
        retryButton =
            case (status j) of
                Inactive ->
                    button [] [ text "Retry" ]

                _ ->
                    span [] []

        deleteButton =
            case (status j) of
                Destroyed ->
                    div [] []

                _ ->
                    button [ class "danger" ] [ text "Delete" ]
    in
        div [ class "job-detail" ]
            [ div [ class "row" ]
                [ span [ class "job-detail__id two columns" ] [ text ("Job " ++ toString j.id) ]
                , span [ class "job-detail__title eight columns" ] [ text j.class ]
                , span [ class "two columns" ]
                    [ span
                        [ classList
                            [ ( "job-detail__status", True )
                            , ( "job-detail__status--queued", status j == Queued )
                            , ( "job-detail__status--scheduled", status j == Scheduled )
                            , ( "job-detail__status--inactive", status j == Inactive )
                            ]
                        ]
                        [ j |> status |> toString |> text ]
                    ]
                ]
            , div [ class "row" ]
                [ span [ class "one columns" ] []
                , span [ class "job-detail__key two columns" ] [ text "Run at" ]
                , span [ class "jb-detail__value three columns", title "(run_at goes here)" ] [ text "(run_at goes here)" ]
                , span [ class "job-detail__key two columns" ] [ text "Queue" ]
                , span [ class "job-detail__value three columns" ]
                    [ text
                        (formatQueue
                            j.queue
                        )
                    ]
                ]
            , div [ class "row" ]
                [ span [ class "one columns" ] []
                , span [ class "job-detail__key two columns" ] [ text "Retryable" ]
                , span [ class "job-detail__value three columns" ] [ text (toString j.retryable) ]
                , span [ class "job-detail__key two columns" ] [ text "Priority" ]
                , span [ class "job-detail__value three columns" ] [ text (toString j.priority) ]
                ]
            , div [ class "row" ]
                [ span [ class "one columns" ] []
                , span [ class "job-detail__key two columns" ] [ text "Failures" ]
                , span [ class "job-detail__value three columns" ] [ text (toString j.errorCount) ]
                , span [ class "job-detail__key two columns" ] [ text "Last failed at" ]
                , span [ class "job-detail__value three columns" ] [ text "(failed_at goes here)" ]
                ]
            , div [ class "row" ]
                [ span [ class "one columns" ] []
                , span [ class "job-detail__key two columns" ] [ text "Arguments" ]
                , span [ class "job-detail__value eight columns" ]
                    [ code [] [ text (toString j.args) ]
                    ]
                ]
            , renderLastError j
            , div [ class "row" ]
                [ span [ class "twelve columns", style [ ( "textAlign", "center" ) ] ]
                    [ retryButton
                    , deleteButton
                    ]
                ]
            ]


renderLastError : Job -> Html msg
renderLastError job =
    div [ class "row" ]
        [ span [ class "one columns" ] []
        , span [ class "job-detail__key two columns" ] [ text "Last error" ]
        , span [ class "job-detail__value eight columns" ]
            [ pre [] [ text "(last_error goes here)" ]
            ]
        ]


getJob : Int -> (Result Error Job -> msg) -> Cmd msg
getJob id cb =
    let
        url =
            "http://localhost:8080/jobs/" ++ (toString id)

        request =
            Http.get url decodeJob
    in
        Http.send cb request
