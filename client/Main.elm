module Main exposing (..)

import Html exposing (Html, div, text, section, header, h1, nav, a)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import JobList exposing (Job, renderJobList, getJobs)
import WorkerList exposing (Worker, renderWorkerList, getWorkers)
import QueueSummary exposing (Summary, getSummaries, renderQueueSummary)
import FailureList exposing (Failure, getFailures, renderFailureList)
import Time


main : Platform.Program Basics.Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        JobList _ ->
            Time.every Time.second (\_ -> Navigate JobListRoute)

        WorkerList _ ->
            Time.every Time.second (\_ -> Navigate WorkerListRoute)

        QueueSummary _ ->
            Time.every Time.second (\_ -> Navigate QueueSummaryRoute)

        FailureList _ ->
            Time.every Time.second (\_ -> Navigate FailureListRoute)


init : ( Model, Cmd Msg )
init =
    ( QueueSummary { summaries = [] }, getSummaries GotSummaries )


type Model
    = JobList { jobs : List Job }
    | WorkerList { workers : List Worker }
    | QueueSummary { summaries : List Summary }
    | FailureList { failures : List Failure }


type Msg
    = NoOp
    | GotJobs (Result Http.Error (List Job))
    | GotWorkers (Result Http.Error (List Worker))
    | GotSummaries (Result Http.Error (List Summary))
    | GotFailures (Result Http.Error (List Failure))
    | Navigate Route


type Route
    = JobListRoute
    | WorkerListRoute
    | QueueSummaryRoute
    | FailureListRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotJobs (Ok jobs) ->
            ( JobList { jobs = jobs }, Cmd.none )

        GotJobs _ ->
            ( model, Cmd.none )

        GotWorkers (Ok workers) ->
            ( WorkerList { workers = workers }, Cmd.none )

        GotWorkers (Err e) ->
            ( model, Cmd.none )

        GotSummaries (Ok summaries) ->
            ( QueueSummary { summaries = summaries }, Cmd.none )

        GotSummaries (Err e) ->
            ( model, Cmd.none )

        GotFailures (Ok failures) ->
            ( FailureList { failures = failures }, Cmd.none )

        GotFailures (Err e) ->
            ( model, Cmd.none )

        Navigate JobListRoute ->
            ( model, getJobs GotJobs )

        Navigate WorkerListRoute ->
            ( model, getWorkers GotWorkers )

        Navigate QueueSummaryRoute ->
            ( model, getSummaries GotSummaries )

        Navigate FailureListRoute ->
            ( model, getFailures GotFailures )


view : Model -> Html Msg
view m =
    let
        content =
            case m of
                JobList { jobs } ->
                    renderJobList jobs

                WorkerList { workers } ->
                    renderWorkerList workers

                QueueSummary { summaries } ->
                    renderQueueSummary summaries

                FailureList { failures } ->
                    renderFailureList failures
    in
        div [ id "container" ]
            [ section [ class "app" ]
                [ header [ class "app-header" ]
                    [ h1 [ class "u-pull-left u-cf app-header__title" ]
                        [ text "Que UI" ]
                    , navigation
                    ]
                , section
                    [ class "app-content" ]
                    [ content
                    ]
                ]
            ]


navigation : Html Msg
navigation =
    nav [ class "u-pull-left app-header__nav" ]
        [ a [ class "app-header__nav__link", onClick (Navigate QueueSummaryRoute) ] [ text "Queue Summary" ]
        , a [ class "app-header__nav__link", onClick (Navigate WorkerListRoute) ] [ text "Workers" ]
        , a [ class "app-header__nav__link", onClick (Navigate JobListRoute) ] [ text "Job List" ]
        , a [ class "app-header__nav__link", onClick (Navigate FailureListRoute) ] [ text "Failures" ]
        ]
