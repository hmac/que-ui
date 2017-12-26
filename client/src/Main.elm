module Main exposing (..)

import Html exposing (Html, div, text, section, header, h1, nav, a)
import Html.Attributes exposing (class, id, href)
import Http
import JobList exposing (Job, renderJobList, getJobs)
import WorkerList exposing (Worker, renderWorkerList, getWorkers)
import QueueSummary exposing (Summary, getSummaries, renderQueueSummary)
import FailureList exposing (Failure, getFailures, renderFailureList)
import Time
import Navigation


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions { history } =
    Time.every Time.second (\_ -> Refresh)


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    ( { current = loc, history = [], items = QueueSummary [] }, navigate loc )


type alias Model =
    { current : Navigation.Location, history : List Navigation.Location, items : ItemList }


type ItemList
    = JobList (List Job)
    | WorkerList (List Worker)
    | QueueSummary (List Summary)
    | FailureList (List Failure)


type Msg
    = NoOp
    | GotJobs (Result Http.Error (List Job))
    | GotWorkers (Result Http.Error (List Worker))
    | GotSummaries (Result Http.Error (List Summary))
    | GotFailures (Result Http.Error (List Failure))
    | Refresh
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotJobs (Ok jobs) ->
            ( { model | items = JobList jobs }, Cmd.none )

        GotJobs _ ->
            ( model, Cmd.none )

        GotWorkers (Ok workers) ->
            ( { model | items = WorkerList workers }, Cmd.none )

        GotWorkers (Err e) ->
            ( model, Cmd.none )

        GotSummaries (Ok summaries) ->
            ( { model | items = QueueSummary summaries }, Cmd.none )

        GotSummaries (Err e) ->
            ( model, Cmd.none )

        GotFailures (Ok failures) ->
            ( { model | items = FailureList failures }, Cmd.none )

        GotFailures (Err e) ->
            ( model, Cmd.none )

        Refresh ->
            ( model, navigate model.current )

        UrlChange loc ->
            ( { model | current = loc, history = model.current :: model.history }, navigate loc )


navigate : Navigation.Location -> Cmd Msg
navigate { hash } =
    case hash of
        "#/queue-summary" ->
            getSummaries GotSummaries

        "#/jobs" ->
            getJobs GotJobs

        "#/failures" ->
            getFailures GotFailures

        "#/workers" ->
            getWorkers GotWorkers

        _ ->
            getSummaries GotSummaries


view : Model -> Html Msg
view m =
    let
        content =
            case m.items of
                JobList items ->
                    renderJobList items

                WorkerList items ->
                    renderWorkerList items

                QueueSummary items ->
                    renderQueueSummary items

                FailureList items ->
                    renderFailureList items
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
    let
        link url label =
            a [ class "app-header__nav__link", href url ] [ text label ]
    in
        nav [ class "u-pull-left app-header__nav" ]
            [ link "#/queue-summary" "Queue Summary"
            , link "#/workers" "Workers"
            , link "#/jobs" "Job List"
            , link "#/failures" "Failures"
            ]
