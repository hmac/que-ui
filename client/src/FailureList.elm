module FailureList exposing (..)

import Json.Decode exposing (int, string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Http exposing (get, send)
import Html exposing (Html, div, text, span, a, button)
import Html.Attributes exposing (class, classList, id, disabled)


type alias Failure =
    { class : String
    , inactive : Int
    , pendingRetry : Int
    }


decodeFailure : Decoder Failure
decodeFailure =
    decode Failure
        |> required "job_class" string
        |> required "inactive" int
        |> required "pending_retry" int


getFailures : (Result Http.Error (List Failure) -> msg) -> Cmd msg
getFailures cb =
    let
        url =
            "http://localhost:8080/failures"

        request =
            Http.get url (list decodeFailure)
    in
        Http.send cb request


renderFailureList : List Failure -> Html msg
renderFailureList failures =
    case failures of
        [] ->
            div [ class "cleanscreen-message" ] [ text "No failed jobs!" ]

        fs ->
            div [ class "grid" ] (List.map renderFailure fs)


renderFailure : Failure -> Html msg
renderFailure f =
    div [ class "grid-row row" ]
        [ a [ class "grid-row-link seven columns" ]
            [ span [ class "grid-cell-text six columns" ] [ text f.class ]
            , span [ class "three columns" ]
                [ span [ class "grid-cell-value" ]
                    [ text
                        (toString
                            f.pendingRetry
                        )
                    ]
                , span [ class "grid-cell-label" ] [ text "pending retry" ]
                ]
            , span [ class "three columns" ]
                [ span [ class "grid-cell-value" ] [ text (toString f.inactive) ]
                , span [ class "grid-cell-label" ] [ text "failed" ]
                ]
            ]
        , span [ class "five columns" ]
            [ button [ disabled (f.inactive == 0) ]
                [ text ("Retry " ++ (toString f.inactive) ++ " jobs")
                ]
            , button [ class "danger", disabled (f.inactive == 0) ]
                [ text ("Delete " ++ (toString f.inactive) ++ " jobs")
                ]
            ]
        ]
