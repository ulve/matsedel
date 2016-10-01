module Main exposing (..)

import Html exposing (..)
import Html.App
import Html exposing (div, text)
import Html.Attributes exposing (class)
import Task
import Http
import Json.Decode exposing (Decoder, list, string, object2, (:=), at)
import Css

-- TYPES


type alias Meal =
    { date : String
    , courses : List String
    }


type alias Model =
    List Meal


type Msg
    = More
    | FetchSucceed (List (List Meal))
    | FetchFail Http.Error



--INIT


init : ( Model, Cmd Msg )
init =
    ( [ { date = "idag", courses = [ "mat", "kött" ] } ], getMeals )


subscriptions : a -> Sub b
subscriptions model =
    Sub.none

styles = 
    Css.asPairs >> Html.Attributes.style

-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- API


getMeals : Cmd Msg
getMeals =
    let
        url =
            "https://skolmaten.se/api/openmeal/v2/meals.json?distributorID=5323089923538944"
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeData url)



-- DECODERS


decodeData : Decoder (List (List Meal))
decodeData =
    at [ "data" ] (list decodeMeals)


decodeMeals : Decoder (List Meal)
decodeMeals =
    at [ "meals" ] (list decodeDays)


decodeDays : Decoder Meal
decodeDays =
    object2 Meal
        ("date" := string)
        ("courses" := decodeCourses)


decodeCourses : Decoder (List String)
decodeCourses =
    list ("name" := string)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        More ->
            ( model, getMeals )

        FetchFail _ ->
            Debug.log "ÅÅÅÅÅ" ( model, Cmd.none )

        FetchSucceed newList ->
            let
                l =
                    Maybe.withDefault [ { date = "idag", courses = [ "mat", "kött" ] } ] (List.head newList)
            in
                ( l, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ styles [] ] <| List.map viewDay model


viewDay : Meal -> Html Msg
viewDay meal =
    Html.div [ class "dag" ]
        [ Html.div [ class "datum" ] [ text meal.date ]
        , Html.div [ class "mat" ] <| (List.map (\f -> div [] [ text f ]) meal.courses)
        ]
