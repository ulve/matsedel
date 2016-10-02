module Main exposing (..)

import Html exposing (..)
import Html.App
import Html exposing (div, text)
import Http
import Json.Decode exposing (Decoder, list, string, object2, (:=), at)
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Typography as Typography
import Task
import Material.Card as Card
import Date


-- TYPES


type alias Meal =
    { date : String
    , courses : List String
    }


type alias Model =
    { meals : List Meal
    , mdl : Material.Model
    }


type alias Mdl =
    Material.Model


type Msg
    = More
    | FetchSucceed (List (List Meal))
    | FetchFail Http.Error
    | Mdl (Material.Msg Msg)



--INIT


init : ( Model, Cmd Msg )
init =
    ( { meals = [ { date = "idag", courses = [ "mat", "kött" ] } ], mdl = Material.model }, getMeals )


subscriptions : a -> Sub b
subscriptions model =
    Sub.none



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
        Mdl msg' ->
            Material.update msg' model

        More ->
            ( model, getMeals )

        FetchFail _ ->
            Debug.log "Error" ( model, Cmd.none )

        FetchSucceed newList ->
            let
                l =
                    Maybe.withDefault [ { date = "", courses = [] } ] (List.head newList)

                p =
                    { mdl = model.mdl, meals = l }
            in
                ( p, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.BlueGrey Color.Cyan <|
        Layout.render Mdl
            model.mdl
            []
            { header = []
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewCard model ]
            }


viewCard : Model -> Html Msg
viewCard model =
    Card.view
        [ Elevation.e2
        , Elevation.transition 250
        , Options.css "width" "480px"
        ]
        [ Card.title
            [ Options.css "background" "url('http://rustlersonline.com/wp-content/uploads/HotDog.png') center / cover"
            , Options.css "height" "256px"
            , Options.css "padding" "0"
            ]
            [ Card.head
                [ Color.text Color.white
                , Options.scrim 0.75
                , Options.css "padding" "16px"
                , Options.css "width" "100%"
                ]
                [ text "Matsedel" ]
            ]
        , Card.text [ Card.expand ]
            [ Options.div [] <| List.map viewDay model.meals
            ]
        ]


viewDay : Meal -> Html b
viewDay meal =
    Options.div []
        [ Options.styled p [ Typography.center, Typography.subhead ] [ text (toWeekDay meal.date) ]
        , Options.styled p [ Typography.center ] <| (List.map (\f -> div [] [ text f ]) meal.courses)
        ]


toWeekDay : String -> String
toWeekDay unparsed =
    case Date.fromString unparsed of
        Result.Ok val ->
            case Date.dayOfWeek val of
                Date.Mon ->
                    "Måndag"

                Date.Tue ->
                    "Tisdag"

                Date.Wed ->
                    "Onsdag"

                Date.Thu ->
                    "Torsdag"

                Date.Fri ->
                    "Fredag"

                Date.Sat ->
                    "Lödag"

                Date.Sun ->
                    "Söndag"

        Result.Err val ->
            "Inget bra datum"
