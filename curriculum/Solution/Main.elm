module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import DateFormat
import Html.Styled as HtmlStyled exposing (Html, a, button, div, img, input, pre, span, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, placeholder, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Random
import Task
import Time
import Url
import Url.Parser as Parser



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- MODEL


type alias Model =
    { counter : Int
    , name : String
    , password : String
    , passwordAgain : String
    , result : ApiResponse
    , dieFace : Int
    , zone : Time.Zone
    , time : Time.Posix
    , jsonResult : JsonResponse
    , key : Nav.Key
    , url : Url.Url
    , route : Route
    }


type alias JsonText =
    { title : String
    , url : String
    }


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type ApiResponse
    = Ready
    | Loading
    | Success String
    | Failure


type JsonResponse
    = Ready1
    | Loading1
    | Success1 JsonText
    | Failure1



-- Route


type Route
    = IndexRoute
    | AboutRoute
    | LoginRoute
    | DashboardRoute


routeParser : Parser.Parser (Route -> c) c
routeParser =
    Parser.oneOf
        [ Parser.map IndexRoute Parser.top
        , Parser.map AboutRoute (Parser.s "about")
        , Parser.map LoginRoute (Parser.s "login")
        , Parser.map DashboardRoute (Parser.s "dashboard")
        ]



-- init


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { counter = 0
      , name = ""
      , password = ""
      , passwordAgain = ""
      , result = Ready
      , dieFace = 1
      , zone = Time.utc
      , time = Time.millisToPosix 0
      , jsonResult = Ready1
      , key = key
      , url = url
      , route = Maybe.withDefault IndexRoute <| Parser.parse routeParser url
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | Name String
    | Password String
    | PasswordAgain String
    | GotText (Result Http.Error String)
    | GetTextClick
    | Roll
    | NewFace Int
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | MorePlease
    | GotGif (Result Http.Error JsonText)
    | OnUrlChange Url.Url
    | OnUrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        Reset ->
            ( { model | counter = 0 }, Cmd.none )

        Name name ->
            ( { model | name = name }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        PasswordAgain password ->
            ( { model | passwordAgain = password }, Cmd.none )

        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | result = Success fullText }, Cmd.none )

                Err _ ->
                    ( { model | result = Failure }, Cmd.none )

        GetTextClick ->
            ( { model | result = Loading }
            , Http.get
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                , expect = Http.expectString GotText
                }
            )

        Roll ->
            ( model, Random.generate NewFace (Random.int 1 6) )

        NewFace faceResult ->
            ( { model | dieFace = faceResult }, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        MorePlease ->
            ( { model | jsonResult = Loading1 }, getRandomCatGif )

        GotGif result ->
            case result of
                Ok jsonText ->
                    ( { model | jsonResult = Success1 jsonText }, Cmd.none )

                Err _ ->
                    ( { model | result = Failure }, Cmd.none )

        OnUrlChange url ->
            ( { model | url = url, route = Maybe.withDefault IndexRoute <| Parser.parse routeParser url }, Cmd.none )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/photos/1"
        , expect = Http.expectJson GotGif gifDecoder
        }


gifDecoder : JD.Decoder JsonText
gifDecoder =
    JD.succeed JsonText
        |> JDP.required "title" JD.string
        |> JDP.required "url" JD.string


display : Model -> String
display model =
    case model.result of
        Ready ->
            "Ready"

        Loading ->
            "Loading"

        Success value ->
            value

        Failure ->
            "Fail to load"



-- VIEW


title : Model -> String
title _ =
    "Learn Elm 2.0"


view : Model -> Browser.Document Msg
view model =
    { title = title model
    , body = [ body model |> toUnstyled ]
    }


body : Model -> Html Msg
body model =
    let
        content =
            case model.route of
                IndexRoute ->
                    div []
                        [ span [ css [ displayFlex, justifyContent center ] ]
                            [ text <| formattedDateTime model.zone model.time ]
                        , div
                            [ css [ displayFlex, justifyContent center ] ]
                            [ button [ onClick Increment, css style.btn ] [ text "plus" ]
                            , button [ onClick Decrement, css <| style.btn ++ [ backgroundColor (hex "f89c9c") ] ] [ text "minus" ]
                            , button [ onClick Reset, css style.resetBtn ] [ text "Reset to zero" ]
                            ]
                        , span [ css [ displayFlex, justifyContent center, marginBottom (em 1) ] ] [ text (String.fromInt model.counter) ]
                        , viewInput "text" "Name" model.name Name
                        , viewInput "password" "Password" model.password Password
                        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
                        , viewValidation model
                        , div [] [ button [ onClick Roll, css style.btn ] [ text "Roll Dice" ] ]
                        , div [] [ text (String.fromInt model.dieFace) ]
                        , viewGif model
                        , button [ onClick GetTextClick, css style.btn ] [ text "Get Text" ]
                        , HtmlStyled.pre []
                            [ text <| display model
                            ]
                        ]

                LoginRoute ->
                    text "Login"

                DashboardRoute ->
                    text "Dashboard"

                AboutRoute ->
                    text "About"
    in
    div [ css style.content ]
        [ span [ css [ displayFlex, flexDirection row, marginBottom (em 1) ] ]
            [ a [ href "/", css [ marginRight (em 1) ] ] [ text "/index" ]
            , a [ href "/login", css [ marginRight (em 1) ] ] [ text "/login" ]
            , a [ href "/dashboard", css [ marginRight (em 1) ] ] [ text "/dashboard" ]
            , a [ href "/about", css [ marginRight (em 1) ] ] [ text "/about" ]
            ]
        , content
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model.jsonResult of
        Failure1 ->
            div []
                [ text "I could not load a random cat for some reason. "
                , button [ onClick MorePlease, css style.btn ] [ text "Try Again!" ]
                ]

        Loading1 ->
            text "Loading..."

        Success1 json ->
            div [ css [ displayFlex, flexDirection column, alignItems center ] ]
                [ button [ onClick MorePlease, css style.resetBtn ] [ text "Reload!" ]
                , span [] [ text json.title ]
                , img [ src json.url ] []
                ]

        Ready1 ->
            div []
                [ button [ onClick MorePlease, css style.btn ] [ text "Display Image!" ]
                ]


formattedDateTime : Time.Zone -> Time.Posix -> String
formattedDateTime =
    DateFormat.format
        [ DateFormat.dayOfMonthSuffix
        , DateFormat.text " "
        , DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.yearNumber
        , DateFormat.text " "
        , DateFormat.hourFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        , DateFormat.text " "
        , DateFormat.amPmUppercase
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if String.length model.name < 3 then
        div [ css style.wrong ] [ text "Name needs to be at least 3 characters" ]

    else if model.password == model.passwordAgain then
        div [ css style.correct ] [ text "OK" ]

    else
        div [ css style.wrong ] [ text "Passwords do not match!" ]


style =
    { btn =
        [ backgroundColor (hex "e4fbff")
        , border3 (px 5) groove (rgb 120 120 120)
        , margin (px 10)
        , fontSize (px 25)
        ]
    , resetBtn =
        [ backgroundColor (hex "f4c983")
        , border3 (px 5) solid (rgb 120 120 120)
        , fontSize (px 25)
        , margin (px 10)
        , minWidth fitContent
        , height (px 40)
        ]
    , content =
        [ fontSize (px 20)
        ]
    , correct =
        [ backgroundColor (hex "75cfb8")
        , margin (px 30)
        , padding (px 10)
        ]
    , wrong =
        [ backgroundColor (hex "e97878")
        , margin (px 30)
        , padding (px 10)
        ]
    }
