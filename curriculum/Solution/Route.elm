module Exercise.Route exposing (Route(..), toRoute)

import String


type Route
    = Home
    | AboutUs
    | Topic String
    | User String
    | Comment String String
    | Unknown


{-| Complete the function that converts a URL string to Route
"/" == Home
"/index" == Home
"/about-us" == AboutUs
"/topic/general" == Topic "general"
"/topic/fun" == Topic "fun"
"/user/cindy-wee" == User "cindy-wee"
"/user/cindy-wee/comment/rxzudk231" == Comment "cindy-wee" "rxzudk231"
"/404" == Unknown
-}
toRoute : String -> Route
toRoute url =
    -- Complete the implementation here
    case String.split "/" url of
        "" :: "" :: _ ->
            Home

        "" :: "index" :: _ ->
            Home

        "" :: "about-us" :: _ ->
            AboutUs

        "" :: "topic" :: t :: _ ->
            Topic t

        [ "", "user", username ] ->
            User username

        [ "", "user", username, comment, text ] ->
            Comment username text

        _ ->
            Unknown
