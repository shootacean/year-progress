module Main exposing (main, view)

import Bootstrap.CDN as CDN
import Bootstrap.Progress as Progress
import Browser
import Date exposing (Date, Interval(..), Unit(..))
import Html exposing (Html, header, main_, footer, div, text, h1, table, tbody, tr, th, td, p, a, br)
import Html.Attributes exposing (class, href)
import Html exposing (Attribute)
import Task exposing (Task)
import Time exposing (Month(..))
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)
import Debug exposing (toString)


type alias Model =
    Date


type Msg
    = ReceiveDate Date


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Date.fromCalendarDate 2021 Jan 1
    , Date.today |> Task.perform ReceiveDate
    )


-- update

update : Msg -> Model -> ( Model, Cmd Msg )
update (ReceiveDate today) _ =
    ( today
    , Cmd.none
    )


-- view

view : Model -> Browser.Document Msg
view date =
    let
        end = thisYearLastDate (Date.year date)
        diffDays = toFloat ( Date.diff Days date end )
        v = 100 - (ceiling (diffDays / 365 * 100))
    in
        Browser.Document
            "Year Progress"
            [ CDN.stylesheet
            , viewHeader
            , main_ [ class "column px-3 py-3" ]
                   [ viewProgressBar v
                   , viewDetail date diffDays
                   ]
            , viewFooter
            ]

viewHeader : Html msg
viewHeader =
    header [ class "px-4 py-5" ]
           [ h1 [ class "title" ] [ text "Year Progress" ] ]

viewFooter : Html msg
viewFooter =
    footer [ class "footer"]
           [ div [ class "content has-text-centered" ]
                 [ p []
                     [ text "Development by "
                     , a [ href "https://shootacean.com" ] [ text "shootacean." ]
                     , br [] []
                     , a [ href "https://github.com/shootacean/year-progress"] [ text "Source code" ]
                     ]
                 ]
           ]

viewProgressBar : Int -> Html msg
viewProgressBar v =
    let
        remain = 100 - v
    in
        Progress.progressMulti
            [ [ Progress.danger, Progress.value <| toFloat v, Progress.label <| toString v]
            , [ Progress.success, Progress.animated, Progress.value <| toFloat remain, Progress.label <| toString remain]
            ]

viewDetail : Date -> Float -> Html msg
viewDetail date diffDays =
    let
        diffMonths = diffDays / 30.5
        diffWeeks = diffDays / 7
        diffHours = diffDays * 24
        diffMinutes = diffHours * 60
        diffSeconds = diffMinutes * 60
    in
        table [ class "table is-striped" ]
              [ tbody []
                      [ viewDetailRow "Today" (Date.format "yyyy-MM-dd" date)
                      , viewDetailRow "Months" (formatNumber diffMonths)
                      , viewDetailRow "Weeks" (formatNumber diffWeeks)
                      , viewDetailRow "Days" (formatNumber diffDays)
                      , viewDetailRow "Hours" (formatNumber diffHours)
                      , viewDetailRow "Minutes" (formatNumber diffMinutes)
                      , viewDetailRow "Seconds" (formatNumber diffSeconds)
                      ]
              ]

viewDetailRow : String -> String -> Html msg
viewDetailRow l v =
    tr []
       [ th [] [ text l ]
       , td [] [ text v ]
       ]


-- helpers

thisYearLastDate : Int -> Date
thisYearLastDate y = Date.fromCalendarDate y Dec 31

formatNumber : Float -> String
formatNumber v =
    format customLocale v

customLocale : Locale
customLocale = { usLocale| decimals = Exact 0 }
