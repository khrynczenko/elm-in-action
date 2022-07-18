module PhotoGroove exposing (main)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser

type alias Message = { data: String, description: String }

initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ] (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ] []
        ]


viewThumbnail : String -> { url : String } -> Html Message
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList
            [ ( "selected"
              , selectedUrl
                    == thumb.url
              )
            ]
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []

update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }
    else
        model


main =
    Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
