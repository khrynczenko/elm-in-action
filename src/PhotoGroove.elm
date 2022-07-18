module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo, selectedUrl : String, chosenSize : ThumbnailSize }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectedIndex Int


type ThumbnailSize
    = Small
    | Medium
    | Large


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 2


photoArray : Array Photo
photoArray =
    Array.fromList
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]


initialModel : Model
initialModel =
    { photos = Array.toList photoArray
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label [ onClick (ClickedSize size) ]
        [ input [ type_ "radio", name "size" ] []
        , text (sizeToString size)
        ]


getPhotoUrl : Int -> String
getPhotoUrl i =
    case Array.get i photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> { url : String } -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList
            [ ( "selected"
              , selectedUrl == thumb.url
              )
            ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotSelectedIndex u ->
            ( { model | selectedUrl = getPhotoUrl u }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
