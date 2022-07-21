module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Random
import Json.Decode
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline as JP


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Photo =
    { url : String
    , size: Int
    , title: String
    }


type alias Model =
    { status: Status, chosenSize : ThumbnailSize }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))


type ThumbnailSize
    = Small
    | Medium
    | Large

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loading -> Loading
        Loaded photos _ -> Loaded photos url
        Errored str -> Errored str


initialModel : Model
initialModel =
    { status = Loading
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


view model =
    div [ class "content" ] <|
          case model.status of
              Loaded photos selectedUrl ->
                  viewLoaded photos selectedUrl model.chosenSize
              Loading -> []
              Errored errorMessage -> [ text ("Error: " ++ errorMessage) ]
          

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize
    =
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString chosenSize) ] 
            (List.map (viewThumbnail selectedUrl) photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ selectedUrl)
            ] [] ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList
            [ ( "selected"
              , selectedUrl == thumb.url
              )
            ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []

splitResponse : String -> String -> List String
splitResponse = String.split

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                    |> Random.generate GotRandomPhoto
                    |> Tuple.pair model
                Loaded ([]) _ ->
                    ( model, Cmd.none )
                Loading -> (model, Cmd.none)
                Errored errorMessage -> (model, Cmd.none)

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )
        GotPhotos result ->
            case result of
                Result.Err errorMessage -> ({ model | status = Errored "Server Error!" }, Cmd.none)
                Result.Ok photos -> 
                    let
                        firstPhoto = List.head photos
                    in
                        case firstPhoto of
                            Nothing -> (model, Cmd.none)
                            Just photo -> ({model | status = Loaded photos photo.url}, Cmd.none)

photoDecoder : Json.Decode.Decoder Photo
photoDecoder = Json.Decode.map3
    (\x y z -> Photo x y z)
    (Json.Decode.field "url" Json.Decode.string)
    (Json.Decode.field "size" Json.Decode.int)
    (Json.Decode.field "title" Json.Decode.string)

photoDecoder2 : Decoder Photo
photoDecoder2 =
    succeed Photo
        |> JP.required "url" string
        |> JP.required "size" int
        |> JP.optional "title" string "(untitled)"


requestPhotos : Cmd Msg
requestPhotos =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder2)
        }

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, requestPhotos )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
