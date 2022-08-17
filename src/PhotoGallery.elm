module PhotoGallery exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (Attribute, Html, button, canvas, div, h1, h3, img, input, label, node, text)
import Html.Attributes as Attr exposing (class, classList, id, name, src, title, type_)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, int, string, succeed)
import Json.Decode.Pipeline as JP
import Json.Encode as Encode
import Random


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias Filters =
    { hue : Int, ripple : Int, noise : Int }


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity


type alias Model =
    { status : Status, chosenSize : ThumbnailSize, filters : Filters, activity : String }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | HueChanged Int
    | RippleChanged Int
    | NoiseChanged Int
    | GotActivity String


type ThumbnailSize
    = Small
    | Medium
    | Large


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loading ->
            Loading

        Loaded photos _ ->
            Loaded photos url

        Errored str ->
            Errored str


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , filters = { hue = 0, ripple = 0, noise = 0 }
    , activity = ""
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


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize model.filters model.activity

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name message magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide message
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> ThumbnailSize -> Filters -> String -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize filters activity =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "activity" ] [ text activity ]
    , div [ class "filters" ]
        [ viewFilter "Hue" HueChanged filters.hue
        , viewFilter "Ripple" RippleChanged filters.ripple
        , viewFilter "Noise" NoiseChanged filters.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas
        [ id "main-canvas"
        , class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


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


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.filters.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.filters.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.filters.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        HueChanged v ->
            let
                filters =
                    model.filters

                newFilters =
                    { filters | hue = v }
            in
            applyFilters { model | filters = newFilters }

        RippleChanged v ->
            let
                filters =
                    model.filters

                newFilters =
                    { filters | ripple = v }
            in
            applyFilters { model | filters = newFilters }

        NoiseChanged v ->
            let
                filters =
                    model.filters

                newFilters =
                    { filters | noise = v }
            in
            applyFilters { model | filters = newFilters }

        ClickedPhoto selectedUrl ->
            applyFilters { model | status = selectUrl selectedUrl model.status }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos result ->
            case result of
                Result.Err _ ->
                    ( { model | status = Errored "Server Error!" }, Cmd.none )

                Result.Ok photos ->
                    let
                        firstPhoto =
                            List.head photos
                    in
                    case firstPhoto of
                        Nothing ->
                            ( model, Cmd.none )

                        Just photo ->
                            applyFilters { model | status = Loaded photos photo.url }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> JP.required "url" string
        |> JP.required "size" int
        |> JP.optional "title" string "(untitled)"


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    Json.Decode.at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"


requestPhotos : Cmd Msg
requestPhotos =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }, requestPhotos )


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }
