module Main exposing (main)

import Browser.Navigation as Nav
import Url exposing (Url)
import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
    { page : Page, key: Nav.Key }


type Page
    = Gallery Gallery.Model
    | Folders Folders.Model
    | SelectedPhoto String Folders.Model
    | NotFound

parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]

view : Model -> Document Msg
view model =
    let
        content =
            text "This isn't even my final form!"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]


isActive : { link : Page, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, Gallery ) -> True
        ( Gallery, _ ) -> False
        ( Folders, Folders ) -> True
        ( Folders, SelectedPhoto _ ) -> True
        ( Folders, _ ) -> False
        ( SelectedPhoto _, _ ) -> False
        ( NotFound, _ ) -> False


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            li
                [ classList
                    [ ( "active"
                      , isActive
                            { link = route
                            , page = page
                            }
                      )
                    ]
                ]
                [ a [ href url ] [ text caption ] ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]
    in
    nav [] [ logo, links ]


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url, key = key}, Cmd.none )

urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
    |> Maybe.withDefault NotFound

main : Program () Model Msg
main =
    Browser.application
            { init = init
            , onUrlRequest = ClickedLink
            , onUrlChange = ChangedUrl
            , subscriptions = \_ -> Sub.none
            , update = update
            , view = view
            }
