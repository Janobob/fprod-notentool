module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), s, top, parse, map, oneOf)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import View.Layout.Header as Header
import View.Pages.Semester.SemesterList as SemesterList
import View.Pages.Semester.SemesterAdd as SemesterAdd
import Shared.Models.Semester exposing (Semester)

type Page =
    SemesterList SemesterList.Model
    | SemesterAdd

type Route =
    HomeRoute
    | SemesterListRoute
    | SemesterAddRoute

type alias Model =
    { 
        header : Header.Model,
        page : Page,
        navKey: Nav.Key
    }

type Msg = 
    HeaderMsg Header.Msg 
    | SemesterListMsg SemesterList.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    let
        header = Header.init
        route = parse routeParser url |> Maybe.withDefault HomeRoute
    in
    case route of
        HomeRoute ->
            ( { header = header
              , page = SemesterList SemesterList.init
              , navKey = key
              }
            , Nav.pushUrl key "/semesters"
            )

        SemesterListRoute ->
            ( { header = header
              , page = SemesterList SemesterList.init
              , navKey = key
              }
            , Cmd.none
            )

        SemesterAddRoute ->
            ( { header = header
              , page = SemesterAdd
              , navKey = key
              }
            , Cmd.none
            )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HeaderMsg hmsg ->
            ( { model | header = Header.update hmsg model.header }, Cmd.none )

        SemesterListMsg SemesterList.NavigateToAdd ->
            ( model
            , Nav.pushUrl model.navKey "/semesters/add"
            )

        SemesterListMsg smsg ->
            case model.page of
                SemesterList smodel ->
                    let
                        (newModel, cmd) =
                            SemesterList.update smsg smodel
                    in
                    ( { model | page = SemesterList newModel }
                    , Cmd.map SemesterListMsg cmd
                    )

                _ ->
                    (model, Cmd.none)

        LinkClicked req ->
            case req of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.navKey (Url.toString url))

                Browser.External href ->
                    (model, Nav.load href)

        UrlChanged url ->
            case parse routeParser url of
                Just HomeRoute ->
                    ( { model | page = SemesterList SemesterList.init }, Cmd.none )

                Just SemesterListRoute ->
                    ( { model | page = SemesterList SemesterList.init }, Cmd.none )

                Just SemesterAddRoute ->
                    ( { model | page = SemesterAdd }, Cmd.none )

                Nothing ->
                    (model, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    { title = "Notentool"
    , body =
        [ Html.map HeaderMsg (Header.view model.header)
        , div [ class "container" ]
            [ case model.page of
                SemesterList smodel ->
                    Html.map SemesterListMsg (SemesterList.view smodel)
                SemesterAdd ->
                    SemesterAdd.view
            ]
        ]
    }

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ top |> map HomeRoute
        , s "semesters" |> map SemesterListRoute
        , s "semesters" </> s "add" |> map SemesterAddRoute
        ]

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }