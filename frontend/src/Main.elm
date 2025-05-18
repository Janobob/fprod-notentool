module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), s, top, parse, map, oneOf, int)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import View.Layout.Header as Header
import View.Pages.Semester.SemesterList as SemesterList
import View.Pages.Semester.SemesterAdd as SemesterAdd
import View.Pages.Semester.SemesterDetail as SemesterDetail
import View.Pages.Semester.SemesterEdit as SemesterEdit
import View.Pages.Semester.SemesterDelete as SemesterDelete
import Shared.Models.Semester exposing (Semester)

type Page =
    SemesterList SemesterList.Model
    | SemesterAdd
    | SemesterDetail Int
    | SemesterEdit Int
    | SemesterDelete Int

type Route =
    HomeRoute
    | SemesterListRoute
    | SemesterAddRoute
    | SemesterDetailRoute Int
    | SemesterEditRoute Int
    | SemesterDeleteRoute Int

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

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ s "semesters" |> map SemesterListRoute
        , s "semesters" </> s "add" |> map SemesterAddRoute
        , s "semesters" </> s "edit" </> int |> map SemesterEditRoute
        , s "semesters" </> s "delete" </> int |> map SemesterDeleteRoute
        , s "semesters" </> int |> map SemesterDetailRoute
        , top |> map HomeRoute
        ]


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

        SemesterDetailRoute id ->
            ( { header = header
              , page = SemesterDetail id
              , navKey = key
              }
            , Cmd.none
            )

        SemesterEditRoute id ->
            ( { header = header, page = SemesterEdit id, navKey = key }
            , Cmd.none
            )

        SemesterDeleteRoute id ->
            ( { header = header, page = SemesterDelete id, navKey = key }
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
        
        SemesterListMsg (SemesterList.NavigateToDetail id) ->
            ( model
            , Nav.pushUrl model.navKey ("/semesters/" ++ String.fromInt id)
            )

        SemesterListMsg (SemesterList.NavigateToEdit id) ->
            ( model
            , Nav.pushUrl model.navKey ("/semesters/" ++ "edit/" ++ String.fromInt id )
            )

        SemesterListMsg (SemesterList.NavigateToDelete id) ->
            ( model
            , Nav.pushUrl model.navKey ("/semesters/" ++ "delete/" ++ String.fromInt id )
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

                Just (SemesterDetailRoute id) ->
                    ( { model | page = SemesterDetail id }, Cmd.none )

                Just (SemesterEditRoute id) ->
                    ( { model | page = SemesterEdit id }, Cmd.none )

                Just (SemesterDeleteRoute id) ->
                    ( { model | page = SemesterDelete id }, Cmd.none )

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
                SemesterDetail id ->
                    SemesterDetail.view id
                SemesterEdit id ->
                    SemesterEdit.view id
                SemesterDelete id ->
                    SemesterDelete.view id
            ]
        ]
    }

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