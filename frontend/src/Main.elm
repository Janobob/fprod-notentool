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
    | SemesterAdd SemesterAdd.Model
    | SemesterDetail Int
    | SemesterEdit SemesterEdit.Model
    | SemesterDelete SemesterDelete.Model

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
    | SemesterAddMsg SemesterAdd.Msg
    | SemesterEditMsg SemesterEdit.Msg
    | SemesterDeleteMsg SemesterDelete.Msg
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
            let
                (listModel, listCmd) = SemesterList.init
            in
            ( { header = header
            , page = SemesterList listModel
            , navKey = key
            }
            , Cmd.batch
                [ Cmd.map SemesterListMsg listCmd
                , Nav.pushUrl key "/semesters"
                ]
            )

        SemesterListRoute ->
            let
                (listModel, listCmd) = SemesterList.init
            in
            ( { header = header
            , page = SemesterList listModel
            , navKey = key
            }
            , Cmd.map SemesterListMsg listCmd
            )

        SemesterAddRoute ->
            ( { header = header
            , page = SemesterAdd SemesterAdd.init
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
            let
                (editModel, editCmd) = SemesterEdit.init id
            in
            ( { header = header, page = SemesterEdit editModel, navKey = key }
            , Cmd.map SemesterEditMsg editCmd
            )

        SemesterDeleteRoute id ->
            let
                (deleteModel, deleteCmd) = SemesterDelete.init id
            in
            ( { header = header, page = SemesterDelete deleteModel, navKey = key }
            , Cmd.map SemesterDeleteMsg deleteCmd
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

        SemesterAddMsg SemesterAdd.Cancel ->
            ( model
            , Nav.pushUrl model.navKey "/semesters"
            )

        SemesterAddMsg addMsg ->
            case model.page of
                SemesterAdd addModel ->
                    let
                        (newModel, cmd) = SemesterAdd.update addMsg addModel
                    in
                    ( { model | page = SemesterAdd newModel }
                    , Cmd.map SemesterAddMsg cmd
                    )

                _ ->
                    (model, Cmd.none)

        SemesterEditMsg SemesterEdit.Cancel ->
            (model, Nav.pushUrl model.navKey "/semesters")

        SemesterEditMsg subMsg ->
            case model.page of
                SemesterEdit editModel ->
                    let
                        (newModel, cmd) = SemesterEdit.update subMsg editModel
                    in
                    ( { model | page = SemesterEdit newModel }
                    , Cmd.map SemesterEditMsg cmd
                    )

                _ ->
                    (model, Cmd.none)

        SemesterDeleteMsg SemesterDelete.Cancel ->
            (model, Nav.pushUrl model.navKey "/semesters")

        SemesterDeleteMsg subMsg ->
            case model.page of
                SemesterDelete deleteModel ->
                    let
                        (newModel, cmd) = SemesterDelete.update subMsg deleteModel
                    in
                    ( { model | page = SemesterDelete newModel }
                    , Cmd.map SemesterDeleteMsg cmd
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
                    let
                        (listModel, listCmd) = SemesterList.init
                    in
                    ( { model | page = SemesterList listModel }
                    , Cmd.map SemesterListMsg listCmd
                    )

                Just SemesterListRoute ->
                    let
                        (listModel, listCmd) = SemesterList.init
                    in
                    ( { model | page = SemesterList listModel }
                    , Cmd.map SemesterListMsg listCmd
                    )

                Just SemesterAddRoute ->
                    ( { model | page = SemesterAdd SemesterAdd.init }, Cmd.none )

                Just (SemesterDetailRoute id) ->
                    ( { model | page = SemesterDetail id }, Cmd.none )

                Just (SemesterEditRoute id) ->
                    let
                        (editModel, editCmd) = SemesterEdit.init id
                    in
                    ( { model | page = SemesterEdit editModel }
                    , Cmd.map SemesterEditMsg editCmd
                    )

                Just (SemesterDeleteRoute id) ->
                    let
                        (deleteModel, deleteCmd) = SemesterDelete.init id
                    in
                    ( { model | page = SemesterDelete deleteModel }
                    , Cmd.map SemesterDeleteMsg deleteCmd
                    )

                Nothing ->
                    (model, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    { title = "Notentool"
    , body =
        [ Html.map HeaderMsg (Header.view model.header)
        , div [ class "container" ]
            [ case model.page of
                SemesterList listModel ->
                    Html.map SemesterListMsg (SemesterList.view listModel)
                SemesterAdd addModel ->
                    Html.map SemesterAddMsg (SemesterAdd.view addModel)
                SemesterDetail id ->
                    SemesterDetail.view id
                SemesterEdit editModel ->
                    Html.map SemesterEditMsg (SemesterEdit.view editModel)
                SemesterDelete deleteModel ->
                    Html.map SemesterDeleteMsg (SemesterDelete.view deleteModel)
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