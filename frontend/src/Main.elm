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
import View.Pages.Module.ModuleDetail as ModuleDetail
import Shared.Models.Semester exposing (Semester)

type Page =
    SemesterList SemesterList.Model
    | SemesterAdd SemesterAdd.Model
    | SemesterDetail SemesterDetail.Model
    | SemesterEdit SemesterEdit.Model
    | SemesterDelete SemesterDelete.Model
    | ModuleDetail ModuleDetail.Model

type Route =
    HomeRoute
    | SemesterListRoute
    | SemesterAddRoute
    | SemesterDetailRoute Int
    | SemesterEditRoute Int
    | SemesterDeleteRoute Int
    | ModuleDetailRoute Int

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
    | SemesterDetailMsg SemesterDetail.Msg
    | SemesterEditMsg SemesterEdit.Msg
    | SemesterDeleteMsg SemesterDelete.Msg
    | ModuleDetailMsg ModuleDetail.Msg
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
        , s "modules" </> int |> map ModuleDetailRoute
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
            let
                (detailModel, detailCmd) = SemesterDetail.init id key
            in
            ( { header = header, page = SemesterDetail detailModel, navKey = key }
            , Cmd.map SemesterDetailMsg detailCmd
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

        ModuleDetailRoute id ->
            let
                (moduleDetailModel, moduleDetailCmd) = ModuleDetail.init id key
            in
            ( { header = header, page = ModuleDetail moduleDetailModel, navKey = key }
            , Cmd.map ModuleDetailMsg moduleDetailCmd
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

        SemesterAddMsg (SemesterAdd.SubmitResult (Ok _)) ->
            ( model, Nav.pushUrl model.navKey "/semesters" )

        SemesterAddMsg (SemesterAdd.SubmitResult (Err err)) ->
            -- Let the page handle the error
            case model.page of
                SemesterAdd addModel ->
                    let
                        (newModel, cmd) = SemesterAdd.update (SemesterAdd.SubmitResult (Err err)) addModel
                    in
                    ( { model | page = SemesterAdd newModel }
                    , Cmd.map SemesterAddMsg cmd
                    )

                _ ->
                    (model, Cmd.none)

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

        SemesterDeleteMsg (SemesterDelete.SemesterDeleted (Ok _)) ->
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

        SemesterDetailMsg subMsg ->
            case model.page of
                SemesterDetail detailModel ->
                    let
                        (newModel, cmd) = SemesterDetail.update subMsg detailModel
                    in
                    ( { model | page = SemesterDetail newModel }
                    , Cmd.map SemesterDetailMsg cmd
                    )

                _ ->
                    (model, Cmd.none)

        ModuleDetailMsg subMsg ->
            case model.page of
                ModuleDetail detailModel ->
                    let
                        (newModel, cmd) = ModuleDetail.update subMsg detailModel
                    in
                    ( { model | page = ModuleDetail newModel }
                    , Cmd.map ModuleDetailMsg cmd
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
                    let
                        (detailModel, detailCmd) = SemesterDetail.init id model.navKey
                    in
                    ( { model | page = SemesterDetail detailModel }
                    , Cmd.map SemesterDetailMsg detailCmd
                    )

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

                Just (ModuleDetailRoute id) ->
                    let
                        (moduleDetailModel, moduleDetailCmd) = ModuleDetail.init id model.navKey
                    in
                    ( { model | page = ModuleDetail moduleDetailModel }
                    , Cmd.map ModuleDetailMsg moduleDetailCmd
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
                SemesterDetail detailModel ->
                    Html.map SemesterDetailMsg (SemesterDetail.view detailModel)
                SemesterEdit editModel ->
                    Html.map SemesterEditMsg (SemesterEdit.view editModel)
                SemesterDelete deleteModel ->
                    Html.map SemesterDeleteMsg (SemesterDelete.view deleteModel)
                ModuleDetail detailModel ->
                    Html.map ModuleDetailMsg (ModuleDetail.view detailModel)
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