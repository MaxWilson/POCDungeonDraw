module App

open Browser.Dom

open Feliz
open Feliz.Router
open Fable.Core.JsInterop

open Components
open Elmish
open Elmish.Navigation
open Thoth.Json

importSideEffects "./styles.sass"

type GraphicElement =
    | Stroke of Stroke * color: string
    | Text of string * Point * color: string
type 't Deferred = NotStarted | InProgress | Ready of 't
type IdentityProvider = Facebook | AAD | Erroneous
type Identity = (IdentityProvider * string) option
type NavCmd = Load of string | HomeScreen
type SavedPicture = { owner: string; tag: string; payload: GraphicElement list }
type Model = {
    alias: string; currentUser: Identity Deferred;
    strokes: GraphicElement list; loadedPictures: SavedPicture list Deferred
    pubsubConnection: (string -> unit) option
    }
type Msg =
    | SetAlias of string
    | SetLocation of string
    | ReceiveIdentity of Identity Deferred
    | ReceiveSavedPictures of SavedPicture list Deferred
    | ReceiveStroke of Stroke
    | ReceiveText of string
    | ResetToPicture of SavedPicture
    | Connected of (string -> unit)
    | RemoteReceiveGraphics of GraphicElement list

let class' (className: string) ctor (elements: _ seq) = ctor [prop.className className; prop.children elements]
let navigateTo (url: string) =
    Browser.Dom.window.location.assign url

[<ReactComponent>]
let LoginButton dispatch =
    let loggingIn, update = React.useState (thunk false)
    if loggingIn then
        Html.span [
            Html.text "Log in with"
            Html.button [prop.text "Facebook"; prop.onClick (thunk1 navigateTo @".auth/login/facebook")]
            Html.button [prop.text "AAD"; prop.onClick (thunk1 navigateTo @".auth/login/aad")]
            ]
    else
        Html.button [prop.text "Login"; prop.onClick (fun _ -> update true)]
[<ReactComponent>]
let TextEntry dispatch =
    let txt, update = React.useState ""
    Html.form [
        prop.onSubmit(fun e -> e.preventDefault(); ReceiveText txt |> dispatch; update "")
        prop.children [
            Html.input [prop.type'.text; prop.placeholder "Enter some text to display"; prop.valueOrDefault txt; prop.onChange update]
            Html.button [prop.type'.submit; prop.text "OK"]
            ]
        ]

let colorOf ix =
    let colors = [
        "red"
        "yellow"
        "orange"
        "green"
        "blue"
        "indigo"
        "violet"
        "black"
        "white"
        ]
    colors[ix % colors.Length]
let update msg model =
    match msg with
    | ReceiveIdentity id -> { model with currentUser = id }, []
    | ReceiveSavedPictures v ->
        match v with
        | Ready(head::_) ->
            { model with loadedPictures = v }, Cmd.ofMsg (ResetToPicture head)
        | _ ->
            { model with loadedPictures = v }, []
    | ResetToPicture v ->
        { model with strokes = v.payload }, []
    | SetAlias v -> { model with alias = v }, []
    | SetLocation v -> model, Cmd.navigate v
    | ReceiveStroke stroke ->
        let stroke' = Stroke(stroke, colorOf model.strokes.Length)
        match model.pubsubConnection with None -> () | Some transmit -> Encode.Auto.toString(0, [stroke']) |> transmit
        { model with strokes = model.strokes@[stroke'] }, []
    | ReceiveText txt ->
        // place the text near the start of a recent line
        let coord =
            match model.strokes |> List.rev
                    |> List.tryPick (function
                        | Stroke(stroke,_) -> stroke.paths |> Array.tryHead
                        | Text(_, point, _) -> createObj ["x", point.x; "y", (point.y + 50. |> box)] |> unbox |> Some) with
            | Some point -> point
            | None -> createObj ["x", 0.; "y", 0.] |> unbox
        let txt = Text(txt, coord, colorOf model.strokes.Length)
        match model.pubsubConnection with None -> () | Some transmit -> Encode.Auto.toString(0, [txt]) |> transmit
        { model with strokes = model.strokes@[txt] }, []
    | RemoteReceiveGraphics graphics ->
        { model with strokes = model.strokes@graphics }, []
    | Connected send ->
        { model with pubsubConnection = Some send }, []

let save model dispatch fileName =
    promise {
        let data = {| id = "ignore"; owner = ""; tag = fileName; payload = model.strokes |}
        try
            do! Thoth.Fetch.Fetch.post($"/api/WriteData", data)
            SetLocation fileName |> dispatch
        with
        | err when err.Message.Contains "401 Unauthorized" ->
            Browser.Dom.window.alert($"Sorry, you can't save until you log in. Please hit the 'Login' button at the top.")
            raise err
        | err ->
            Browser.Dom.window.alert($"Oops! Something went wrong. {err}")
            raise err
        }

let load model dispatch fileName =
    promise {
        ReceiveSavedPictures (InProgress) |> dispatch
        let! data = Thoth.Fetch.Fetch.tryFetchAs($"/api/ReadData/{fileName}")
        match data with
        | Ok matches ->
            ReceiveSavedPictures (Ready matches) |> dispatch
            let! clientUrlResponse = Fetch.fetch $"/api/CreateTokenRequest/dnd/{fileName}/" []
            let! clientUrl = clientUrlResponse.text()
            let onMsg (txt: string) =
                match txt |> Decode.Auto.fromString with
                | Error e -> shouldntHappen e
                | Ok v -> v |> RemoteReceiveGraphics |> dispatch
            WebSync.connect(clientUrl, fileName, ignore, onMsg) |> Connected |> dispatch
        | Error err ->
            shouldntHappen err
        }

module Nav =
    open Elmish.Navigation
    let parse (loc:Browser.Types.Location) : NavCmd =
        match loc.hash with
        | s when s.StartsWith("#") -> s.Substring(1).Replace("/", "") |> Load
        | _ -> HomeScreen
    let nav (navCmd: NavCmd) model =
        match navCmd with
        | HomeScreen -> model, Cmd.Empty
        | Load pictureName ->
            model, Cmd.ofSub (fun dispatch -> load model dispatch pictureName |> Promise.start)

let init (onload:NavCmd) =
    {   alias = ""
        currentUser = NotStarted
        strokes = []
        loadedPictures = NotStarted
        pubsubConnection = None
    } |> Nav.nav onload

let view (model:Model) dispatch =
    class' "main" Html.div [
        match model.currentUser with
        | NotStarted -> Html.div [prop.text "Initializing identity..."]
        | InProgress -> Html.div [prop.text "Retreiving identity..."]
        | Ready None -> Html.div [Html.text "Hello, stranger."; LoginButton dispatch]
        | Ready (Some ((Facebook | AAD | Erroneous), accountName)) -> Html.div [Html.text $"Hello, {accountName}"; Html.button [prop.text "Log out"; prop.onClick (thunk1 navigateTo @".auth/logout")]]
        class' "sketching" Html.div [
            match model.loadedPictures with
            | InProgress -> Html.div "Loading pictures, please wait..."
            | _ ->
                SketchPad(dispatch << ReceiveStroke)
                Svg.svg [
                    svg.className "display"
                    svg.viewBox(0, 0, 400, 300)
                    svg.children [
                        for element in model.strokes do
                            match element with
                            | Stroke(stroke, color) ->
                                Svg.path [
                                        [match stroke.paths |> List.ofArray with
                                                | first :: rest ->
                                                    'M', [[first.x; first.y]]
                                                    yield! rest |> List.map (fun p -> 'L', [[p.x; p.y]])
                                                | [] -> ()]
                                        |> svg.d
                                        svg.stroke color
                                        svg.strokeWidth 4
                                        svg.fill "none"
                                    ]
                            | Text(text, point, color) ->
                                Svg.text [
                                    svg.x point.x
                                    svg.y point.y
                                    svg.text text
                                    svg.stroke color
                                    svg.fontSize 40
                                    svg.fill color
                                    ]
                        ]
                    ]
            ]
        TextEntry dispatch
        SaveButton (save model dispatch)

        Counter()
        Html.div [
            Html.span "Name override:"
            Html.input [prop.placeholder "Enter your alias, Mr. Bond"; prop.valueOrDefault model.alias; prop.onChange (SetAlias >> dispatch)]
            ]
        Message model.alias
        ]

open Browser.Dom
open Elmish
open Elmish.React

module Auth =
    type Provider = Facebook | AAD
    type Principal = {
        userId: string
        userRoles: string list
        identityProvider: string
        userDetails: string
        }
    type AuthResponse = {
        clientPrincipal: Principal option
        }

Program.mkProgram init update view
    |> Program.withSubscription(fun model ->
        Cmd.ofSub(fun dispatch ->
            promise {
                dispatch (ReceiveIdentity InProgress)
                let! resp = Thoth.Fetch.Fetch.tryFetchAs ".auth/me"
                match resp with
                | Ok (v: Auth.AuthResponse) ->
                    v.clientPrincipal |> Option.map (function
                    | v when v.identityProvider = "facebook" -> Facebook, v.userDetails
                    | v when v.identityProvider = "aad" -> AAD, v.userDetails
                    | v -> Erroneous, $"{v.identityProvider}/{v.userDetails}")
                    |> Ready |> ReceiveIdentity
                | Error err -> ReceiveIdentity (Ready (Some (Erroneous, err.ToString())))
                |> dispatch
            }
            |> Promise.start
            )
        )
    |> Program.toNavigable Nav.parse Nav.nav
    |> Program.withReactBatched "root"
    |> Program.run