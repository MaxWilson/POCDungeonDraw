module App

open Browser.Dom

open Feliz
open Feliz.Router
open Fable.Core.JsInterop

open Components
open Elmish
open Elmish.Navigation
importSideEffects "./styles.sass"


type 't Deferred = NotStarted | InProgress | Ready of 't
type IdentityProvider = Facebook | AAD | Erroneous
type Identity = (IdentityProvider * string) option
type NavCmd = Load of string | HomeScreen
type Model = { tag: string; currentUser: Identity Deferred; currentParty: string Deferred; strokes: (Stroke * string) list }
type Msg =
    | SetTag of string
    | ReceiveParty of string Deferred
    | ReceiveIdentity of Identity Deferred
    | ReceiveStroke of Stroke

type TestData = { tag: string; payload: string list }

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
    | ReceiveParty v -> { model with currentParty = v }, []
    | ReceiveIdentity id -> { model with currentUser = id }, []
    | SetTag v -> { model with tag = v }, Cmd.navigate v
    | ReceiveStroke stroke -> 
        printfn $"{(stroke.paths |> Array.map(fun p -> p.x, p.y), colorOf model.strokes.Length)}"
        { model with strokes = (stroke, colorOf model.strokes.Length)::model.strokes }, []

let save model dispatch fileName =
    promise {
        match model.currentParty with
        | Ready party ->
            let data : TestData = { tag = fileName; payload = [ party ] }
            do! Thoth.Fetch.Fetch.post($"/api/WriteData", data)
            SetTag fileName |> dispatch
        | _ -> ()
        }

let load model dispatch fileName =
    promise {
        ReceiveParty (InProgress) |> dispatch
        let! data = Thoth.Fetch.Fetch.tryFetchAs($"/api/ReadData/{fileName}")
        match data with
        | Ok ([{ payload = head::_}: TestData]) ->
            ReceiveParty (Ready head) |> dispatch
        | Ok lst ->
            ()
        | Error err ->
            ()
        }

module Nav =
    open Elmish.Navigation
    let parse (loc:Browser.Types.Location) : NavCmd =
        printfn $"Loc.hash='{loc.hash}'"
        match loc.hash with
        | s when s.StartsWith("#") -> s.Substring(1).Replace("/", "") |> Load
        | _ -> HomeScreen
    let nav (navCmd: NavCmd) model =
        match navCmd with
        | HomeScreen -> model, Cmd.Empty
        | Load tag ->
            model, Cmd.ofSub (fun dispatch -> load model dispatch tag |> Promise.start)

let init (onload:NavCmd) =
    { tag = System.Guid.NewGuid().ToString(); currentUser = NotStarted; currentParty = NotStarted; strokes = [] } |> Nav.nav onload

let view (model:Model) dispatch =
    class' "main" Html.div [
        match model.currentUser with
        | NotStarted -> Html.div [prop.text "Initializing identity..."]
        | InProgress -> Html.div [prop.text "Retreiving identity..."]
        | Ready None -> Html.div [Html.text "Hello, stranger."; LoginButton dispatch]
        | Ready (Some ((Facebook | AAD | Erroneous), accountName)) -> Html.div [Html.text $"Hello, {accountName}"; Html.button [prop.text "Log out"; prop.onClick (thunk1 navigateTo @".auth/logout")]]
        class' "sketching" Html.div [
            SketchPad(dispatch << ReceiveStroke)
            Svg.svg [
                svg.className "display"
                svg.viewBox(0, 0, 400, 400)
                svg.children [
                    for (stroke,color) in model.strokes do
                        Svg.path [
                                [match stroke.paths |> List.ofArray with
                                        | first :: rest ->
                                            'M', [[first.x; first.y]]
                                            yield! rest |> List.map (fun p -> 'L', [[p.x; p.y]])
                                        | [] -> ()]
                                |> svg.d
                                svg.stroke color
                            ]
                    ]
                svg.strokeWidth 4
                svg.fill "none"
                ]

            ]
        Html.div [
            Html.textarea [
                prop.placeholder "enter some text"
                match model.currentParty with
                | Ready party ->
                    prop.valueOrDefault party
                | _ -> prop.value ""
                prop.onChange (Ready >> ReceiveParty >> dispatch)
                ]
            SaveButton (save model dispatch >> Promise.start)
            Html.span "Tag:"
            Html.input [prop.placeholder "Enter a tag"; prop.valueOrDefault model.tag; prop.onChange (SetTag >> dispatch)]
            ]
        Counter()
        Message model.tag
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