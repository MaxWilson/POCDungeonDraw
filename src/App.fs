module App

open Browser.Dom

open Feliz
open Feliz.Router
open Fable.Core.JsInterop

open Components
open Elmish

type 't Deferred = NotStarted | InProgress | Ready of 't
type IdentityProvider = Facebook | AAD | Erroneous
type Identity = (IdentityProvider * string) option
type Model = { tag: string; currentUser: Identity Deferred; currentParty: string }
type Msg =
    | SetTag of string
    | UpdateParty of string
    | ReceiveIdentity of Identity Deferred

type TestData = { tag: string; payload: string list }

let init _ = { tag = System.Guid.NewGuid().ToString(); currentUser = NotStarted; currentParty = "" }, []

let navigateTo (url: string) =
    Browser.Dom.window.location.assign url

[<ReactComponent>]
let loginButton dispatch =
    let loggingIn, update = React.useState (thunk false)
    if loggingIn then
        Html.span [
            Html.text "Log in with"
            Html.button [prop.text "Facebook"; prop.onClick (thunk1 navigateTo @".auth/login/facebook")]
            Html.button [prop.text "AAD"; prop.onClick (thunk1 navigateTo @".auth/login/aad")]
            ]
    else
        Html.button [prop.text "Login"; prop.onClick (fun _ -> update true)]

let update msg model =
    match msg with
    | UpdateParty v -> { model with currentParty = v }, []
    | ReceiveIdentity id -> { model with currentUser = id }, []
    | SetTag v -> { model with tag = v }, Cmd.navigate v

let view (model:Model) dispatch =
    Html.div [
        match model.currentUser with
        | NotStarted -> Html.div [prop.text "Initializing identity..."]
        | InProgress -> Html.div [prop.text "Retreiving identity..."]
        | Ready None -> Html.div [Html.text "Hello, stranger."; loginButton dispatch]
        | Ready (Some ((Facebook | AAD | Erroneous), accountName)) -> Html.div [Html.text $"Hello, {accountName}"; Html.button [prop.text "Log out"; prop.onClick (thunk1 navigateTo @".auth/logout")]]
        sketchpad()
        Html.div [
            Html.textarea [
                prop.placeholder "enter some text"
                prop.valueOrDefault model.currentParty
                prop.onChange (UpdateParty >> dispatch)
                ]
            SaveButton (fun fileName -> Promise.start <| promise {
                let data : TestData = { tag = fileName; payload = [ model.currentParty ] }
                do! Thoth.Fetch.Fetch.post($"/api/WriteData", data)
                SetTag fileName |> dispatch
                })
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
                ()
            }
            |> Promise.start
            )
        )
    |> Program.withReactBatched "root"
    |> Program.run