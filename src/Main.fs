module Main

open Elmish
open Elmish.Navigation
open Elmish.React
open Home

module Cmd =
    let ofSub f =
        [[], fun dispatch ->
                f dispatch
                { new System.IDisposable with
                               member this.Dispose(): unit = ()
                               }
            ]

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
                if model.pubsubConnection.IsNone then
                    do! joinChannel "default" dispatch
            }
            |> Promise.start
            )
        )
    |> Program.toNavigable Nav.parse Nav.nav
    |> Program.withReactBatched "root"
    |> Program.run