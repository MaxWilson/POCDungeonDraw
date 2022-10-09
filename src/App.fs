module App

open Browser.Dom

open Feliz
open Fable.Core.JsInterop


open Components

type Model = { tag: string }
type Msg = SetTag of string
let init _ = { tag = System.Guid.NewGuid().ToString() }

let update msg model =
    match msg with
    | SetTag tag -> { model with tag = tag }

let view (model:Model) dispatch =
    Html.div [  sketchpad()
                Html.div [
                    Html.span "Tag:"
                    Html.input [prop.placeholder "Enter a tag"; prop.valueOrDefault model.tag; prop.onChange (SetTag >> dispatch)]
                    ]
                Counter()
                Message model.tag
                ]

open Browser.Dom
open Elmish
open Elmish.React

Program.mkSimple init update view
    |> Program.withReactBatched "root"
    |> Program.run