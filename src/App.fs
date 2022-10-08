module App

open Browser.Dom
open Fetch
open Thoth.Fetch
open Feliz
open Fable.Core.JsInterop

type 't Deferred = NotStarted | InProgress | Ready of 't

module SketchCanvas =
    open Feliz
    open Fable.Core
    open Fable.Core.JsInterop

    [<Erase>]
    type ISketchProperty =
        interface end

    [<AutoOpen>]
    module private Interop =
        let inline mkSketchAttr (key: string) (value: obj) : ISketchProperty = unbox (key, value)
    [<Erase>]
    type sketch =
        static member create (props: ISketchProperty list) = Interop.reactApi.createElement(import "ReactSketchCanvas" "react-sketch-canvas", createObj !!props)
        static member width (v:int) = mkSketchAttr "width" v
        static member height (v: int) = mkSketchAttr "height" v
        static member strokeWidth (v: int) = mkSketchAttr "strokeWidth" v
        static member strokeColor (v: string) = mkSketchAttr "strokeColor" v
        static member style (props: IStyleAttribute list) : ISketchProperty = !!Interop.mkAttr "style" (createObj !!props)
    [<Erase>]
    type style =
        static member inline border (v:string) = Interop.mkStyle "border" v
        static member inline borderRadius (v:string) = Interop.mkStyle "borderRadius" v

[<ReactComponent>]
let Counter () =
    let (count, setCount) = React.useState (0)

    Html.div [ Html.h1 count
               Html.button [ prop.text "Increment"
                             prop.onClick (fun _ -> setCount (count + 1)) ] ]

[<ReactComponent>]
let Message () =
    let (message, setMessage) = React.useState (NotStarted)
    Html.div [  Html.button [   prop.text "Get a message from the API"
                                prop.onClick
                                    (fun _ ->
                                        promise {
                                            setMessage InProgress
                                            let! message =
                                                Fetch.get (
                                                    "/api/GetMessage?name=FSharpie",
                                                    headers = [ HttpRequestHeaders.Accept "application/json" ]
                                                )

                                            setMessage (message |> Ready)
                                            return ()
                                        }
                                        |> ignore) ]
                match message with
                | NotStarted -> Html.none
                | InProgress -> Html.div "Executing..."
                | Ready (msg:string) -> Html.p msg
            ]
open SketchCanvas
[<ReactComponent>]
let App () =
    React.fragment [
        sketch.create [
            sketch.style [style.border "1em dashed purple"]
            sketch.height 400; sketch.width 600; sketch.strokeWidth 4; sketch.strokeColor "blue"
            ]
        Counter()
        Message() ]

open Browser.Dom

ReactDOM.render (App(), document.getElementById "root")
