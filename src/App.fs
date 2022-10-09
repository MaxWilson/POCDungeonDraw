module App

open Browser.Dom
open Fetch
open Thoth.Fetch
open Feliz
open Fable.Core.JsInterop

type 't Deferred = NotStarted | InProgress | Ready of 't


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

open Components.SketchCanvas
[<ReactComponent>]
let sketchpad dispatch =
    let canvas = React.useRef(None)
    let html, htmlUpdate = React.useState "Press the Export SVG button"
    let lastPath, lastPathUpdate = React.useState "No paths yet"
    let lastStroke, lastStrokeUpdate = React.useState "No strokes yet"
    Html.div [
        let exportSvg _ =
            promise {
                match canvas.current with
                | Some canvas ->
                    let! (svg: string) = canvas?exportSvg()
                    htmlUpdate svg
                | None -> ()
            } |> Promise.start
        sketch.create [
            sketch.ref canvas
            sketch.style [style.border "0.06em dashed purple"]
            sketch.height 400; sketch.width 600; sketch.strokeWidth 4; sketch.strokeColor "blue"
            sketch.onChange (fun path -> System.Console.WriteLine path)
            sketch.onStroke (fun path -> System.Console.WriteLine path)
            ]
        Html.div [
            Html.button [prop.text "Export SVG"; prop.onClick(exportSvg)]
            Html.textarea [prop.value html; prop.readOnly true]
            Html.textarea [prop.value lastStroke; prop.readOnly true]
            Html.textarea [prop.value lastPath; prop.readOnly true]
            ]
        ]

[<ReactComponent>]
let App () =
    React.fragment [
        sketchpad()
        Counter()
        Message() ]

open Browser.Dom

ReactDOM.render (App(), document.getElementById "root")
