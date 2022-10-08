module App

open Browser.Dom
open Fetch
open Thoth.Fetch
open Feliz
open Fable.Core.JsInterop

type 't Deferred = NotStarted | InProgress | Ready of 't

module SketchCanvas =
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    type StyleProps = {
        border: string
        borderRadius: string
        }
    let sampleStyle = { border = "0.625rem solid #9c9c9c"; borderRadius = "1.25rem" }
    type SketchCanvasProps =
        | Width of int
        | Height of int
        | StrokeWidth of int
        | StrokeColor of string
        | Style of obj
    let sampleProps = [Style sampleStyle; Height 400; Width 600; StrokeWidth 4; StrokeColor "red" ]
    let sketch (props : SketchCanvasProps list) : ReactElement =
        System.Console.WriteLine (keyValueList CaseRules.LowerFirst props)
        ofImport "ReactSketchCanvas" "react-sketch-canvas" (keyValueList CaseRules.LowerFirst props) []

[<ReactComponent>]
let Counter () =
    let (count, setCount) = React.useState (0)

    Html.div [ Html.h1 count
               Html.button [ prop.text "Increment"
                             prop.onClick (fun _ -> setCount (count + 1)) ] ]

[<ReactComponent>]
let Message () =
    let (message, setMessage) = React.useState (NotStarted)
    Html.div [ Html.button [ prop.text "Get a message from the API"
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

[<ReactComponent>]
let App () = React.fragment [
    SketchCanvas.sketch [
        SketchCanvas.Style (SketchCanvas.sampleStyle)
        SketchCanvas.Height 400; SketchCanvas.Width 600; SketchCanvas.StrokeWidth 4; SketchCanvas.StrokeColor "blue"
        ]
    Counter()
    Message() ]

open Browser.Dom

ReactDOM.render (App(), document.getElementById "root")
