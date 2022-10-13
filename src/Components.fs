module Components
open Feliz
open Fetch
open Thoth.Fetch

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
            static member inline create (props: ISketchProperty list) = Interop.reactApi.createElement(import "ReactSketchCanvas" "react-sketch-canvas", createObj !!props)
            static member inline width (v:int) = mkSketchAttr "width" v
            static member inline height (v: int) = mkSketchAttr "height" v
            static member inline strokeWidth (v: int) = mkSketchAttr "strokeWidth" v
            static member inline strokeColor (v: string) = mkSketchAttr "strokeColor" v
            static member inline style (props: IStyleAttribute list) : ISketchProperty = !!Interop.mkAttr "style" (createObj !!props)
            static member inline ref (ref: IRefValue<#Browser.Types.HTMLElement option>): ISketchProperty = !!Interop.mkAttr "ref" ref
            static member inline onStroke (handler: obj -> unit): ISketchProperty = mkSketchAttr "onStroke" handler
            static member inline onChange (handler: obj -> unit): ISketchProperty = mkSketchAttr "onChange" handler
        [<Erase>]
        type style =
            static member inline border (v:string) = Interop.mkStyle "border" v
            static member inline borderRadius (v:string) = Interop.mkStyle "borderRadius" v
open SketchCanvas
open Fable.Core.JsInterop

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
let Counter () =
    let (count, setCount) = React.useState (0)

    Html.div [ Html.h1 count
               Html.button [ prop.text "Increment"
                             prop.onClick (fun _ -> setCount (count + 1)) ] ]

[<ReactComponent>]
let Message (tag:string) =
    let (message, setMessage) = React.useState (NotStarted)
    Html.div [  Html.button [   prop.text "Get a message from the API"
                                prop.onClick
                                    (fun _ ->
                                        promise {
                                            setMessage InProgress
                                            let! message =
                                                Fetch.get (
                                                    $"/api/GetMessage?name={tag}",
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
let SaveButton saveCmd =
    let fileName, update = React.useState None
    Html.div [
        match fileName with
        | None ->
            Html.button [
                prop.text "Save"
                prop.onClick (thunk1 update (Some ""))
                ]
        | Some fileName ->
            Html.span [
                let invalid = fileName |> System.String.IsNullOrWhiteSpace
                Html.input [prop.placeholder "E.g. myStuff1"; prop.valueOrDefault fileName; prop.onChange (Some >> update)]
                Html.text $"You typed: '{fileName}'"
                Html.button [prop.disabled invalid; prop.text "OK"; if not invalid then prop.onClick (fun _ -> saveCmd fileName; update None)]
                Html.button [prop.disabled invalid; prop.text "Cancel"; prop.onClick (thunk1 update None)]
                ]
        ]