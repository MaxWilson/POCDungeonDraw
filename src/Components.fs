module Components
open Feliz
open Fetch
open Thoth.Fetch

type 't Deferred = NotStarted | InProgress | Ready of 't
type Point = { x: float; y: float }
type Stroke = { paths: Point array }

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
        static member inline onStroke (handler: Stroke -> unit): ISketchProperty = mkSketchAttr "onStroke" handler
        static member inline onChange (handler: Point array -> unit): ISketchProperty = mkSketchAttr "onChange" handler
    [<Erase>]
    type style =
        static member inline border (v:string) = Interop.mkStyle "border" v
        static member inline borderRadius (v:string) = Interop.mkStyle "borderRadius" v
open SketchCanvas
open Fable.Core.JsInterop

[<ReactComponent>]
let SketchPad strokeColor receiveStroke =
    let canvas = React.useRef(None)
    let html, htmlUpdate = React.useState "Press the Export SVG button"
    let lastPath, lastPathUpdate = React.useState None
    let lastStroke, lastStrokeUpdate = React.useState { paths = Array.empty }
    sketch.create [
        sketch.ref canvas
        sketch.style [style.border "0.06em dashed purple"]
        sketch.height 300; sketch.width 300; sketch.strokeWidth 4; sketch.strokeColor strokeColor
        sketch.onChange (fun paths -> lastPathUpdate (if paths.Length > 0 then Some paths[-1] else None))
        sketch.onStroke (fun stroke -> (if stroke.paths.Length > 1 then receiveStroke stroke); lastStrokeUpdate stroke)
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
                                                    $"/api/GetMessage" + (if tag <> "" then "?name=" + tag else ""),
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
    let operationState, updateState = React.useState NotStarted
    let fileName, update = React.useState None
    Html.div [
        match operationState with
        | InProgress ->
            Html.div "Saving..."
        | Ready (msg:string) ->
            Html.div [
                Html.span [prop.text msg]
                Html.button [prop.text "OK"; prop.onClick (fun _ -> updateState NotStarted)]
                ]
        | NotStarted ->
            match fileName with
            | None ->
                Html.button [
                    prop.text "Save"
                    prop.onClick (thunk1 update (Some ""))
                    ]
            | Some fileName ->
                Html.span [
                    let invalid = fileName |> System.String.IsNullOrWhiteSpace
                    Html.span "Save as: "
                    Html.input [prop.placeholder "E.g. myPicture1"; prop.valueOrDefault fileName; prop.onChange (Some >> update)]
                    let onClick() =
                        promise {
                            updateState InProgress
                            try
                                do! saveCmd fileName
                                update None
                                updateState (Ready $"Saved '{fileName}'")
                            with _err ->
                                updateState NotStarted
                        }
                    Html.button [prop.disabled invalid; prop.text "OK"; if not invalid then prop.onClick (fun _ -> onClick() |> Promise.start)]
                    Html.button [prop.text "Cancel"; prop.onClick (thunk1 update None)]
                    ]
        ]