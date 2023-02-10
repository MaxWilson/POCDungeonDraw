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
open Fable.Core

[<ReactComponent>]
let SketchPad0 strokeColor receiveStroke =
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

let toReactElement (element: JSX.Element): ReactElement = unbox element

[<ReactComponent>]
let SketchPad strokeColor receiveStroke =
    JSX.jsx $"""<div class="stacked">
        <button>Hello there!</button>
        <button>Goodbyte!</button>
        <button>I don't know why you say goodbye</button>
        <button>I say</button>
        <button>Hello!</button>
    </div>
    """
    |> toReactElement

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