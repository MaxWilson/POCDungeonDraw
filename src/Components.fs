﻿module Components
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
open Browser

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

type LineData = { color: string; points: (float * float) list }

[<ReactComponent>]
let SketchPad strokeColor receiveStroke =
    let lines, setLines = React.useState []
    let brush, setBrush = React.useState "black"
    let isDrawing = React.useRef false
    let handleMouseDown e =
        isDrawing.current <- true
        let pos = e?target?getStage()?getPointerPosition()
        setLines([{color = brush; points = [pos?x, pos?y]}]@lines) // start a new line with only one point
    let handleMouseMove e =
        if isDrawing.current then
            let stage = e?target?getStage()
            let point = stage?getPointerPosition()
            match lines with
            | current::priors ->
                let current' = { current with points = current.points@[point?x, point?y]}
                current'::priors |> setLines
            | [] -> shouldntHappen()
    let handleMouseUp e =
        isDrawing.current <- false
    let makeLine ix (line: LineData) =
        JSX.jsx $"""
            <Line
                  key={ix}
                  points={line.points |> List.collect(fun(x,y) -> [x;y]) |> Array.ofList}
                  stroke={line.color}
                  strokeWidth={5}
                  tension={0.5}
                  lineCap="round"
                  lineJoin="round"
                  globalCompositeOperation=
                  {
                    if line.color = "eraser" then "destination-out" else "source-over"
                  }
                />
        """
    JSX.jsx $"""
    import {{ Stage, Layer, Line, Text }} from 'react-konva';
    <div>
      <Stage
        width={window.innerWidth}
        height={window.innerHeight - 200.}
        onMouseDown={handleMouseDown}
        onMousemove={handleMouseMove}
        onMouseup={handleMouseUp}
      >
        <Layer>
          <Text text="Just start drawing" x={5} y={30} />
          {lines |> List.mapi makeLine |> Array.ofList}
        </Layer>
      </Stage>
      <select
        value={brush}
        onChange={fun event ->
          setBrush(event?target?value)
        }
      >
        <option value="pen">Pen</option>
        <option value="eraser">Eraser</option>
      </select>
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