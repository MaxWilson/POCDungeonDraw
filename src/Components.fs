module Components
open Feliz
open Fetch
open Thoth.Fetch

type 't Deferred = NotStarted | InProgress | Ready of 't
type Point = { x: float; y: float }
type Stroke = { points: float array } // flattened coordinate list, e.g. [x1;y1;x2;y2] and so on. Perf optimization relative to flattening with every render.
type GraphicElement =
    | Stroke of Stroke * color: string
    | Text of string * Point * color: string

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

let toReactElement (element: JSX.Element): ReactElement = unbox element

type LineData = { color: string; points: (float*float) list }

[<ReactComponent>]
let SketchPad (strokeColor:string) (existing: GraphicElement list) receiveStroke =
    let (currentLine: LineData option), setCurrentLine = React.useState None
    let isDrawing = React.useRef false
    let handleMouseDown e =
        isDrawing.current <- true
        let pos = e?target?getStage()?getPointerPosition()
        setCurrentLine(Some {color = strokeColor; points = [pos?x, pos?y]}) // start a new line with only one point
    let handleMouseMove e =
        if isDrawing.current then
            let stage = e?target?getStage()
            let point = stage?getPointerPosition()
            match currentLine with
            | Some current ->
                let x: float = point?x
                let y: float = point?y
                let current' = { current with points = (x,y)::current.points }
                current' |> Some |> setCurrentLine
            | None -> ()
    let handleMouseUp e =
        isDrawing.current <- false
        match currentLine with
        | Some line ->
            let points1 = line.points |> Array.ofList |> Array.rev
            // turn it into a graphic element and send it
            receiveStroke ({ points = line.points |> List.rev |> List.collect (fun (x,y) -> [x;y]) |> Array.ofList })
        | None -> ()
    let renderGraphicElement (ix:int) =
        function
        | Stroke(stroke,color) ->
            JSX.jsx $"""
                <Line
                        key={ix}
                        points={stroke.points}
                        stroke={color}
                        strokeWidth={5}
                        tension={0.5}
                        lineCap="round"
                        lineJoin="round"
                        globalCompositeOperation=
                        {
                            if color = "Eraser" then "destination-out" else "source-over"
                        }
                    />
            """
        | Text(txt, point, color) ->
            JSX.jsx $"""<Text text={txt} x={point.x} y={point.y} stroke={color} fill={color} fontSize={40}/>
                """
    let makeLineFromCurrent (line: LineData) =
        JSX.jsx $"""
            <Line
                  key="current"
                  points={line.points |> List.collect(fun (x,y) -> [x;y]) |> Array.ofList}
                  stroke={line.color}
                  strokeWidth={5}
                  tension={0.5}
                  lineCap="round"
                  lineJoin="round"
                  globalCompositeOperation=
                  {
                    if line.color = "Eraser" then "destination-out" else "source-over"
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
          {existing |> List.mapi renderGraphicElement |> Array.ofList}
          {[match currentLine with None -> () | Some line -> makeLineFromCurrent line]}
        </Layer>
      </Stage>
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