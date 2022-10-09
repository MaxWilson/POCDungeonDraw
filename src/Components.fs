module Components
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