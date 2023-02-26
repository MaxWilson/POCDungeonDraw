module DataContracts

type Point = { x: float; y: float }
type Stroke = { points: float array } // flattened coordinate list, e.g. [x1;y1;x2;y2] and so on. Perf optimization relative to flattening with every render.
type GraphicElement =
    | Stroke of Stroke * color: string * brushSize: string
    | Text of string * Point * color: string
type SavedPicture = { tag: string; owner: string; payload: GraphicElement array }
