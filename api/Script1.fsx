#r "nuget:Dapper.Fsharp"
#r "nuget:System.Data.SqlClient"
#r "nuget:Thoth.Json.NET"
open Dapper
open Dapper.FSharp
open Dapper.FSharp.MSSQL
open System.Data.SqlClient
open Thoth.Json.Net
OptionTypes.register()

type Point = { x: float; y: float }
type Stroke = { points: float array } // flattened coordinate list, e.g. [x1;y1;x2;y2] and so on. Perf optimization relative to flattening with every render.
type GraphicElement =
    | Stroke of Stroke * color: string * brushSize: string
    | Text of string * Point * color: string

type SavedPicture = { tag: string; owner: string; payload: GraphicElement array }

module SQL =
    type SavedPicture = { tag: string; owner: string; json: string }

type SQLConvert() =
    static member private decode<'t> (s: string) =
        match (Decode.Auto.fromString<'t> s) with
        | Ok v -> v
        | Error err -> failwithf "Error decoding %s" err
    static member toSQL (p: SavedPicture): SQL.SavedPicture =
        { tag = p.tag; owner = p.owner; json = Encode.Auto.toString p.payload }
    static member fromSQL (p: SQL.SavedPicture): SavedPicture =
        { tag = p.tag; owner = p.owner; payload = SQLConvert.decode p.json }
open type SQLConvert

let pictures = table'<SQL.SavedPicture> "SavedPicture" |> inSchema "sketch"

//create a connection
let conn = new SqlConnection("Server=.;Database=SQLDB;Trusted_Connection=True;")
conn.Open()
let result (t: _ System.Threading.Tasks.Task) = t.Result

//create an fsharp.dapper example object
let example = { tag = "example1"; owner = ""; payload = [| GraphicElement.Stroke({ points = [| 1.0; 2.0; 3.0; 4.0 |] }, "red", "Huge") |] }

insert {
    into pictures
    value (SQLConvert.toSQL example)
} |> conn.InsertAsync |> result

let rowsAffected = conn.Execute("sketch.sp_savePicture", (SQLConvert.toSQL example), commandType = System.Data.CommandType.StoredProcedure)

(select {
    for p in pictures do
    where (p.tag = "example1")
} |> conn.SelectAsync<SQL.SavedPicture>) |> result |> Seq.map fromSQL

