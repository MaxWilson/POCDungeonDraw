module SQL

open Thoth.Json.Net
open Dapper
open Dapper.FSharp
open Dapper.FSharp.MSSQL
open System.Data.SqlClient

type SavedPicture = { tag: string; owner: string; json: string }

type SQLConvert() =
    static member private decode<'t> (s: string) =
        match (Decode.Auto.fromString<'t> s) with
        | Ok v -> v
        | Error err -> failwithf "Error decoding %s" err
    static member toSQL (p: DataContracts.SavedPicture): SavedPicture =
        { tag = p.tag; owner = p.owner; json = Encode.Auto.toString p.payload }
    static member fromSQL (p: SavedPicture): DataContracts.SavedPicture =
        { tag = p.tag; owner = (if System.String.IsNullOrWhiteSpace p.owner then "publicDomain" else p.owner); payload = SQLConvert.decode p.json }
open type SQLConvert
open System.Threading.Tasks
open System.Net.Http
open System.Net

let pictures = table'<SavedPicture> "SavedPicture" |> inSchema "sketch"

module Task =
    let map f t = task {
        let! x = t
        return f x
    }
    let ignore t = t |> map ignore

let toJsonResponse data =
    new HttpResponseMessage(
        HttpStatusCode.OK,
        Content = new StringContent(Encode.Auto.toString data))

let read(conn: SqlConnection, tag, userName) =
    task {
        do! conn.OpenAsync()
        let! result =
            conn.QueryAsync<SavedPicture>(
                "select tag, owner, json from sketch.SavedPicture where tag = @tag and (owner is not distinct from @owner) order by owner desc",
                {| tag = tag; owner = userName |}
                )
        return result |> Seq.map fromSQL |> Array.ofSeq |> toJsonResponse
    }

let upsert(conn:SqlConnection, input) =
    task {
        do! conn.OpenAsync()
        let! rowsAffected = conn.ExecuteAsync("sketch.sp_savePicture", (SQLConvert.toSQL input), commandType = System.Data.CommandType.StoredProcedure)
        if rowsAffected = 0 then failwith "Unable to write for some reason"
        return ()
    }
