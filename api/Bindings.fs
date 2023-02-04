module Bindings

open Microsoft.Azure.Cosmos
open Microsoft.Azure.WebJobs.Extensions.CosmosDB
open Thoth.Json.Net
open type System.Text.Encoding
open System.IO

exception ThothJsonInputException of msg: string

type ThothCosmosDbSerializerFactory() =
    interface ICosmosDBSerializerFactory with
        member this.CreateSerializer(): CosmosSerializer = 
            {
                new CosmosSerializer() with
                    override this.ToStream(input) =
                        Encode.Auto.toString(input) |> UTF8.GetBytes 
                        |> fun x -> new MemoryStream(x) :> Stream
                    override this.FromStream<'t>(input) =
                        use txt = new StreamReader(input)
                        match Decode.Auto.fromString<'t>(txt.ReadToEnd()) with
                        | Ok v -> v
                        | Error err -> err |> ThothJsonInputException |> raise
                }
    