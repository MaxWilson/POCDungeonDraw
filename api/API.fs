namespace Company.Function

open System
open System.IO
open Microsoft.AspNetCore.Mvc
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.AspNetCore.Http
open Thoth.Json.Net
open Microsoft.Extensions.Logging
open System.Net.Http
open System.Net

module GetMessage =
    open Azure.Security.KeyVault.Secrets
    open Azure.Identity
    open Microsoft.Azure.WebJobs.Extensions.Http
    open Microsoft.AspNetCore.Routing
    open System.Threading.Tasks

    // Define a nullable container to deserialize into.
    type NameContainer = { Name: string }

    // For convenience, it's better to have a central place for the literal.
    [<Literal>]
    let Name = "name"

    type Point = { x: float; y: float }
    type Stroke = { paths: Point array }
    type GraphicElement =
        | Stroke of Stroke * color: string
        | Text of string * Point * color: string
    type SavedPicture = { id: string; tag: string; owner: string; payload: GraphicElement array }

    let toJsonResponse data =
        new HttpResponseMessage(
            HttpStatusCode.OK,
            Content = new StringContent(Encode.Auto.toString data))


    [<FunctionName("ReadData")>]
    let ReadData ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", Route = "ReadData/{id}")>] req: HttpRequest) (log: ILogger)
        ([<CosmosDB(
            databaseName = "%Database%",
            containerName = "%Container%",
            Connection = "CosmosDbConnectionString",
            SqlQuery ="SELECT * FROM c WHERE c.tag={id} ORDER BY c._ts")>] data: SavedPicture seq)
        =
        let id = match req.HttpContext.GetRouteData().Values.TryGetValue "id" with
                    | true, v -> v
                    | _ -> failwith "Id is mandatory"
        log.LogInformation $"Calling ReadData/{id}"

        match req with
        | Auth.Identity ident ->
            // own stuff comes first, then public domain
            (data |> Seq.filter (fun d -> d.owner = ident.UserDetails || d.owner = "publicDomain") |> Seq.sortByDescending (fun d -> d.owner = ident.UserDetails) |> Array.ofSeq |> toJsonResponse)
        | _ ->
            (data |> Seq.filter (fun d -> d.owner = "publicDomain") |> Array.ofSeq |> toJsonResponse)
        |> Task.FromResult


    [<FunctionName("WriteData")>]
    let WriteData ([<HttpTrigger(AuthorizationLevel.Anonymous, "post")>] req: HttpRequest) (log: ILogger)
        ([<CosmosDB(
            databaseName = "%Database%",
            containerName = "%Container%",
            CreateIfNotExists = true,
            PartitionKey = "/id",
            Connection = "CosmosDbConnectionString")>] output: IAsyncCollector<SavedPicture> )
        =
        task {
            let isPublicDomain = req.Query["publicDomain"] |> bool.TryParse |> function true, v -> v | _ -> false

            let! requestBody = ((new StreamReader(req.Body)).ReadToEndAsync())
            log.LogInformation $"About to deserialize '{requestBody}'"
            match Decode.Auto.fromString<SavedPicture>(requestBody) with
            | Ok data ->
                let owner, recordId =
                    match req with
                    | Auth.Identity ident when not isPublicDomain -> ident.UserDetails, $"{ident.UserDetails}-{data.tag}"
                    | _ -> "publicDomain", $"anonymous-{data.tag}"

                // allow dedication to public domain: anyone can edit or retrieve
                let data = { data with owner = owner; id = recordId }

                log.LogInformation $"Got tag = '{data.id}'"
                log.LogInformation $"Writing {data.id} to CosmosDB"

                // notice that we're taking Thoth JSON as input and effectively outputting
                // another form of JSON as output via CosmosDB IAsyncCollector. Therefore
                // we don't need or want Thoth guarantees on existence of id, for example.
                try
                    do! output.AddAsync(data)
                with err ->
                    log.LogError $"Could not save because '{err.ToString()}'"
                    return raise err
            | Error err ->
                return $"Could not deserialize JSON because '{err}'" |> InvalidOperationException |> raise
        }


    [<FunctionName("GetMessage")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = null)>] req: HttpRequest)
        (log: ILogger)
        =
        async {
            log.LogInformation("F# HTTP trigger function processed a request.")

            let nameOpt =
                if req.Query.ContainsKey(Name) then
                    Some(req.Query.[Name].[0])
                else
                    None

            use stream = new StreamReader(req.Body)
            let! reqBody = stream.ReadToEndAsync() |> Async.AwaitTask
            let data =
                Decode.Auto.fromString<NameContainer>(reqBody)

            let name =
                match nameOpt with
                    | Some n -> n
                    | None ->
                        match data with
                        | Error _ ->
                            match req with
                            | Auth.Identity ident -> ident.UserDetails
                            | _ -> "stranger"
                        | Ok nc -> nc.Name

            let responseMessage =
                if (String.IsNullOrWhiteSpace(name)) then
                    "This HTTP triggered function executed successfully. Pass a name in the query string or in the request body for a personalized response."
                else
                    "Hello, "
                    + name
                    + "! This HTTP triggered function executed successfully."
                    + System.Guid.NewGuid().ToString()

            return OkObjectResult(responseMessage) :> IActionResult
        }
        |> Async.StartAsTask
