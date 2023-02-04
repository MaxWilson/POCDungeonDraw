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

    // Define a nullable container to deserialize into.
    [<AllowNullLiteral>]
    type NameContainer() =
        member val Name = "" with get, set

    // For convenience, it's better to have a central place for the literal.
    [<Literal>]
    let Name = "name"

    type Point = { x: float; y: float }
    type Stroke = { paths: Point array }
    type GraphicElement =
        | Stroke of Stroke * color: string
        | Text of string * Point * color: string
    type SavedPicture = { id: string; tag: string; owner: string; payload: GraphicElement array }

    [<FunctionName("psst")>]
    let Psst ([<HttpTrigger(AuthorizationLevel.Function, "get")>] req: HttpRequest) (log: ILogger)
        =
        task {
            let secretClient = new SecretClient(
                new Uri("https://shiningsword.vault.azure.net"),
                new DefaultAzureCredential())
            let! secret = secretClient.GetSecretAsync("Test123")
            return ("Yo dude? I'm still here." + secret.Value.Value.Length.ToString())
        }

    let toJsonResponse data =
        new HttpResponseMessage(
            HttpStatusCode.OK, 
            Content = new StringContent(Encode.Auto.toString data))


    [<FunctionName("ReadData")>]
    let ReadData ([<HttpTrigger(AuthorizationLevel.Function, "get", Route = "ReadData/{id}")>] req: HttpRequest) (log: ILogger)
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
            task {
                return (data |> Seq.filter (fun d -> d.owner = ident.UserDetails || d.owner = "publicDomain") |> Array.ofSeq |> toJsonResponse)
            }
        | _ -> task { return Array.empty |> toJsonResponse }

    [<FunctionName("WriteData")>]
    let WriteData ([<HttpTrigger(AuthorizationLevel.Function, "post")>] req: HttpRequest) (log: ILogger)
        ([<CosmosDB(
            databaseName = "%Database%",
            containerName = "%Container%",
            CreateIfNotExists = true,
            PartitionKey = "/id",
            Connection = "CosmosDbConnectionString")>] output: IAsyncCollector<SavedPicture> )
        =
        task {
            let isPublicDomain = req.Query["publicDomain"] |> bool.TryParse |> function true, v -> v | _ -> false
            match req with
            | Auth.Identity ident ->
                let! requestBody = ((new StreamReader(req.Body)).ReadToEndAsync());
                log.LogInformation $"About to deserialize '{requestBody}'"
                match Decode.Auto.fromString<SavedPicture>(requestBody) with
                | Error err -> $"Could not deserialize JSON because '{err}'" |> InvalidOperationException |> raise
                | Ok data ->
                    // allow dedication to public domain: anyone can edit or retrieve
                    let owner = if isPublicDomain then "publicDomain" else ident.UserDetails
                    let data = { data with owner = owner; id = $"{ident.UserDetails}-{data.tag}" }
                    log.LogInformation $"Got tag = '{data.id}'"
                    log.LogInformation $"Writing {data.id} to CosmosDB"

                    // notice that we're taking Thoth JSON as input and effectively outputting
                    // another form of JSON as output via CosmosDB IAsyncCollector. Therefore
                    // we don't need or want Thoth guarantees on existence of id, for example.
                    try
                        do! output.AddAsync(data)
                    with err ->
                        log.LogError $"Could not save because '{err.ToString()}'"
                        raise err
            | _ -> failwith "You must log in first in order to save"
        }


    [<FunctionName("GetMessage")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Function, "get", "post", Route = null)>] req: HttpRequest)
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
                match req with
                | Auth.Identity ident -> ident.UserDetails
                | _ ->
                    match nameOpt with
                    | Some n -> n
                    | None ->
                        match data with
                        | Error _ | Ok null -> ""
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
