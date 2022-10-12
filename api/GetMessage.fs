namespace Company.Function

open System
open System.IO
open Microsoft.AspNetCore.Mvc
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open Microsoft.Extensions.Logging

module GetMessage =
    open Azure.Security.KeyVault.Secrets
    open Azure.Identity
    open Microsoft.Azure.WebJobs.Extensions.Http

    // Define a nullable container to deserialize into.
    [<AllowNullLiteral>]
    type NameContainer() =
        member val Name = "" with get, set

    // For convenience, it's better to have a central place for the literal.
    [<Literal>]
    let Name = "name"

    type TestData = { id: string; tag: string; owner: string; payload: string list }

    [<FunctionName("psst")>]
    let Psst ([<HttpTrigger(AuthorizationLevel.Function, "get")>] req: HttpRequest) (log: ILogger)
        =
        task {
            let secretClient = new SecretClient(
                new Uri("https://shiningsword.vault.azure.net"),
                new DefaultAzureCredential())
            let! secret = secretClient.GetSecretAsync("Test123")
            return ("Yo dude? I'm still here." + secret.Value.Value)
        }

    [<FunctionName("ReadData")>]
    let ReadData ([<HttpTrigger(AuthorizationLevel.Function, "get", Route = "ReadData/{id}")>] req: HttpRequest) (log: ILogger)
        ([<CosmosDB(
            databaseName = "ToDoList",
            collectionName = "Items",
            ConnectionStringSetting = "CosmosDbConnectionString",
            SqlQuery ="SELECT * FROM c WHERE c.tag={id} ORDER BY c._ts")>] data: TestData seq)
        =
        match req with
        | Auth.Identity ident ->
            task {
                return (data |> Seq.filter (fun d -> d.owner = ident.UserDetails) |> Array.ofSeq)
            }
        | _ -> task { return Array.empty }

    [<FunctionName("WriteData")>]
    let WriteData ([<HttpTrigger(AuthorizationLevel.Function, "post")>] req: HttpRequest) (log: ILogger)
        ([<CosmosDB(
            databaseName = "ToDoList",
            collectionName = "Items",
            ConnectionStringSetting = "CosmosDbConnectionString")>] output: IAsyncCollector<TestData> )
        =
        task {
            match req with
            | Auth.Identity ident ->
                let! requestBody = ((new StreamReader(req.Body)).ReadToEndAsync());
                log.LogInformation $"About to deserialize '{requestBody}'"
                let data = JsonConvert.DeserializeObject<TestData>(requestBody);
                let data = { data with owner = ident.UserDetails; id = $"{ident.UserDetails}/{data.tag}" }
                log.LogInformation $"Got tag = '{data.id}'"
                log.LogInformation $"Writing {data.id} to CosmosDB"
                do! output.AddAsync(data)
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
                JsonConvert.DeserializeObject<NameContainer>(reqBody)

            let name =
                match req with
                | Auth.Identity ident -> ident.UserDetails
                | _ ->
                    match nameOpt with
                    | Some n -> n
                    | None ->
                        match data with
                        | null -> ""
                        | nc -> nc.Name

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
