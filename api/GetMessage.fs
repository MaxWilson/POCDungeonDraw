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

    type TestData = { tag: string }

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
            SqlQuery ="SELECT * FROM c WHERE c.id={id} ORDER BY c._ts")>] productItem: TestData seq)
        =
        task {
            return (productItem |> Array.ofSeq)
        }

    [<FunctionName("WriteData")>]
    let WriteData ([<HttpTrigger(AuthorizationLevel.Function, "post")>] req: HttpRequest) (log: ILogger)
        ([<CosmosDB(
            databaseName = "ToDoList",
            collectionName = "Items",
            ConnectionStringSetting = "CosmosDbConnectionString")>] output: IAsyncCollector<TestData> )
        =
        task {
            let! requestBody = ((new StreamReader(req.Body)).ReadToEndAsync());
            log.LogInformation $"About to deserialize '{requestBody}'"
            let data = JsonConvert.DeserializeObject<TestData>(requestBody);
            log.LogInformation $"Got tag = '{data.tag}'"
            log.LogInformation $"Writing {id} to CosmosDB"
            do! output.AddAsync(data)
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
