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
    // Define a nullable container to deserialize into.
    [<AllowNullLiteral>]
    type NameContainer() =
        member val Name = "" with get, set

    // For convenience, it's better to have a central place for the literal.
    [<Literal>]
    let Name = "name"

    [<FunctionName("psst")>]
    let psst ([<HttpTrigger(AuthorizationLevel.Function, "get")>] req: HttpRequest) (log: ILogger) =
        async {
            let secretClient = new SecretClient(
                new Uri("https://identitytest.vault.azure.net"),
                new DefaultAzureCredential());
        var secret = await secretClient.GetSecretAsync("<SecretName>");
            return "Yo dude? I'm still here."
        }
        |> Async.StartAsTask


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
