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
open Azure.Security.KeyVault.Secrets
open Azure.Identity
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.AspNetCore.Routing
open System.Threading.Tasks
open Azure.Messaging.WebPubSub
open Microsoft.Extensions.Configuration
open System.Data.SqlClient

module API =
    open DataContracts
    // Define a nullable container to deserialize into.
    type NameContainer = { Name: string }

    let toJsonResponse data =
        new HttpResponseMessage(
            HttpStatusCode.OK,
            Content = new StringContent(Encode.Auto.toString data))

    [<FunctionName("CreateTokenRequest")>]
    let CreateTokenRequest(
        [<HttpTrigger(AuthorizationLevel.Anonymous, "get", Route = "CreateTokenRequest/{hubName}/{groupName}/")>] req: HttpRequest,
        hubName : string,
        groupName : string,
        log : ILogger) =
        task {
            let clientId =
                match req with
                | Auth.Identity ident -> ident.UserDetails
                | _ -> "anonymous"
            let webPubSubService = new WebPubSubServiceClient(
                Environment.GetEnvironmentVariable("AzPubSubConnectionString"),
                hubName)
            let! clientUri = webPubSubService.GetClientAccessUriAsync(
                TimeSpan.FromMinutes(30),
                clientId,
                [$"webpubsub.joinLeaveGroup.{groupName}"; $"webpubsub.sendToGroup.{groupName}"]
                )

            return new HttpResponseMessage(
                HttpStatusCode.OK,
                Content = new StringContent(clientUri.ToString())
                )
            }

type SQLApi(conn: SqlConnection) =
    [<FunctionName("ReadData")>]
    member _.ReadData ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", Route = "ReadData/{id}")>] req: HttpRequest)
        (log: ILogger)
        =
        let id = match req.HttpContext.GetRouteData().Values.TryGetValue "id" with
                    | true, v -> v.ToString()
                    | _ -> failwith "Id is mandatory"
        log.LogInformation $"Calling ReadData/{id}"
        SQL.read(conn, id, req.HttpContext.User.Identity.Name)

    [<FunctionName("WriteData")>]
    member _.WriteData ([<HttpTrigger(AuthorizationLevel.Anonymous, "post")>] req: HttpRequest)
        (log: ILogger)
        =
        task {
            let isPublicDomain = req.Query["publicDomain"] |> bool.TryParse |> function true, v -> v | _ -> false

            let! requestBody = ((new StreamReader(req.Body)).ReadToEndAsync())
            log.LogInformation $"About to deserialize '{requestBody}'"
            match Decode.Auto.fromString<DataContracts.SavedPicture>(requestBody) with
            | Ok data ->
                let owner =
                    match req with
                    | Auth.Identity ident when not isPublicDomain -> ident.UserDetails
                    | _ -> "" // turns into DB null

                // allow dedication to public domain: anyone can edit or retrieve
                let data = { data with owner = owner }

                log.LogInformation $"Got tag = '{data.tag}'"
                log.LogInformation $"Writing {data.tag} to SQL"

                // notice that we're taking Thoth JSON as input and effectively outputting
                // another form of JSON as output via CosmosDB IAsyncCollector. Therefore
                // we don't need or want Thoth guarantees on existence of id, for example.
                try
                    do! SQL.upsert(conn, data)
                with err ->
                    log.LogError $"Could not save because '{err.ToString()}'"
                    return raise err
            | Error err ->
                return $"Could not deserialize JSON because '{err}'" |> InvalidOperationException |> raise
        }
