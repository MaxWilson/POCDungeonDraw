module Auth
open System
open System.Text
open System.Text.Json
open Microsoft.AspNetCore.Http

// ref https://learn.microsoft.com/en-us/azure/static-web-apps/user-information?tabs=csharp#api-functions
type ClientPrincipal = {
    IdentityProvider: string
    UserId: string
    UserDetails: string
    UserRoles: string seq
}

let (|Identity|_|) (req:HttpRequest) =
    // ref: https://learn.microsoft.com/en-us/azure/static-web-apps/user-information?tabs=csharp#api-functions
    match req.Headers.TryGetValue("x-ms-client-principal") with
    | true, header when header.Count > 0 ->
        try
            header[0]
            |> Convert.FromBase64String
            |> System.Text.Encoding.UTF8.GetString
            |> fun json -> JsonSerializer.Deserialize<ClientPrincipal>(json, JsonSerializerOptions(PropertyNameCaseInsensitive=true))
            |> Some
        with _ -> None
    | _ -> None