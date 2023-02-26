module Startup

open Microsoft.Azure.Functions.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection
open Microsoft.Azure.WebJobs.Extensions.CosmosDB
open Bindings
open Dapper.FSharp.MSSQL
open Microsoft.Extensions.Configuration
open System
open System.Data.SqlClient

type Startup() =
    inherit FunctionsStartup()
    let config : IConfiguration =
        (new ConfigurationBuilder())
            .SetBasePath(Environment.CurrentDirectory)
            .AddJsonFile("appsettings.json", true)
            .AddJsonFile("local.settings.json", true)
            .AddEnvironmentVariables()
            .Build();

    override this.ConfigureAppConfiguration(builder: IFunctionsConfigurationBuilder) =
        ()
    override this.Configure(builder: IFunctionsHostBuilder) =
        OptionTypes.register()
        builder.Services
            .AddScoped<SqlConnection>(fun _ -> new SqlConnection (config["SQLConnectionString"]))
            |> ignore
        ()

[<assembly: FunctionsStartup(typeof<Startup>)>]
do ()