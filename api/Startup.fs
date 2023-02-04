module Startup

open Microsoft.Azure.Functions.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection
open Microsoft.Azure.WebJobs.Extensions.CosmosDB
open Bindings

type Startup() =
    inherit FunctionsStartup()
    override this.ConfigureAppConfiguration(builder: IFunctionsConfigurationBuilder) =
        ()
    override this.Configure(builder: IFunctionsHostBuilder) =
        builder.Services.AddSingleton<ICosmosDBSerializerFactory, ThothCosmosDbSerializerFactory>()
            |> ignore
        ()

[<assembly: FunctionsStartup(typeof<Startup>)>]
do ()