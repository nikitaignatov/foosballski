namespace Foosball
module Signalr=

    open Owin
    open FSharp.Interop.Dynamic
    open Microsoft.AspNet.SignalR
    open Microsoft.Owin.Hosting
    open Microsoft.Owin.Cors
    open Microsoft.AspNet.SignalR.Hubs
    
    [<HubName("foosball")>]
    type FoosballHub () =
        inherit Hub ()

        member x.command message =
            printfn "Received an order: %s" message

        member x.execute message =
            printfn "Execute command: %A" message


    type Server (host:string) =
        let startup (a:IAppBuilder) =
            a.UseCors(CorsOptions.AllowAll) |> ignore
            a.MapSignalR() |> ignore
            
        let clients = GlobalHost.ConnectionManager.GetHubContext<FoosballHub>().Clients
        do
            WebApp.Start(host, startup) |> ignore
            printfn "Signalr server running on %s" host
            
        member x.Send message = clients.All?publish message
        member x.Time message = clients.All?time message
        member x.Players message = clients.All?players message