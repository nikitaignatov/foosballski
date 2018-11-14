namespace Foosball

module Signalr = 
    open Owin
    open FSharp.Interop.Dynamic
    open Microsoft.AspNet.SignalR
    open Microsoft.Owin.Hosting
    open Microsoft.Owin.Cors
    open Microsoft.AspNet.SignalR.Hubs
    open Newtonsoft.Json
    open Model
    
    type t<'a>() = 
        static let event = (new Event<Id * Time * 'a>())
        static member Observable = event.Publish |> Observable.map id
        
        static member Publish id ev = 
            try 
                event.Trigger(id, Time.Now, ev)
            with e -> 
                printfn "Event: %A" ev
                printfn "error: %s" e.Message
            ()
        
        static member Update id msg = 
            try 
                let ev = JsonConvert.DeserializeObject<'a>(msg)
                t<'a>.Publish (Id.Parse id) ev
            with e -> 
                printfn "message: %s %s" id msg
                printfn "error: %s" e.Message
            ()
    
    [<HubName("foosball")>]
    type FoosballHub() = 
        inherit Hub()
        member x.execute id message = 
            printfn "Received an order: %s %s" id message
            t<GameCommand>.Update id message
    
    type Server(host : string) = 
        
        let startup (a : IAppBuilder) = 
            a.UseCors(CorsOptions.AllowAll) |> ignore
            let hubConfiguration = new HubConfiguration()
            hubConfiguration.EnableDetailedErrors <- true
            a.MapSignalR("/signalr", hubConfiguration) |> ignore
        
        let clients = GlobalHost.ConnectionManager.GetHubContext<FoosballHub>().Clients
        
        do 
            WebApp.Start(host, startup) |> ignore
            printfn "Signalr server running on %s" host
        
        member x.Send message = clients.All?publish message
        member x.Time message = clients.All?time message
        member x.Players message = clients.All?players message
