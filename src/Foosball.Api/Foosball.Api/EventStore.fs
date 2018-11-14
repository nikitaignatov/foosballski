namespace Foosball

module EventStore = 
    open System
    open Model
    open GameLogic
    open FSharp.Control.Reactive
    
    let agg = 
        { zero = Model.State.zero
          apply = apply
          exec = handle }
    
    let path id = Utils.pathGetOrCreate "games" (sprintf "game-%s.json" (id.ToString())) []
    
    let load (id : Model.Id) : Model.GameEvent list = 
        path id
        |> IO.File.ReadAllText
        |> Utils.deserialize<GameEvent list>
    
    let commit (id : Model.Id, v : int) (e : Model.GameEvent list) = 
        match e with
        | [] -> ()
        | _ -> 
            let path = path id
            let events = load id
            let json = Utils.serialize (events @ e)
            IO.File.WriteAllText(path, json)
            GameSubscription<Model.GameEvent list>.Update e
            ()
    
    let handler = makeHandler agg (load, commit)
    
    type private GameMessage = 
        | Execute of (Id * GameCommand)
    
    type GameAgent(cardreader, signalr : Signalr.Server) = 
        
        let stream (id : Id) = 
            (Observable.interval (Duration.FromSeconds 1.) |> Observable.map (fun _ -> Tick))
            |> Observable.merge (cardreader)
            |> Observable.merge (Sensor.stream "A0")
            |> Observable.merge (Sensor.stream "A1")
            |> Observable.merge (Sensor.stream "A2")
            |> Observable.merge (Sensor.stream "A3")
            |> Observable.map (fun s -> id, s)
        
        let rec loop (state : (Id * IDisposable) option) (inbox : MailboxProcessor<GameMessage>) = 
            let loop x = loop x inbox
            async { 
                try 
                    let! msg = inbox.Receive()
                    printfn "Message: %A" msg
                    let result = 
                        match state, msg with
                        | (_, Execute(_, cmd)) & (None, Execute(_, NewGame id)) -> 
                            handler (id, 0) cmd
                            let sub = stream id |> Observable.subscribe (fun (_, x) -> inbox.Post(Execute(id, x)) |> ignore)
                            signalr.Send(load id |> List.fold apply State.Zero)
                            Some(id, sub)
                        | Some(id, subscription), Execute(cid, EndGame) when cid = id -> 
                            subscription.Dispose()
                            handler (id, 0) (EndGame)
                            signalr.Send(load id |> List.fold apply State.Zero)
                            None
                        | Some(id, subscription), Execute(cid, cmd) when cid = id -> 
                            handler (id, 0) cmd
                            signalr.Send(load id |> List.fold apply State.Zero)
                            Some(id, subscription)
                        | _ -> 
                            (printfn "ERROR executing: %A with state: %A" msg state)
                            state
                    return! loop (result)
                with e -> printfn "Error: %A" e
            }
        
        let agent = MailboxProcessor<GameMessage>.Start(loop (None))
        member this.Execute(id : Id, msg : GameCommand) = agent.Post(Execute(id, msg))
    
    let Game cardreader = new GameAgent(cardreader)
