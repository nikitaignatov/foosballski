namespace Foosball

module GameDto = 
    open Model
    
    type t = 
        { status : (TeamColor * int) * (TeamColor * int)
          events : (Model.GameEvent * string list) list }
    
    let goals_within_seconds = 
        function 
        | Pattern.Goal.GoalWithinSeconds 1. x -> "Goal within 1 second" |> Some
        | Pattern.Goal.GoalWithinSeconds 2. x -> "Goal within 2 second" |> Some
        | Pattern.Goal.GoalWithinSeconds 4. x -> "Goal within 4 second" |> Some
        | Pattern.Goal.GoalWithinSeconds 8. x -> "Goal within 8 second" |> Some
        | _ -> None
    
    let speed = 
        function 
        | Achievement.HowCouldYouMissThat x -> sprintf "How could they miss that: %f km/h" (x * 3.6m) |> Some
        | Achievement.SpeedOfLight x -> sprintf "Speed of light: %f km/h" (x * 3.6m) |> Some
        | Achievement.MachThree x -> sprintf "Mach 3: %f km/h" (x * 3.6m) |> Some
        | Achievement.MachTwo x -> sprintf "Mach 2: %f km/h" (x * 3.6m) |> Some
        | Achievement.MachOne x -> sprintf "Mach 1: %f km/h" (x * 3.6m) |> Some
        | _ -> None
    
    let toDto (input : Model.GameEvent list) = 
        { events = 
              input
              |> List.rev
              |> List.fold (fun (state, result) v -> (v :: state), ((v, [] |> List.choose id) :: result)) ([], [])
              |> snd
          status = input |> List.fold (Pattern.GameControl.``|GameStatus2|``) (((TeamColor.Black, 0), (TeamColor.White, 0))) }

//printfn "%A" Arduino.Command.Start
module App = 
    open Model
    open PCSC.Monitoring
    open Nfc.Reader
    
    [<EntryPoint>]
    let main argv = 
        let settings = Settings.current
        let signalr = Signalr.Server(settings.Load().signalr)
        System.Diagnostics.Process.Start(settings.Load().app) |> ignore
        let cardToPlayer settings card = 
            let player = 
                match (settings, card) with
                | Settings.PlayerFromCard player -> player.player
                | _ -> Player.zero
            { card = Card card
              player = player
              goals = [] }
            |> Register
            |> Some
        try 
            let obs2, (monitor : SCardMonitor) = CardReader.execute()
            
            let regs = 
                obs2
                |> Observable.map (fun x -> 
                       match x with
                       | Nfc.Reader.CardReader.Inserted x -> cardToPlayer (settings.Load()) x
                       | _ -> None)
                |> Observable.choose id
            
            let agent = new EventStore.GameAgent(regs,signalr)
            
            let subscription = 
                Signalr.t<GameCommand>.Observable
                |> Observable.map (fun (a, b, c) -> a, c)
                |> Observable.subscribe agent.Execute
            
            let connector, init = ArduinoSerialConnector.connect (settings.Load().sensor) stdin.ReadLine
            init()
            connector.start()
            stdout.WriteLine("start")
            printfn "exiting application"
            monitor.Dispose()
            subscription.Dispose()
            connector.close()
        with e -> printfn "%A" e
        stdin.ReadLine()
        0 // return an integer exit code
