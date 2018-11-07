namespace Foosball

module GameDto = 
    open Model
    
    type t = 
        { status : (Team * int) * (Team * int)
          events : (Model.GameCommand * string list) list }
    
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
    
    let toDto (input : Model.GameCommand list) = 
        { events = 
              input
              |> List.rev
              |> List.fold (fun (state, result) v -> 
                     (v :: state), 
                     ((v, 
                       [ goals_within_seconds (v :: state)
                         speed (v :: state) ]
                       |> List.choose id)
                      :: result)) ([], [])
              |> snd
          status = input |> List.fold (Pattern.GameControl.``|GameStatus|``) (((Team.black, 0), (Team.white, 0))) }

//printfn "%A" Arduino.Command.Start
module App = 
    open Newtonsoft.Json
    open Model
    open PCSC.Monitoring
    open Nfc.Reader
    
    [<EntryPoint>]
    let main argv = 
        let settings = Settings.current
        let s = settings.Load()
        let q = new System.Timers.Timer(3.)
        let config = (Model.GameConfig.GameTimeLimited(Model.Duration.FromSeconds 120.))
        let signalr = Signalr.Server(settings.Load().signalr)
        System.Diagnostics.Process.Start(settings.Load().app) |> ignore
        let publishGame (g : GameCommand list) = 
            signalr.Send(JsonConvert.SerializeObject(GameDto.toDto g, Formatting.Indented))
            ()
        
        let publishTime (g : string) = 
            signalr.Time(JsonConvert.SerializeObject(g, Formatting.Indented))
            ()
        
        let publishPlayers (g : Registration list) = 
            signalr.Players(JsonConvert.SerializeObject(g, Formatting.Indented))
            ()
        
        let cardToPlayer settings card = 
            match (settings, card) with
            | Settings.PlayerFromCard player -> Register({card=Card card;player= player.player;goals=[]}) |> Some
            | _ -> Register({card=Card card;player= Player.zero;goals=[]}) |> Some
        try 
            let obs2, (monitor : SCardMonitor) = CardReader.execute()
        
            let regs = 
                obs2
                |> Observable.map (fun x -> 
                       match x with
                       | Nfc.Reader.CardReader.Inserted x -> cardToPlayer (settings.Load()) x
                       | _ -> None)
                |> Observable.choose id
        
            use result = GameLogic.start (regs) (Model.Team.white) config publishGame publishTime publishPlayers
            let connector, init = ArduinoSerialConnector.connect (settings.Load().sensor) stdin.ReadLine
            init()
            connector.start()
            stdout.WriteLine("start")
            printfn "exiting application"
            monitor.Dispose()
            result.Dispose()
            connector.close()
        with e -> printfn "%A" e
        stdin.ReadLine()
        0 // return an integer exit code
