﻿namespace Foosball

module Team = 
    type Player = 
        { name : string }
    
    type t = 
        { offense : Player
          defense : Player }

module Game = 
    type t = 
        | NotConfigured
        | Playing of teams : string array
        | Goal of team : Team.t
        | ThrowIn of team : Team.t
        | Substitution
        | Ended of result : string

//printfn "%A" Arduino.Command.Start
module App = 
    open Newtonsoft.Json

    [<EntryPoint>]
    let main argv = 
        let q = new System.Timers.Timer(3.)
        let config = (Model.GameConfig.TimeLimited(Model.Duration.FromSeconds 80.))
        let signalr = Signalr.Server "http://localhost:8070"
        use result = GameLogic.start (Model.Team.White) config (fun g-> signalr.Send (JsonConvert.SerializeObject(g,Formatting.Indented)))
        stdin.ReadLine() |> ignore
        let connector = ArduinoSerialConnector.connect "COM3" stdin.ReadLine
        connector.WriteLine "start"
        printfn "exiting application"
        0 // return an integer exit code
