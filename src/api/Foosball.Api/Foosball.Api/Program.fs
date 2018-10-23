namespace Foosball

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
    [<EntryPoint>]
    let main argv = 
        let q = new System.Timers.Timer(3.)
        q.Elapsed |> Observable.subscribe (fun _ -> ())
        let connector = ArduinoSerialConnector.connect "COM3" stdin.ReadLine
        printfn "exiting application"
        0 // return an integer exit code
