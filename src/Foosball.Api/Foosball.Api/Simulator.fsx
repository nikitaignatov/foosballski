// load packages
#r @"..\packages\FSharp.Data.3.0.0\lib\net45\FSharp.Data.dll"
#r @"..\packages\System.Reactive.4.0.0\lib\net46\System.Reactive.dll"
#r @"..\packages\Newtonsoft.Json.11.0.2\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\FSharp.Control.Reactive.4.1.0\lib\net46\FSharp.Control.Reactive.dll"
// load files
#load "Arduino.fs"
#load "ArduinoSerialConnector.fs"
#load "Model.fs"
#load "Sensor.fs"
#load "Achievement.fs"
#load "ConsolePrinter.fs"
#load "GameLogic.fs"
#load "Signalr.fs"

open System
open FSharp.Data
open FSharp.Control.Reactive
open Newtonsoft.Json
open Foosball
open Foosball.Model

let signalr = Signalr.Server "http://localhost:8070"
let publish ev = JsonConvert.SerializeObject ev |> Arduino.t.Update
let r = new Random()

let send disconnectDuration sensor = 
    Arduino.SensorReading(sensor, Arduino.Disconnected, 1L) |> publish
    Arduino.SensorReading(sensor, Arduino.Connected, 2L + disconnectDuration) |> publish
    ()

let sendRandomDuration sensor = send (int64 (r.Next(0, 10)) * 2000L) sensor

let sendDelayedRandom sensor = 
    System.Threading.Thread.Sleep(r.Next(0, 3) * 1000)
    sendRandomDuration sensor

module GameDto = 
    type t = 
        { status : (Team * int) * (Team * int)
          events : (Model.t * string list) list }
    
    let goals_within_seconds = 
        function 
        | Pattern.GoalWithinSeconds 1. x -> "Goal within 1 second" |> Some
        | Pattern.GoalWithinSeconds 2. x -> "Goal within 2 second" |> Some
        | Pattern.GoalWithinSeconds 4. x -> "Goal within 4 second" |> Some
        | Pattern.GoalWithinSeconds 8. x -> "Goal within 8 second" |> Some
        | _ -> None
    
    let speed = 
        function 
        | Achievement.HowCouldYouMissThat x -> sprintf "How could they miss that: %f km/h" (x * 3.6m) |> Some
        | Achievement.SpeedOfLight x -> sprintf "Speed of light: %f km/h" (x * 3.6m) |> Some
        | Achievement.MachThree x -> sprintf "Mach 3: %f km/h" (x * 3.6m) |> Some
        | Achievement.MachTwo x -> sprintf "Mach 2: %f km/h" (x * 3.6m) |> Some
        | Achievement.MachOne x -> sprintf "Mach 1: %f km/h" (x * 3.6m) |> Some
        | _ -> None
    
    let toDto (input : Model.t list) = 
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
          status = input |> List.fold (Pattern.``|GameStatus|``) (((Team.Black, 0), (Team.White, 0))) }

let publishGame (g : t list) = 
    signalr.Send(JsonConvert.SerializeObject(GameDto.toDto g, Formatting.Indented))
    ()

let publishTime (g : Duration) = 
    signalr.Time(JsonConvert.SerializeObject(g.ToString("mm\:ss"), Formatting.Indented))
    ()

let execute = List.iter sendRandomDuration
let (wt, wg, bt, bg) = ("A1", "A2", "A0", "A3")
let config = (GameConfig.GameTimeLimited(Duration.FromSeconds 60.))
let result = GameLogic.start (Model.Team.Black) config publishGame publishTime

[ wt; wg; wt; bg; bt; bg; bt ] |> List.iter sendDelayedRandom
execute [ bt ]
execute [ wt ]
execute [ bg ]
execute [ bt ]
execute [ bg ]
execute [ bt ]
execute [ wg; wt ]
execute [ wt; wg; wt ]
result.Dispose()
ArduinoSerialConnector.connect "COM3" stdin.ReadLine
// type commands[start,stop,test,exit] into REPL
