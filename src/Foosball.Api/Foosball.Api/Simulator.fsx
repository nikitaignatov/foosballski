// load packages
#r @"..\packages\FSharp.Data.3.0.0\lib\net45\FSharp.Data.dll"
#r @"..\packages\System.Reactive.4.1.2\lib\net46\System.Reactive.dll"
//
#r @"..\packages\Dynamitey.2.0.9.136\lib\net40\Dynamitey.dll"
#r @"..\packages\FSharp.Control.Reactive.4.1.0\lib\net46\FSharp.Control.Reactive.dll"
#r @"..\packages\FSharp.Data.3.0.0\lib\net45\FSharp.Data.dll"
#r @"..\packages\FSharp.Interop.Dynamic.4.0.3.130\lib\net40\FSharp.Interop.Dynamic.dll"
#r @"..\packages\Newtonsoft.Json.11.0.2\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\PCSC.4.0.1\lib\net40\PCSC.dll"
#r @"..\packages\PCSC.Iso7816.4.0.1\lib\net40\PCSC.Iso7816.dll"
#r @"..\packages\Owin.1.0\lib\net40\Owin.dll"
#r @"..\packages\Microsoft.AspNet.SignalR.Core.2.3.0\lib\net45\Microsoft.AspNet.SignalR.Core.dll"
#r @"..\packages\Microsoft.Owin.4.0.0\lib\net451\Microsoft.Owin.dll"
#r @"..\packages\Microsoft.AspNet.SignalR.Core.2.3.0\lib\net45\Microsoft.AspNet.SignalR.Core.dll"
#r @"..\packages\Microsoft.Owin.4.0.0\lib\net451\Microsoft.Owin.dll"
#r @"..\packages\Microsoft.Owin.Cors.4.0.0\lib\net451\Microsoft.Owin.Cors.dll"
#r @"..\packages\Microsoft.AspNet.Cors.5.2.6\lib\net45\System.Web.Cors.dll"
#r @"..\packages\Microsoft.Owin.Diagnostics.4.0.0\lib\net451\Microsoft.Owin.Diagnostics.dll"
#r @"..\packages\Microsoft.Owin.FileSystems.4.0.0\lib\net451\Microsoft.Owin.FileSystems.dll"
#r @"..\packages\Microsoft.Owin.Host.HttpListener.4.0.0\lib\net451\Microsoft.Owin.Host.HttpListener.dll"
#r @"..\packages\Microsoft.Owin.Hosting.4.0.0\lib\net451\Microsoft.Owin.Hosting.dll"
#r @"..\packages\Microsoft.Owin.Security.4.0.0\lib\net451\Microsoft.Owin.Security.dll"
#r @"..\packages\Microsoft.Owin.StaticFiles.4.0.0\lib\net451\Microsoft.Owin.StaticFiles.dll"
// TODO: Send refs to F# interactive
// load files
#load "Arduino.fs"
#load "ArduinoSerialConnector.fs"
#load "Model.fs"
#load "Sensor.fs"
#load "Achievement.fs"
#load "ConsolePrinter.fs"
#load "GameLogic.fs"
#load "Signalr.fs"
#load "Nfc.fs"

open System
open FSharp.Data
open FSharp.Control.Reactive
open Newtonsoft.Json
open Foosball
open Foosball.Model
open Nfc.Reader
open PCSC.Monitoring

let obs2, (monitor : SCardMonitor) = CardReader.execute()
let signalr = Signalr.Server "http://localhost:8070"
let publish ev = JsonConvert.SerializeObject ev |> Arduino.t.Update
let r = new Random()

let send disconnectDuration sensor = 
    Arduino.SensorReading(sensor, Arduino.Disconnected, 1L) |> publish
    Arduino.SensorReading(sensor, Arduino.Connected, 2L + disconnectDuration) |> publish
    ()

let sendRandomDuration sensor = send (int64 (r.Next(1, 35)) * 1500L) sensor

let sendDelayedRandom sensor = 
    System.Threading.Thread.Sleep(r.Next(0, 3) * 1000)
    sendRandomDuration sensor

module GameDto = 
    type t = 
        { status : (Team * int) * (Team * int)
          events : (Model.GameEvent * string list) list }
    
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
    
    let toDto (input : Model.GameEvent list) = 
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
          status = input |> List.fold (Pattern.``|GameStatus|``) (((Team.black, 0), (Team.white, 0))) }

let publishGame (g : GameEvent list) = 
    signalr.Send(JsonConvert.SerializeObject(GameDto.toDto g, Formatting.Indented))
    ()

let publishTime (g : string) = 
    signalr.Time(JsonConvert.SerializeObject(g, Formatting.Indented))
    ()

let publishPlayers (g : Player list) = 
    signalr.Players(JsonConvert.SerializeObject(g, Formatting.Indented))
    ()

let execute = List.iter sendRandomDuration
let (wt, wg, bt, bg) = ("A1", "A2", "A0", "A3")
let config = (GameConfig.GameTimeLimited(Duration.FromSeconds 60.))
let cardToPlayer card = { Player.zero with card = card }

let regs = 
    obs2
    |> Observable.map (fun x -> 
           match x with
           | Nfc.Reader.CardReader.Inserted x -> 
               cardToPlayer x
               |> Register
               |> Some
           | _ -> None)
    |> Observable.choose id

let result = GameLogic.start (regs) (Model.Team.black) config publishGame publishTime publishPlayers

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
monitor.Cancel()
ArduinoSerialConnector.connect "COM3" stdin.ReadLine
// type commands[start,stop,test,exit] into REPL
