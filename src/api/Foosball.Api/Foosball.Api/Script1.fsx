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
#load "Achievements.fs"
#load "GameLogic.fs"

open System
open FSharp.Data
open FSharp.Control.Reactive
open Newtonsoft.Json
open Foosball

let publish ev = JsonConvert.SerializeObject ev |> Arduino.t.Update
let r = new Random()

let send disconnectDuration sensor = 
    Arduino.SensorReading(sensor, Arduino.Disconnected, 1) |> publish
    Arduino.SensorReading(sensor, Arduino.Connected, 2 + disconnectDuration) |> publish
    ()

let sendRandomDuration sensor = send (r.Next(0, 10) * 2) sensor

let sendDelayedRandom sensor = 
    System.Threading.Thread.Sleep(r.Next(0, 15) * 1000)
    sendRandomDuration sensor

let execute = List.iter sendRandomDuration
let (wt, wg, bt, bg) = ("A1", "A2", "A0", "A3")
let result = GameLogic.start (Model.Team.White)

[ wt; wg; wt; bg; bt; bg; bt ] |> List.iter sendDelayedRandom
execute [ wt ]
execute [ bg; bt ]
execute [ wt; wg; wt ]
result.Dispose()
ArduinoSerialConnector.connect "COM3" stdin.ReadLine
// type commands[start,stop,test,exit] into REPL
