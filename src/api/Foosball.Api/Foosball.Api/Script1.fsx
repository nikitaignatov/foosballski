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

open FSharp.Data
open FSharp.Control.Reactive
open Foosball
open System
open Foosball.Arduino
open Foosball.Model
open Newtonsoft.Json
open Foosball
open System.Reactive

let notSensorReading (t1 : DateTimeOffset, a : Arduino.Event) = 
    match a with
    | SensorReading(_, _, _) -> false
    | _ -> true

let nonSensors = Arduino.t.Observable |> Observable.filter notSensorReading

let counter = 
    Arduino.t.Observable
    |> Observable.scanInit (0, Stopped) (fun (count, _) (_, v) -> count + 1, v)
    |> Observable.subscribe (fun (count, v) -> printfn "count: %d %A" count v)

let print a = 
    match a with
    | Goal y -> printfn "goal: %A speed: %f" y.team y.speed
    | ThrowInAfterGoal x -> printfn "throwIn after gooal: %A" x.team
    | ThrowIn x -> printfn "throwIn: %A speed: %f" x.team x.speed
    | _ -> ()

let printGame title a = 
    a
    |> List.rev
    |> List.mapi (sprintf "%-5d: %O")
    |> fun list -> (sprintf "-- %s -----------------" title) :: list
    |> List.iter (printfn "%s")

let combined = 
    (Sensor.stream "A0")
    |> Observable.merge (Sensor.stream "A1")
    |> Observable.merge (Sensor.stream "A2")
    |> Observable.merge (Sensor.stream "A3")
    |> Observable.merge (Observable.interval (TimeSpan.FromSeconds 1.) |> Observable.map (fun _ -> Tick))

type GameConfig = 
    | TimeLimited of int
    | GoalLimitedTotal of int
    | GoalLimitedTeam of int

let (|IsGoal|_|) = 
    function 
    | Goal _ -> Some()
    | _ -> None

let (|TrowInAny|_|) = 
    function 
    | ThrowIn x | ThrowInAfterEscape x | ThrowInAfterGoal x -> Some(x)
    | _ -> None

let (|NotEnded|) = 
    function 
    | EndGame :: EndGame :: _ -> false
    | _ -> true

let (|Ended|_|) = 
    function 
    | EndGame :: _ -> Some()
    | _ -> None

let (|IsStartGame|_|) = 
    function 
    | StartGame(a, b) -> Some(a, b)
    | _ -> None

let (|GoalCount|_|) input = 
    input
    |> List.choose (|IsGoal|_|)
    |> List.length
    |> Some

let (|GameStartTime|_|) input = 
    input
    |> List.choose (|IsStartGame|_|)
    |> List.map snd
    |> List.tryHead

let gameLogic state event = 
    match (state, event) with
    | _, Tick -> state
    | _, Reset -> [ state |> List.last ]
    | EndGame :: _, _ -> state
    | [ StartGame(x, _) ], ThrowIn { team = y } when x = y -> event :: state
    | TrowInAny _ :: _, Goal _ -> event :: state
    | TrowInAny _ :: _, TrowInAny t -> ThrowInAfterEscape(t) :: state
    | Goal { team = x } :: _, ThrowIn t when x = t.team -> ThrowInAfterGoal(t) :: state
    | _ -> 
        printfn "INVALID EVENT: %A" event
        state

let endGame config state event = 
    match (config, state, event) with
    | _, _, EndGame :: _ -> state
    | GoalLimitedTotal limit, GoalCount count, _ when count = limit -> EndGame :: state
    | TimeLimited seconds, GameStartTime time, _ when (DateTimeOffset.Now.Subtract(time)).TotalSeconds > (float seconds) -> 
        printfn "%A" (DateTimeOffset.Now.Subtract(time)).TotalSeconds
        EndGame :: state
    | _, _, _ -> event

let ac state event = 
    match (state, event) with
    | _, Achievements.GoalsInRow 3 team -> (sprintf "%A %d goals in row " team 3) :: state
    | _ -> 
        printfn "INVALID state: %A" event
        state

let goals2 = 
    combined
    |> Observable.scanInit [ StartGame(White, DateTimeOffset.Now) ] gameLogic
    |> Observable.scanInit [] (endGame (TimeLimited(30)))
    |> Observable.takeWhile (|NotEnded|)

let result = 
    goals2 |> Observable.subscribe (fun c -> 
                  match c with
                  | Ended -> printGame "GAME RESULT" c
                  | _ -> printGame "GAME STATE" c)

let publish ev = JsonConvert.SerializeObject ev |> Arduino.t.Update

Arduino.t.Update """ {"Case":"PinReading","Fields":[1,{"Case":"On"}]} """
Arduino.t.Update """ {"Case":"PinReading","Fields":[1,{"Case":"Off"}]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A0",{"Case":"Disconnected"},600]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Disconnected"},600]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Connected"},610]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A0",{"Case":"Connected"},601]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"Started"} """
Arduino.t.Update """ {"Case":"Stopped"} """
counter.Dispose()
result.Dispose()
ArduinoSerialConnector.connect "COM3" stdin.ReadLine
// type commands[start,stop,test,exit] into REPL
