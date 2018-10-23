// load packages
#r @"..\packages\FSharp.Data.3.0.0\lib\net45\FSharp.Data.dll"
#r @"..\packages\System.Reactive.4.0.0\lib\net46\System.Reactive.dll"
#r @"..\packages\Newtonsoft.Json.11.0.2\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\FSharp.Control.Reactive.4.1.0\lib\net46\FSharp.Control.Reactive.dll"

// load files
#load "Arduino.fs"
#load "ArduinoSerialConnector.fs"

open FSharp.Data
open System.Reactive.Linq
open FSharp.Control.Reactive
open Foosball
open System
open Foosball.Arduino

let sameSensor a b = a = b
let occuredWithin (threshold:float) (t1:DateTimeOffset) (t2:DateTimeOffset)= t2.Subtract(t1).TotalMilliseconds<threshold
let simultaneous ((t1:DateTimeOffset,a:Arduino.Event) ,(t2:DateTimeOffset,b:Arduino.Event)) =
    match a,b with 
    | SensorReading(a,Disconnected,_),SensorReading(b,Connected,_) 
        when (occuredWithin 800. t1 t2) && (sameSensor a b)
        -> true
    | _ -> false
let notSensorReading (t1:DateTimeOffset,a:Arduino.Event)  =
    match a with 
    | SensorReading( _,_,_)        -> false
    | _ -> true

let withSensor sensor (_,a:Arduino.Event)  =
    match a with 
    | SensorReading(a,_,_) when a = sensor -> true
    | _ -> false

let nonSensors = 
    Arduino.t.Observable
    |> Observable.filter notSensorReading

let counter = 
    Arduino.t.Observable
    |> Observable.scanInit (0,Stopped)(fun (count,_) (_,v) -> count + 1,v) 
    |> Observable.subscribe (fun (count,v)->printfn "count: %d %A" count v)

    
type Reading={id:string;time:int; timestamp:DateTimeOffset;}
let stream sensor = 
    Arduino.t.Observable
    |> Observable.filter (withSensor sensor)
    |> Observable.pairwise 
    |> Observable.partition simultaneous

    //|> Observable.map (fun (key)-> key.Key,Observable.pairwise key|>Observable.partition simultaneous|> fst|>Observable.map (fun (ev1,_) -> ev1))
    |> fun (a,_)-> a|> Observable.map (fun ((t,SensorReading(key,_,time)),(_,SensorReading(_,_,time1)))->
                                                {id=key;time=time1-time;timestamp=t} )
    //|>Observable.subscribe (fun a ->printfn "sim: %A" a)
type Team = Black | White
type FoosballMetaData = {team:Team;spead:float;timestamp:DateTimeOffset}
type Foosball =
    | StartGame of Team
    | EndGame
    | Goal of FoosballMetaData
    | ThrowIn of FoosballMetaData
    | ThrowInAfterGoal of FoosballMetaData
// sensor mapping
let (|InBlackGoal|_|) input = 
    match input with
    | {Reading.id="A2"} -> Some {input with id = "black"}
    | _ -> None

let (|InWhiteGoal|_|) input = 
    match input with
    | {Reading.id="A3"} -> Some {input with id = "white"}
    | _ -> None

let (|ThrowInBlack|_|) input = 
    match input with
    | {Reading.id="A0"} -> Some {input with id = "black"}
    | _ -> None

let (|ThrowInWhite|_|) input = 
    match input with
    | {Reading.id="A1"} -> Some {input with id = "white"}
    | _ -> None
    
// general patterns
let (|ThrowIn|_|) input = 
    match input with
    | ThrowInWhite x -> Some x
    | ThrowInBlack x -> Some x
    | _ -> None

let (|Goal|_|) (a,b) = 
    match a,b with
    | ThrowIn _, InBlackGoal x -> Some x
    | ThrowIn _, InWhiteGoal x -> Some x
    | _ -> None

// logic patterns
let (|ThrowInAfterGoal|_|) (a, b) = 
    match (a, b) with
    | InBlackGoal _,ThrowInBlack x -> Some x
    | InWhiteGoal _,ThrowInWhite x -> Some x
    | _ -> None

let print (e) = 
    match e with 
    | Goal y -> printfn "goal: %A"y.id
    | ThrowInAfterGoal x->printfn "throwIn after gooal: %A" x.id
    | _,ThrowIn x->printfn "throwIn: %A" x.id
    |_->()
let combined =    
    (stream "A0")
    |> Observable.merge (stream "A1") 
    |> Observable.merge (stream "A2")
    |> Observable.merge (stream "A3")
    |> Observable.pairwise

let goals = combined |> Observable.subscribe print

Arduino.t.Update """ {"Case":"PinReading","Fields":[1,{"Case":"On"}]} """
Arduino.t.Update """ {"Case":"PinReading","Fields":[1,{"Case":"Off"}]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A0",{"Case":"Disconnected"},600]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Disconnected"},600]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Connected"},610]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A0",{"Case":"Connected"},610]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A2",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Disconnected"},820]} """
Arduino.t.Update """ {"Case":"SensorReading","Fields":["A1",{"Case":"Connected"},868]} """
Arduino.t.Update """ {"Case":"Started"} """
Arduino.t.Update """ {"Case":"Stopped"} """

counter.Dispose()
goals.Dispose()


ArduinoSerialConnector.connect "COM3" stdin.ReadLine
// type commands[start,stop,test,exit] into REPL

type evs = JsonProvider< """
[
{ "event": "disconnected", "data": { "sensor_pin": "A0",    "time": 15289   } },
{ "event": "connected", "data": { "sensor_pin": "A0",       "time": 15420} },
{ "event": "disconnected", "data": { "sensor_pin": "A0",    "time": 141243   } },
{ "event": "connected", "data": { "sensor_pin": "A0",       "time": 141265} }
]
""" >

let distance cm = cm /  (100_000m)
let ball= distance 1.8m
let first = evs.GetSamples().[0]
let m = 
    evs.GetSamples()
    |> Seq.skipWhile (fun x -> x.Event = "disconnected")
    |> Seq.map (fun a -> a.Event, a.Data.Time)
    |> Seq.scan (fun (a, _, x, b) (c, d) -> c, (sprintf "%s -> %s" a c), (d - b), d) (first.Event, "", 0, first.Data.Time)
    |> Seq.filter (fun (_, x, _, _) -> x = "disconnected -> connected")
    |> Seq.map (fun (_, x, y, _) -> x, (decimal y) / 1000.m) // time in seconds
    |> Seq.map (fun (event,seconds) -> event,  ball / (seconds / 3600m)) // km/hour
    |> Seq.toList
