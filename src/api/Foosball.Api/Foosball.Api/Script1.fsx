#r @"..\packages\FSharp.Data.3.0.0\lib\net45\FSharp.Data.dll"

open FSharp.Data

type evs = JsonProvider< """
[{ "event": "disconnected", "data": { "sensor_pin": "A0", "time": 19848076} },{ "event": "connected", "data": { "sensor_pin": "A0", "time": 20942444} },{ "event": "disconnected", "data": { "sensor_pin": "A0", "time": 24426348} },{ "event": "connected", "data": { "sensor_pin": "A0", "time": 27554332} },{ "event": "disconnected", "data": { "sensor_pin": "A0", "time": 28886284} },{ "event": "connected", "data": { "sensor_pin": "A0", "time": 28964268} },{ "event": "disconnected", "data": { "sensor_pin": "A0", "time": 30918716} },{ "event": "connected", "data": { "sensor_pin": "A0", "time": 30932948} },{ "event": "disconnected", "data": { "sensor_pin": "A0", "time": 32730652} },{ "event": "connected", "data": { "sensor_pin": "A0", "time": 32744900} },{ "event": "disconnected", "data": { "sensor_pin": "A0", "time": 296639356} },{ "event": "connected", "data": { "sensor_pin": "A0", "time": 296654668} },
{ "event": "disconnected", "data": { "sensor_pin": "A0", "time":   1243986284} },
{ "event": "connected", "data": { "sensor_pin": "A0", "time":  1244002652} }]
""" >

let distance cm = cm /  (100_000m)
let ball= distance 4m
let first = evs.GetSamples().[0]
let m = 
    evs.GetSamples()
    |> Seq.skipWhile (fun x -> x.Event = "disconnected")
    |> Seq.map (fun a -> a.Event, a.Data.Time)
    |> Seq.scan (fun (a, _, x, b) (c, d) -> c, (sprintf "%s -> %s" a c), (d - b), d) (first.Event, "", 0, first.Data.Time)
    |> Seq.filter (fun (_, x, _, _) -> x = "disconnected -> connected")
    |> Seq.map (fun (_, x, y, _) -> x, (decimal y) / 1000_000.m) // time in seconds
    |> Seq.map (fun (event,seconds) -> event,  ball / (seconds / 3600m)) // km/hour
    |> Seq.toList
