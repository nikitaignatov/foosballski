namespace Foosball

module Sensor = 
    open Foosball.Model
    open Foosball.Arduino
    open System
    open FSharp.Data.UnitSystems
    
    module Speed = 
        open System
        
        let ball = 3.8m / 100m
        
        let fromTime time = 
            printfn "%f" time
            let time = 
                if time > 0m then time
                else 1m
            
            let time = (time / 1000000m)
            let result = ball / time
            Decimal.Round(result, 2)
    
    let toMeta team (speed, timestamt) = 
        { team = team
          speed = speed
          timestamp = timestamt
          gametime = TimeSpan.Zero
          id = Guid.NewGuid()}
    
    // sensor mapping
    let (|Sensor|_|) id input = 
        match input with
        | { Reading.id = x } when x = id -> Some(input.speed, input.timestamp)
        | _ -> None
    
    let subtract (t1 : Time, t2 : Time) = t2.Subtract(t1).TotalMilliseconds
    let occuredWithin (threshold : float) t = (subtract t) < threshold
    
    let simultaneous ((t1, a), (t2, b)) = 
        match a, b with
        | SensorReading(a, Disconnected, _), SensorReading(b, Connected, _) when (occuredWithin 800. (t1, t2)) && (a = b) -> true
        | _ -> false
    
    let withSensor sensor (_, a : Event) = 
        match a with
        | SensorReading(a, _, _) when a = sensor -> true
        | _ -> false
    
    let toDomainModel = 
        function 
        | Sensor "A0" x -> ThrowIn(toMeta Black x)
        | Sensor "A1" x -> ThrowIn(toMeta White x)
        | Sensor "A2" x -> Goal(toMeta White x)
        | Sensor "A3" x -> Goal(toMeta Black x)
    
    let toReading ((t, SensorReading(key, _, time)), (_, SensorReading(_, _, time1))) = 
        { id = key
          speed = Speed.fromTime (decimal (time1 - time))
          timestamp = t }
    
    let stream sensor = 
        Arduino.t.Observable
        |> Observable.filter (withSensor sensor)
        |> Observable.pairwise
        |> Observable.partition simultaneous
        |> fun (a, _) -> a |> Observable.map toReading
        |> Observable.map toDomainModel
