namespace Foosball

module GameLogic = 
    open System
    open Foosball.Model
    open Foosball.Patters
    open FSharp.Control.Reactive
    
    let increment state (time : Time) = 
        match state with
        | TrowInAny _ :: _, Tick -> time.AddSeconds(1.)
        | _ -> time
    
    let setGameTime event (time : Time) = 
        let update f x = f { x with gametime = time.Subtract(Time.MinValue) }
        match event with
        | ThrowIn x -> update ThrowIn x
        | ThrowInAfterGoal x -> update ThrowInAfterGoal x
        | ThrowInAfterEscape x -> update ThrowInAfterEscape x
        | Goal x -> (update Goal x)
        | _ -> event
    
    let gameLogic (time, state) event = 
        let event = setGameTime event time
        time, 
        match (state, event) with
        | _, Tick -> state
        | _, Reset -> [ state |> List.last ]
        | EndGame _ :: _, _ -> state
        | [ StartGame(x, _) ], ThrowIn { team = y } when x = y -> event :: state
        | TrowInAny _ :: _, Goal _ -> event :: state
        | TrowInAny _ :: _, TrowInAny t -> ThrowInAfterEscape(t) :: state
        | Goal { team = x } :: _, ThrowIn t when x = t.team -> ThrowInAfterGoal(t) :: state
        | _ -> 
            printfn "INVALID EVENT: %A" event
            state
    
    let endGame config (state) (time, event) = 
        let result = (Time.Now, time)
        match (config, state, event) with
        | _, _, EndGame _ :: _ -> state
        | GoalLimitedTotal limit, GoalCount count, _ when count = limit -> EndGame result :: state
        | TimeLimited seconds, _, _ when (time.Subtract(Time.MinValue)).TotalSeconds > (float seconds) -> 
            printfn "GAME TIME: %A" (time.Subtract(Time.MinValue)).TotalSeconds
            EndGame result :: state
        | _, _, _ -> event
    
    let printGame title a = 
        a
        |> List.rev
        |> List.mapi (sprintf "%-3d: %O")
        |> fun list -> (sprintf "-- %s -----------------" title) :: list
        |> List.iter (printfn "%s")
    
    let gameStream team = 
        (Observable.interval (TimeSpan.FromSeconds 1.) |> Observable.map (fun _ -> Tick))
        |> Observable.merge (Sensor.stream "A0")
        |> Observable.merge (Sensor.stream "A1")
        |> Observable.merge (Sensor.stream "A2")
        |> Observable.merge (Sensor.stream "A3")
        |> Observable.scanInit (Time.MinValue, [ StartGame(team, Time.Now) ]) gameLogic
        |> Observable.scanInit ([]) (endGame (TimeLimited(30)))
        |> Observable.takeWhile (|NotEnded|)
    
    let start team = 
        gameStream team |> Observable.subscribe (fun c -> 
                               match c with
                               | Tick :: _ -> printf "."
                               | Ended -> printGame "GAME RESULT" c
                               | _ -> printGame "GAME STATE" c)
