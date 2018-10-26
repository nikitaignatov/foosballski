namespace Foosball

module GameLogic = 
    open System
    open Foosball.Model
    open Foosball.Pattern
    open FSharp.Control.Reactive
    open Newtonsoft.Json
    
    let increment state (time : Duration) = 
        match state with
        | TrowInAny _ :: _, Tick -> time.Add(Duration.FromSeconds 1.)
        | _ -> time
    
    let setGameTime event (time : Duration) = 
        let update f x = f { x with gametime = time }
        match event with
        | ThrowIn x -> update ThrowIn x
        | ThrowInAfterGoal x -> update ThrowInAfterGoal x
        | ThrowInAfterEscape x -> update ThrowInAfterEscape x
        | Goal x -> (update Goal x)
        | _ -> event
    
    let gameLogic (time, state) event = 
        let time = increment (state, event) time
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
        | GameTimeLimited durtion, _, _ when time >= durtion -> EndGame result :: state
        | TimeLimited duration, x, _ when List.last event
                                          |> (fun (StartGame(_, x)) -> Time.Now.Subtract x)
                                          >= duration -> 
            printfn "GAME TIME: %O" (time)
            EndGame result :: state
        | _, _, _ -> event
    
    let gameStream team config = 
        (Observable.interval (Duration.FromSeconds 1.) |> Observable.map (fun _ -> Tick))
        |> Observable.merge (Sensor.stream "A0")
        |> Observable.merge (Sensor.stream "A1")
        |> Observable.merge (Sensor.stream "A2")
        |> Observable.merge (Sensor.stream "A3")
        |> Observable.scanInit (Duration.Zero, [ StartGame(team, Time.Now) ]) gameLogic
        |> Observable.scanInit ([]) (endGame config)
        |> Observable.takeWhile (|NotEnded|)
    
    let start team config = 
        config
        |> gameStream team
        |> Observable.subscribe (fun c -> 
               match c with
               | Tick :: _ -> printf "."
               | Ended -> 
                   Newtonsoft.Json.JsonConvert.SerializeObject(c, Formatting.Indented) |> fun s -> IO.File.WriteAllText("c:/temp/game_result_" + (Time.Now.ToFileTime().ToString()) + ".json", s)
                   ConsolePrinter.printGame "GAME RESULT" c
               | _ -> ConsolePrinter.printGame "GAME STATE" c)
