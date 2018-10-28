namespace Foosball

module GameLogic = 
    open System
    open Foosball.Model
    open Foosball.Pattern
    open FSharp.Control.Reactive
    open Newtonsoft.Json
    
    let increment state (time : Duration) = 
        match state with
        | GameControl.TrowInAny _ :: _, Tick -> time.Add(Duration.FromSeconds 1.)
        | _ -> time
    
    let setGameTime event (time : Duration) = 
        let update f x = f { x with gametime = time }
        match event with
        | ThrowIn x -> update ThrowIn x
        | ThrowInAfterGoal x -> update ThrowInAfterGoal x
        | ThrowInAfterEscape x -> update ThrowInAfterEscape x
        | Goal x -> (update Goal x)
        | _ -> event
    
    let endGame config (state) (time, event) = 
        let result = (Time.Now, time)
        match (config, state, event) with
        | _, _, EndGame _ :: _ -> state
        | GoalLimitedTotal limit, Goal.Count count, _ when count = limit -> EndGame result :: state
        | GameTimeLimited durtion, _, _ when time >= durtion -> EndGame result :: state
        | TimeLimited duration, x, _ when (List.last event |> function 
                                           | (StartGame(_, x)) -> Time.Now.Subtract x
                                           | _ -> Duration.Zero)
                                          >= duration -> 
            printfn "GAME TIME: %O" (time)
            EndGame result :: state
        | _, _, _ -> event
    
    let gameLogic f config (time, state) event = 
        let time = increment (state, event) time
        f (time.ToString("mm\:ss"))
        let event = setGameTime event time
        let event = endGame config state (time, event :: state) |> List.head
        printfn "-> %A" event
        time, 
        match (state, event) with
        | Registration.RegisterPlayers state -> state
        | _, EndGame _ -> event :: state
        | _, Tick -> state
        | StartGame(x, _) :: _, ThrowIn { team = y } when x.color = y.color -> event :: state
        | GameControl.TrowInAny _ :: _, Goal _ -> event :: state
        | GameControl.TrowInAny _ :: _, GameControl.TrowInAny t -> ThrowInAfterEscape(t) :: state
        | Goal { team = x } :: _, ThrowIn t when x.color = t.team.color -> ThrowInAfterGoal(t) :: state
        | _ -> 
            printfn "INVALID EVENT: %A" event
            state
    
    let gameStream cardreader t team config = 
        (Observable.interval (Duration.FromSeconds 1.) |> Observable.map (fun _ -> Tick))
        |> Observable.merge (cardreader)
        |> Observable.merge (Sensor.stream "A0")
        |> Observable.merge (Sensor.stream "A1")
        |> Observable.merge (Sensor.stream "A2")
        |> Observable.merge (Sensor.stream "A3")
        |> Observable.scanInit (Duration.Zero, [ Configure(config) ]) (gameLogic t config)
        |> Observable.map snd
        |> Observable.takeWhile GameControl.(|ContinueObserving|)
    
    let start c team config f t players = 
        config
        |> gameStream c t team
        |> Observable.subscribe (fun c -> 
               f c
               match c with
               | StartGame _ :: RegisterTeam({ defense = c; attack = d }) :: RegisterTeam({ defense = a; attack = b }) :: _ -> players [ a; b; c; d ]
               | Registration.RegisteredPlayers(_, p, message) -> 
                   t message
                   players (p)
               | Ended -> 
                   Newtonsoft.Json.JsonConvert.SerializeObject(c, Formatting.Indented) |> fun s -> IO.File.WriteAllText("c:/temp/game_result_" + (Time.Now.ToFileTime().ToString()) + ".json", s)
                   ConsolePrinter.printGame "GAME RESULT" c
               | _ -> ConsolePrinter.printGame "GAME STATE " c)
