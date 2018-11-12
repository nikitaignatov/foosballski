namespace Foosball

module GameLogic = 
    open System
    open Foosball.Model
    open Foosball.Pattern
    open FSharp.Control.Reactive
    open Newtonsoft.Json
    
    let increment (time : Duration) = 
        function 
        | GameControl.Playing, Tick -> time.Add(Duration.FromSeconds 1.)
        | _ -> time
    
    let setGameTime event (time : Duration) = 
        let update f (x : EventMetaData) = f { x with gametime = time }
        match event with
        | ThrowIn x -> update ThrowIn x
        | ThrowInAfterGoal x -> update ThrowInAfterGoal x
        | ThrowInAfterEscape x -> update ThrowInAfterEscape x
        | Goal x -> (update Goal x)
        | _ -> event
    
    let endGame config (state) (time, event) = 
        match ((Time.Now, time), config, state, event) with
        | _, _, _, EndGame _ :: _ -> state
        | GameControl.End.ByTotalGoalCount state -> state
        | GameControl.End.ByGameTimeLimit state -> state
        | GameControl.End.ByTimeLimit state -> state
        | _ -> event
    
    let gameLogic f config (time, state) event = 
        let time = increment time (state, event)
        let event = setGameTime event time
        let event = endGame config state (time, event :: state) |> List.head
        time, 
        match (state, event) with
        | GameControl.Playing, Tick -> 
            f (time.ToString("mm\:ss"))
            state
        | _, Tick -> state
        | Registration.RegisterPlayers f state -> state
        | GameControl.EndGame state -> state
        | GameControl.StartGame state -> state
        | GameControl.RegisterGoal state -> state
        | GameControl.RegisterWhoScoredLastGoal state -> state
        | GameControl.RegisterBallOutsideField state -> state
        | GameControl.RegisterThrowInAfterGoal state -> state
        | _ -> 
            printfn "INVALID EVENT: %A" event
            state
    
    let gameStream (cardreader : IObservable<GameCommand>) = 
        (Observable.interval (Duration.FromSeconds 1.) |> Observable.map (fun _ -> Tick))
        |> Observable.merge (Signalr.t<GameCommand>.Observable |> Observable.map (fun (id, time, cmd) -> cmd))
        |> Observable.merge (cardreader)
        |> Observable.merge (Sensor.stream "A0")
        |> Observable.merge (Sensor.stream "A1")
        |> Observable.merge (Sensor.stream "A2")
        |> Observable.merge (Sensor.stream "A3")
    
    let start c = 
        c
        |> gameStream
        |> Observable.subscribe (fun c -> 
               match c with
               //| Registration.RegisteredPlayers(_, p, message) -> players (p)
               //| EndGame _ :: _ -> Utils.serialize c |> saveFile
               //| Registration.GoalsByPlayers list -> players list
               | Tick -> 
                   EventStore.Game.Execute c
                   ()
               | _ -> printfn "GAME STATE %A" c)
