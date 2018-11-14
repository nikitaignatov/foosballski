namespace Foosball

module GameLogic = 
    open System
    open Foosball.Model
    open FSharp.Control.Reactive
    
    type Aggregate<'TState, 'TCommand, 'TEvent> = 
        { zero : 'TState
          apply : 'TState -> 'TEvent -> 'TState
          exec : 'TState -> 'TCommand -> Result<'TEvent list, string> }
    
    type GameSubscription<'a>() = 
        static let event = (new Event<'a>())
        static member Observable = event.Publish |> Observable.map id
        static member Update msg = 
            try 
                event.Trigger(msg)
            with e -> 
                printfn "message: %A" msg
                printfn "error: %s" e.Message
            ()
    
    let apply (state : State) event = 
        let updateTimestamp (state : GameData) timestamp = { state with timestamp = timestamp }
        let updateTime (state : GameData) gametime = { state with timer = gametime } |> updateTimestamp
        match event with
        | Initialized id -> 
            match state with
            | Zero -> New(id)
            | _ -> ErrorState "Can only be applied in Zero state"
        | Configured conf -> 
            match state with
            | New(id) -> Configuration(id, conf)
            | _ -> ErrorState "Can only be configured when game is new."
        | Swapped(White) -> 
            match state with
            | (WaitingThrowIn(_, state)) & (WaitingThrowIn(_, { whiteDefense = wd; whiteCenterForward = wcf })) -> 
                { state with whiteDefense = wcf
                             whiteCenterForward = wd }
                |> fun c -> WaitingThrowIn(Black, c)
            | _ -> ErrorState "Can only be configured when game is new."
        | ThrownIn meta -> 
            match state with
            | WaitingThrowIn(color, state) when meta.color = color -> 
                { state with throwins = meta :: state.throwins
                             timestamp = meta.timestamp }
                |> Playing
            | Playing state -> 
                { state with throwins = meta :: state.throwins
                             timer = meta.gametime
                             timestamp = meta.timestamp }
                |> Playing
            | Pause state -> 
                { state with throwins = meta :: state.throwins
                             timestamp = meta.timestamp }
                |> Playing
            | _ -> ErrorState(sprintf "Can only be done when WaitingThrowIn or Playing. current: %A" state)
        | Paused(time, duration) -> 
            match state with
            | Playing state -> updateTime state duration time |> Pause
            | _ -> ErrorState "Can only be configured when game is new."
        | ScoredGoal(color, meta) -> 
            match state with
            | Playing state -> updateTime state meta.gametime meta.timestamp |> fun x -> (color, meta, x) |> GoalRegistration
            | _ -> ErrorState "Can only be configured when game is new."
        | PlayerScoredLastGoal(p) -> 
            match state with
            | GoalRegistration(color, _, state) -> 
                ((if color = White then Black
                  else White), state)
                |> WaitingThrowIn
            | _ -> ErrorState "Can only be configured when game is new."
        | RejectGoal -> 
            match state with
            | GoalRegistration(color, _, state) -> 
                ((if color = White then Black
                  else White), state)
                |> WaitingThrowIn
            | _ -> ErrorState "Can only be configured when game is new."
        | RegisteredPlayer p -> 
            match state with
            | Configuration(id, config) -> (id, config, { Reg.zero with whiteDefense = Some p }) |> Registration
            | (Registration(_, _, r)) & (Registration(id, config, { whiteCenterForward = None })) -> Registration(id, config, { r with whiteCenterForward = Some p })
            | (Registration(_, _, r)) & (Registration(id, config, { blackDefense = None })) -> Registration(id, config, { r with blackDefense = Some p })
            | (Registration(id, config, { blackDefense = Some bd; whiteCenterForward = Some wcf; whiteDefense = Some wd })) -> State.WaitingThrowIn(Black, GameData.start id config wd wcf bd p)
            | _ -> State.ErrorState(sprintf "Can only register when game is in configuration or registration mode. %A" state)
        | EndedGame(time, duration) -> 
            match state with
            | Playing state -> updateTime state duration time |> State.Result
            | _ -> ErrorState(sprintf "Can only end the game when in playing state. Currnet state:  %A" state)
        | _ -> state
    
    let handle state command = 
        let apply event = 
            match apply state event with
            | ErrorState s -> Error s
            | _ -> Ok [ event ]
        match state, command with
        | Result _, _ -> Ok []
        | Playing data, EndGame -> 
            let data = GameData.update data
            EndedGame(data.timestamp, data.timer) |> apply
        | _, NewGame id -> Initialized id |> apply
        | _, Configure data -> Configured data |> apply
        | _, ScoredLastGoal data -> PlayerScoredLastGoal data |> apply
        | GoalRegistration(Black, goal, { blackDefense = (r, _); blackCenterForward = (r1, _) }), Register data when r = data || r1 = data -> PlayerScoredLastGoal data.player |> apply
        | GoalRegistration(White, goal, { whiteDefense = (r, _); whiteCenterForward = (r1, _) }), Register data when r = data || r1 = data -> PlayerScoredLastGoal data.player |> apply
        | GoalRegistration(color, goal, game), Register data -> PlayerScoredLastGoal data.player |> apply
        | _, Register data -> RegisteredPlayer data |> apply
        | WaitingThrowIn(color, game), ThrowIn x when x.color = color -> 
            { x with gametime = game.timer
                     timestamp = x.timestamp }
            |> ThrownIn
            |> apply
        | Pause game, ThrowIn x -> 
            { x with gametime = game.timer
                     timestamp = x.timestamp }
            |> ThrownIn
            |> apply
        | Playing game, ThrowIn x -> 
            { x with gametime = game.timer.Add(x.timestamp - game.timestamp)
                     timestamp = x.timestamp }
            |> ThrownIn
            |> apply
        | _, Swap data -> Swapped data |> apply
        | Playing game, Goal(color, goal) -> 
            (color, 
             { goal with gametime = game.timer.Add(goal.timestamp - game.timestamp)
                         timestamp = goal.timestamp })
            |> ScoredGoal
            |> apply
        | _, Tick -> Ok []
        | _, e -> Error(sprintf "Cannot apply the command %A when in state %A" command state)
    
    let makeHandler (aggregate : Aggregate<'TState, 'TCommand, 'TEvent>) (load : Id -> 'TEvent list, commit : Id * int -> ('TEvent list -> unit)) = 
        fun (id, version) command -> 
            let state = load id |> List.fold aggregate.apply aggregate.zero
            let event = aggregate.exec state command
            match event with
            | Ok event -> event |> commit (id, version)
            | Error message -> printfn "%s" message
