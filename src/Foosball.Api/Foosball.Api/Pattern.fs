namespace Foosball

module Pattern = 
    open Foosball.Model
    
    module Goal = 
        let (|IsGoal|_|) = 
            function 
            | Goal meta -> Some(meta)
            | _ -> None
        
        let (|Goals|_|) a = a |> (List.choose (|IsGoal|_|) >> Some)
        
        let (|NotGoal|_|) = 
            function 
            | Goal _ -> None
            | _ -> Some()
        
        let (|GoalsInRow|_|) count input = 
            input
            |> List.choose (|IsGoal|_|)
            |> List.map (fun { team = team } -> team)
            |> List.fold (fun (team, acc) v -> 
                   if acc >= count then team, acc
                   elif v.color = team then team, acc + 1
                   else v.color, 1) (Black, 0)
            |> fun (team, countInRow) -> 
                if countInRow >= count then Some team
                else None
        
        let (|Speed|_|) = 
            function 
            | IsGoal x :: _ -> Some(x.speed)
            | _ -> None
        
        let (|Count|_|) = 
            function 
            | Goals x -> x |> (List.length >> Some)
            | _ -> None
        
        let rec (|DurationBetweenGoals|_|) = 
            function 
            | [] -> None
            | x :: [] -> Some([ (x.gametime, x) ])
            | x :: [ y ] -> Some([ (x.gametime - y.gametime, x) ])
            | x :: DurationBetweenGoals((duration, y) :: tail) -> Some((x.gametime - y.gametime, x) :: (duration, y) :: tail)
            | _ -> None
        
        let (|GoalWithinSeconds|_|) seconds list = 
            match list with
            | IsGoal _ :: _ & Goals(DurationBetweenGoals((duration, x) :: tail)) when duration.TotalSeconds <= seconds -> Some((duration, x) :: tail)
            | _ -> None
    
    module GameControl = 
        let (|EndGame|_|) (state, event) = 
            match (state, event) with
            | _, EndGame _ -> (event :: state) |> Some
            | _ -> None
        
        let (|StartGame|_|) (state, event) = 
            match (state, event) with
            | StartGame(x, _) :: _, ThrowIn { team = y } when x.color = y.color -> (event :: state) |> Some
            | _ -> None
        
        let (|TrowInAny|_|) = 
            function 
            | ThrowIn x | ThrowInAfterEscape x | ThrowInAfterGoal x -> Some(x)
            | _ -> None
        
        let (|Playing|_|) = 
            function 
            | TrowInAny _ :: _ -> Some()
            | _ -> None
        
        let (|Paused|_|) = 
            function 
            | TrowInAny _ :: _ -> None
            | _ -> Some()
        
        let (|RegisterGoal|_|) (state, event) = 
            match (state, event) with
            | TrowInAny _ :: _, Goal _ -> (event :: state) |> Some
            | _ -> None
        
        let (|RegisterBallOutsideField|_|) (state, event) = 
            match (state, event) with
            | TrowInAny _ :: _, TrowInAny t -> ThrowInAfterEscape(t) :: state |> Some
            | _ -> None
        
        let (|RegisterThrowInAfterGoal|_|) (state, event) = 
            match (state, event) with
            | Goal { team = x } :: _, ThrowIn t when x.color = t.team.color -> ThrowInAfterGoal(t) :: state |> Some
            | _ -> None
        
        let (|GameStatus|) acc v = 
            match (acc, v) with
            | ((a, sa), x), Goal.IsGoal t when t.team = a -> ((a, int sa + 1), x)
            | (x, (b, sb)), Goal.IsGoal t when t.team = b -> (x, (b, int sb + 1))
            | _ -> acc
        
        let (|ContinueObserving|) = 
            function 
            | Model.GameEvent.EndGame _ :: Model.GameEvent.EndGame _ :: _ -> false
            | _ -> true
        
        module End = 
            let (|ByGameTimeLimit|_|) ((now, time), config, state, event) = 
                match (config, state, event) with
                | GameTimeLimited durtion, _, _ when time >= durtion -> EndGame(now, time) :: state |> Some
                | _ -> None
            
            let (|ByTotalGoalCount|_|) (result, config, state, event) = 
                match (config, state, event) with
                | GoalLimitedTotal limit, Goal.Count count, _ when count = limit -> EndGame result :: state |> Some
                | _ -> None
            
            let (|ByTimeLimit|_|) ((result), config, state, event) = 
                let duration x = 
                    List.last event |> function 
                    | (Model.StartGame(_, x)) -> Time.Now.Subtract x
                    | _ -> Duration.Zero
                match (config, state, event) with
                | TimeLimited limit, x, _ when duration x >= limit -> EndGame(result) :: state |> Some
                | _ -> None
    
    module Registration = 
        let private empty = Player.zero
        
        let (|AllPlayersRegistered|_|) input = 
            match input |> List.rev with
            | [ Configure _; Register a; Register b; Register c; Register d ] -> Some([ a; b; c; d ])
            | Configure _ :: Register a :: Register b :: Register c :: Register d :: _ -> Some([ a; b; c; d ])
            | _ -> None
        
        let (|NotAllPlayersRegistered|_|) = 
            function 
            | AllPlayersRegistered _ -> None
            | _ -> Some()
        
        let (|MissingOnePlayer|_|) input = 
            match input with
            | [ Register wd; Register wa; Register bd; Configure _ ] -> Some(wd, wa, bd)
            | _ -> None
        
        let (|RegisterPlayers|_|) (state, event) = 
            match (state, event) with
            | MissingOnePlayer(wd, wa, bd), Register ba -> 
                let white = Team.create (White) wa wd
                let black = Team.create (Black) ba bd
                let start = StartGame(black, Time.Now)
                start :: RegisterTeam black :: RegisterTeam white :: event :: state |> Some
            | NotAllPlayersRegistered, Register _ -> event :: state |> Some
            | _ -> None
        
        let private (|RegisterWhiteDefense|_|) = 
            function 
            | [ Configure _ ] -> Some([ empty; empty; empty; empty ])
            | _ -> None
        
        let private (|RegisterWhiteAttack|_|) = 
            function 
            | [ Register a; Configure _ ] -> Some([ a; empty; empty; empty ])
            | _ -> None
        
        let private (|RegisterBlackDefense|_|) = 
            function 
            | [ Register b; Register a; Configure _ ] -> Some([ a; b; empty; empty ])
            | _ -> None
        
        let private (|RegisterBlackAttack|_|) = 
            function 
            | [ Register c; Register b; Register a; Configure _ ] -> Some([ a; b; c; empty ])
            | _ -> None
        
        let (|RegisteredPlayers|_|) input = 
            match input with
            | [ Register d; Register c; Register b; Register a; Configure _ ] -> Some("READY", [ a; b; c; d ], "Throw in the ball to start the game.")
            | RegisterBlackAttack(a) -> Some("Configure", a, "Register player for Black Attack position")
            | RegisterBlackDefense(a) -> Some("Configure", a, "Register player for Black Defense position")
            | RegisterWhiteAttack(a) -> Some("Configure", a, "Register player for White Attack position")
            | RegisterWhiteDefense(a) -> Some("Configure", a, "Register player for White Defense position")
            | _ -> None
