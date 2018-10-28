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
        let (|TrowInAny|_|) = 
            function 
            | ThrowIn x | ThrowInAfterEscape x | ThrowInAfterGoal x -> Some(x)
            | _ -> None
        
        let (|ContinueObserving|) = 
            function 
            | EndGame _ :: EndGame _ :: _ -> false
            | _ -> true
        
        let (|Ended|_|) = 
            function 
            | EndGame _ :: _ -> Some()
            | _ -> None
        
        let (|IsStartGame|_|) = 
            function 
            | StartGame(a, b) -> Some(a, b)
            | _ -> None
        
        let (|GameStartTime|_|) input = 
            input
            |> List.choose (|IsStartGame|_|)
            |> List.map snd
            |> List.tryHead
        
        let (|NumberOfBallEscapes|_|) = Some()
        
        let (|GameStatus|) acc v = 
            match (acc, v) with
            | ((a, sa), x), Goal.IsGoal t when t.team = a -> ((a, int sa + 1), x)
            | (x, (b, sb)), Goal.IsGoal t when t.team = b -> (x, (b, int sb + 1))
            | _ -> acc
    
    module Registration = 
        let (|AllPlayersRegistered|_|) input = 
            match input |> List.rev with
            | [ Configure _; Register _; Register _; Register _; Register _ ] -> Some()
            | Configure _ :: Register _ :: Register _ :: Register _ :: Register _ :: _ -> Some()
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
        
        let (|RegisterWhiteDefense|_|) input = 
            match input with
            | [ Configure _ ] -> Some([ Player.zero; Player.zero; Player.zero; Player.zero ])
            | _ -> None
        
        let (|RegisterWhiteAttack|_|) input = 
            match input with
            | [ Register a; Configure _ ] -> Some([ a; Player.zero; Player.zero; Player.zero ])
            | _ -> None
        
        let (|RegisterBlackDefense|_|) = 
            function 
            | [ Register b; Register a; Configure _ ] -> Some([ a; b; Player.zero; Player.zero ])
            | _ -> None
        
        let (|RegisterBlackAttack|_|) input = 
            match input with
            | [ Register c; Register b; Register a; Configure _ ] -> Some([ a; b; c; Player.zero ])
            | _ -> None
        
        let (|RegisteredPlayers|_|) input = 
            match input with
            | [ Register d; Register c; Register b; Register a; Configure _ ] -> Some("READY", [ a; b; c; d ], "Throw in the ball to start the game.")
            | RegisterBlackAttack(a) -> Some("Configure", a, "Register player for Black Attack position")
            | RegisterBlackDefense(a) -> Some("Configure", a, "Register player for Black Defense position")
            | RegisterWhiteAttack(a) -> Some("Configure", a, "Register player for White Attack position")
            | RegisterWhiteDefense(a) -> Some("Configure", a, "Register player for White Defense position")
            | _ -> None
