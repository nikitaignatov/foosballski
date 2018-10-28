namespace Foosball

module Pattern = 
    open Foosball.Model
    open Foosball.Arduino
    open System
    
    let (|IsGoal|_|) = 
        function 
        | Goal meta -> Some(meta)
        | _ -> None
    
    let (|NotGoal|_|) = 
        function 
        | Goal _ -> None
        | _ -> Some()
    
    let (|AllPlayersRegistered|_|) input = 
        printfn "AllPlayersRegistered: %A" (input |> List.rev)
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
    
    let (|RegisterAllPlayers|_|) (state, event) = 
        match (state, event) with
        | MissingOnePlayer(wd, wa, bd), Register ba -> 
            let white = Team.create (White) wa wd
            let black = Team.create (Black) ba bd
            let start = StartGame(black, Time.Now)
            start :: RegisterTeam black :: RegisterTeam white :: event :: state |> Some
        | _ -> None
    
    let (|RegisterWhiteDefense|_|) input = 
        match input with
        | [ Configure _ ] -> Some("CONFIGURATION", [ Player.zero; Player.zero; Player.zero; Player.zero ], "Register player for White Defense position")
        | _ -> None
    
    let (|RegisterWhiteAttack|_|) input = 
        match input with
        | [ Register a; Configure _ ] -> Some("CONFIGURATION", [ a; Player.zero; Player.zero; Player.zero ], "Register player for White Attack position")
        | _ -> None
    
    let (|RegisterBlackDefense|_|) input = 
        match input with
        | [ Register b; Register a; Configure _ ] -> Some("CONFIGURATION", [ a; b; Player.zero; Player.zero ], "Register player for Black Defense position")
        | _ -> None
    
    let (|RegisterBlackAttack|_|) input = 
        match input with
        | [ Register c; Register b; Register a; Configure _ ] -> Some("CONFIGURATION", [ a; b; c; Player.zero ], "Register player for Black Attack position")
        | _ -> None
    
    let (|RegisteredPlayers|_|) input = 
        match input with
        | [ Register d; Register c; Register b; Register a; Configure _ ] -> Some("READY", [ a; b; c; d ], "Throw in the ball to start the game.")
        | RegisterBlackAttack(a) -> Some(a)
        | RegisterBlackDefense(a) -> Some(a)
        | RegisterWhiteAttack(a) -> Some(a)
        | RegisterWhiteDefense(a) -> Some(a)
        | _ -> None
    
    let (|GoalsInRow|_|) count input = 
        input
        |> List.choose (|IsGoal|_|)
        |> List.map (fun { team = team } -> team)
        |> List.fold (fun (team, acc) v -> 
               if acc >= count then team, acc
               elif v.color = team then team, acc + 1
               else v.color, 1) (TeamColor.Black, 0)
        |> fun (team, countInRow) -> 
            if countInRow >= count then Some team
            else None
    
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
    
    let (|Goals|_|) input = 
        input
        |> List.choose (|IsGoal|_|)
        |> Some
    
    let (|GoalCount|_|) = 
        function 
        | Goals x -> 
            x
            |> List.length
            |> Some
        | _ -> None
    
    let (|GameStartTime|_|) input = 
        input
        |> List.choose (|IsStartGame|_|)
        |> List.map snd
        |> List.tryHead
    
    let rec (|DurationBetweenGoals|_|) = 
        function 
        | [] -> None
        | x :: [] -> Some([ (x.gametime, x) ])
        | x :: [ y ] -> Some([ (x.gametime - y.gametime, x) ])
        | x :: DurationBetweenGoals((duration, y) :: tail) -> Some((x.gametime - y.gametime, x) :: (duration, y) :: tail)
        | _ -> None
    
    let (|NumberOfBallEscapes|_|) = Some()
    
    let (|GoalWithinSeconds|_|) seconds list = 
        match list with
        | IsGoal _ :: _ & Goals(DurationBetweenGoals((duration, x) :: tail)) when duration.TotalSeconds <= seconds -> Some((duration, x) :: tail)
        | _ -> None
    
    let (|GoalSpeed|_|) = 
        function 
        | IsGoal x :: _ -> Some(x.speed)
        | _ -> None
    
    let (|GameStatus|) acc v = 
        match (acc, v) with
        | ((a, sa), x), IsGoal t when t.team = a -> ((a, int sa + 1), x)
        | (x, (b, sb)), IsGoal t when t.team = b -> (x, (b, int sb + 1))
        | _ -> acc

module Achievement = 
    open Foosball.Model
    open Foosball.Arduino
    open System
    
    let (|FastGoalsInRow|_|) consecutive = None
    let (|BallEscapedMoreThan|_|) times = None
    let (|LongBattle|_|) = None
    
    /// goal with very slow speed
    let (|HowCouldYouMissThat|_|) = 
        function 
        | Pattern.GoalSpeed speed when speed < 1m -> Some(speed)
        | _ -> None
    
    /// goal speed > 28 km/h
    let (|MachOne|_|) = 
        function 
        | Pattern.GoalSpeed speed when speed > 8m -> Some(speed)
        | _ -> None
    
    /// goal speed > 43 km/h
    let (|MachTwo|_|) = 
        function 
        | Pattern.GoalSpeed speed when speed > 12m -> Some(speed)
        | _ -> None
    
    /// goal speed > 64 km/h
    let (|MachThree|_|) = 
        function 
        | Pattern.GoalSpeed speed when speed > 18m -> Some(speed)
        | _ -> None
    
    /// goal speed > 72 km/h
    let (|SpeedOfLight|_|) = 
        function 
        | Pattern.GoalSpeed speed when speed > 20m -> Some(speed)
        | _ -> None
