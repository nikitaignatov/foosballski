namespace Foosball

module Pattern = 
    open Foosball.Model
    
    module Goal = 
        let (|IsGoal|_|) = 
            function 
            | Goal(color, meta) -> Some(color, meta)
            | _ -> None
        
        let (|NotGoal|_|) = 
            function 
            | IsGoal _ -> None
            | _ -> Some()
        
        let (|Goals|_|) a = a |> (List.choose (|IsGoal|_|) >> Some)
        
        let (|Speed|_|) = 
            function 
            | IsGoal(_, x) :: _ -> Some(x.speed)
            | _ -> None
        
        let (|Count|_|) = 
            function 
            | Goals x -> x |> (List.length >> Some)
            | _ -> None
        
        let (|GoalsInRow|_|) count = function 
            | Goals goals -> 
                goals
                |> List.map (fun (team, _) -> team)
                |> List.fold (fun (team, acc) v -> 
                       if acc >= count then team, acc
                       elif v = team then team, acc + 1
                       else v, 1) (Black, 0)
                |> fun (team, countInRow) -> 
                    if countInRow >= count then Some team
                    else None
        
        let rec (|DurationBetweenGoals|_|) (input : (TeamColor * GoalMetaData) list) = 
            match input with
            | [] -> None
            | (_, x) :: [] -> Some([ (x.gametime, x) ])
            | (_, x) :: [ (_, y) ] -> Some([ (x.gametime - y.gametime, x) ])
            | (_, x) :: DurationBetweenGoals((duration, y) :: tail) -> Some((x.gametime - y.gametime, x) :: (duration, y) :: tail)
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
            | StartGame(x, _) :: _, ThrowIn { color = y } when x.color = y -> (event :: state) |> Some
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
            | Playing -> None
            | _ -> Some()
        
        
        let (|GameStatus|) acc v = 
            match (acc, v) with
            | ((a, sa), x), Goal.IsGoal (t,_) when t = a -> ((a, int sa + 1), x)
            | (x, (b, sb)), Goal.IsGoal (t,_) when t = b -> (x, (b, int sb + 1))
            | _ -> acc
        
        let (|GameStatus2|) acc v = 
            match (acc, v) with
            | ((a, sa), x), Model.GameEvent.ScoredGoal(t, _) when t = a -> ((a, int sa + 1), x)
            | (x, (b, sb)), Model.GameEvent.ScoredGoal(t, _) when t = b -> (x, (b, int sb + 1))
            | _ -> acc
        
        let (|ContinueObserving|) = 
            function 
            | Model.GameCommand.EndGame _ :: Model.GameCommand.EndGame _ :: _ -> false
            | _ -> true
        
        //module End = 
        //    let (|ByGameTimeLimit|_|) ((now, time), config, state, event) = 
        //        match (config, state, event) with
        //        | GameTimeLimited durtion, _, _ when time >= durtion -> EndGame(now, time) :: state |> Some
        //        | _ -> None
            
        //    let (|ByTotalGoalCount|_|) (result, config, state, event) = 
        //        match (config, state, event) with
        //        | GoalLimitedTotal limit, Goal.Count count, _ when count = limit -> EndGame result :: state |> Some
        //        | _ -> None
            
        //    let (|ByTimeLimit|_|) ((result), config, state, event) = 
        //        let duration x = 
        //            List.last event |> function 
        //            | (Model.StartGame(_, x)) -> Time.Now.Subtract x
        //            | _ -> Duration.Zero
        //        match (config, state, event) with
        //        | TimeLimited limit, x, _ when duration x >= limit -> EndGame(result) :: state |> Some
        //        | _ -> None