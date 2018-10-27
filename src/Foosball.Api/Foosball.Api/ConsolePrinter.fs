namespace Foosball

module ConsolePrinter = 
    open Foosball.Model
    
    let mapper acc v = 
        match (acc, v) with
        | ((a, sa), x), Pattern.IsGoal t when t.team = a -> (a, sa + 1), x
        | (x, (b, sb)), Pattern.IsGoal t when t.team = b -> x, (b, sb + 1)
        | _ -> acc
    
    let status = 
        function 
        | Pattern.TrowInAny _ :: _ -> "PLAYING"
        | EndGame _ :: _ -> "ENDED"
        | _ -> "PAUSED"
    
    let achievements = 
        function 
        | Pattern.GoalWithinSeconds (3.) x -> sprintf "Goal within 10 seconds: %A" x
        | _ -> "No Achievemtns"
    
    let printGame title (state : t list) = 
        let summary = state |> List.fold (mapper) ((Team.Black, 0), (Team.White, 0))
        state
        |> List.rev
        |> List.mapi (sprintf "%-3d: %O")
        |> fun list -> (sprintf "-- %s -----------------" title) :: (sprintf "-- %A -> %s" summary (status state)) :: (sprintf "-- Achievements: %A " achievements) :: list
        |> List.iter (printfn "%s")
