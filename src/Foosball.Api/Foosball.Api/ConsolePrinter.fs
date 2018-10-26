namespace Foosball

module ConsolePrinter = 
    open Foosball.Model
    open Foosball.Patters
    
    let printGame title (state : t list) = 
        let mapper acc v = 
            match (acc, v) with
            | ((a, sa), x), IsGoal t when t.team = a -> (a, sa + 1), x
            | (x, (b, sb)), IsGoal t when t.team = b -> x, (b, sb + 1)
            | _ -> acc
        
        let status = 
            match state with
            | Patters.TrowInAny _ :: _ -> "PLAYING"
            | EndGame _ :: _ -> "ENDED"
            | _ -> "PAUSED"
        let achievements = 
            match state with
            | Achievements.GoalWithinSeconds (3.) x-> sprintf "Goal within 10 seconds: %A" x
            | _ -> "No Achievemtns"
        
        let summary = state |> List.fold (mapper) ((Team.Black, 0), (Team.White, 0))
        state
        |> List.rev
        |> List.mapi (sprintf "%-3d: %O")
        |> fun list -> (sprintf "-- %s -----------------" title) :: (sprintf "-- %A -> %s" summary status) :: (sprintf "-- Achievements: %A " achievements) :: list
        |> List.iter (printfn "%s")
