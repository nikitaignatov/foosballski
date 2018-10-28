namespace Foosball

module ConsolePrinter = 
    open Foosball.Model
    
    let mapper acc v = 
        match (acc, v) with
        | ((a, sa), x), Pattern.Goal.IsGoal t when t.team.color = a -> (a, sa + 1), x
        | (x, (b, sb)), Pattern.Goal.IsGoal t when t.team.color = b -> x, (b, sb + 1)
        | _ -> acc
    
    let status = 
        function 
        | Pattern.TrowInAny _ :: _ -> "PLAYING"
        | EndGame _ :: _ -> "ENDED"
        | _ -> "PAUSED"
    
    let printGame title (state : GameEvent list) = 
        let summary = state |> List.fold (mapper) ((Team.black.color, 0), (Team.white.color, 0))
        state
        |> List.rev
        |> List.mapi (fun i a -> sprintf "%-3d: %O" i (a.GetType()))
        |> fun list -> (sprintf "-- %s -----------------" title) :: (sprintf "-- %A -> %s" summary (status state)) :: list
        |> List.iter (printfn "%s")
