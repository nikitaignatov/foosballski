namespace Foosball

module ConsolePrinter = 
    open Foosball.Model
    open Foosball.Patters
    
    let printGame title (state : t list) = 
        let mapper acc v = 
            match (acc, v) with
            | ((a, sa), x), IsGoal team when team = a -> (a, sa + 1), x
            | (x, (b, sb)), IsGoal team when team = b -> x, (b, sb + 1)
            | _ -> acc
        
        let status = 
            match state with
            | Patters.TrowInAny _ :: _ -> "PLAYING"
            | EndGame _ :: _ -> "ENDED"
            | _ -> "PAUSED"
        
        let summary = state |> List.fold (mapper) ((Black, 0), (White, 0))
        state
        |> List.rev
        |> List.mapi (sprintf "%-3d: %O")
        |> fun list -> (sprintf "-- %s -----------------" title) :: (sprintf "-- %A -> %s" summary status) :: list
        |> List.iter (printfn "%s")
