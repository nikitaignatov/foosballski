namespace Foosball

module Achievements = 
    open Foosball.Model
    open Foosball.Arduino
    open System
    
    let (|IsGoal|_|) = 
        function 
        | Goal { team = t } -> Some(t)
        | _ -> None
    
    let (|GoalsInRow|_|) count input = 
        input
        |> List.choose (|IsGoal|_|)
        |> List.fold (fun (team, acc) v -> 
               if acc >= count then team, acc
               elif v = team then team, acc + 1
               else v, 1) (Black, 0)
        |> fun (team, countInRow) -> 
            if countInRow >= count then Some team
            else None
