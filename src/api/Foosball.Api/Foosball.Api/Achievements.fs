namespace Foosball

module Patters = 
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
    
    let (|TrowInAny|_|) = 
        function 
        | ThrowIn x | ThrowInAfterEscape x | ThrowInAfterGoal x -> Some(x)
        | _ -> None
    
    let (|NotEnded|) = 
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
    
    let (|GoalCount|_|) input = 
        input
        |> List.choose (|IsGoal|_|)
        |> List.length
        |> Some
    
    let (|GameStartTime|_|) input = 
        input
        |> List.choose (|IsStartGame|_|)
        |> List.map snd
        |> List.tryHead

module Achievements = 
    open Foosball.Model
    open Foosball.Arduino
    open System
    
    let (|GoalWithinSeconds|_|) seconds = Some()
    let (|LongBattle|_|) count input = Some()
