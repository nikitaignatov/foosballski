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
    
    let (|GoalsInRow|_|) count input = 
        input
        |> List.choose (|IsGoal|_|)
        |> List.map (fun { team = team } -> team)
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
        | Goals(DurationBetweenGoals((duration, x) :: tail)) when duration.TotalSeconds <= seconds -> Some((duration, x) :: tail)
        | _ -> None

module Achievement = 
    open Foosball.Model
    open Foosball.Arduino
    open System
    
    let (|FastGoalsInRow|_|) consecutive = None
    let (|BallEscapedMoreThan|_|) times = None
    let (|LongBattle|_|) = None
    
    /// goal with very slow speed
    let (|HowCouldYouMissThat|_|) = None
    
    /// goal speed > 50 km/h
    let (|MachOne|_|) = None
    
    /// goal speed > 75 km/h
    let (|MachTwo|_|) = None
    
    /// goal speed > 100 km/h
    let (|MachThree|_|) = None
    
    /// goal speed > 130 km/h
    let (|SpeedOfLight|_|) = None
