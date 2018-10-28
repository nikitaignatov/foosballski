namespace Foosball

module Achievement = 
    let (|FastGoalsInRow|_|) consecutive = None
    let (|BallEscapedMoreThan|_|) times = None
    let (|LongBattle|_|) = None
    
    /// goal with very slow speed
    let (|HowCouldYouMissThat|_|) = 
        function 
        | Pattern.Goal.Speed speed when speed < 1m -> Some(speed)
        | _ -> None
    
    /// goal speed > 28 km/h
    let (|MachOne|_|) = 
        function 
        | Pattern.Goal.Speed speed when speed > 8m -> Some(speed)
        | _ -> None
    
    /// goal speed > 43 km/h
    let (|MachTwo|_|) = 
        function 
        | Pattern.Goal.Speed speed when speed > 12m -> Some(speed)
        | _ -> None
    
    /// goal speed > 64 km/h
    let (|MachThree|_|) = 
        function 
        | Pattern.Goal.Speed speed when speed > 18m -> Some(speed)
        | _ -> None
    
    /// goal speed > 72 km/h
    let (|SpeedOfLight|_|) = 
        function 
        | Pattern.Goal.Speed speed when speed > 20m -> Some(speed)
        | _ -> None
