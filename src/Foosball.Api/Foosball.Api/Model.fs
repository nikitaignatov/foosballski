namespace Foosball

module Model = 
    open System
    
    type Time = DateTimeOffset
    type Duration = TimeSpan
    
    type Reading = 
        { id : string
          speed : decimal
          timestamp : Time }
    
    type Team = 
        | Black
        | White
    
    type EventMetaData = 
        { team : Team
          speed : decimal
          timestamp : Time
          gametime : Duration }
        member m.print event = sprintf "[%A]: %-25s [time: %O] [game time: %O] [speed: %.2f]" m.team event (m.timestamp.ToString("HH:mm:ss")) (m.gametime) m.speed
    
    type GameConfig = 
        | GameTimeLimited of Duration
        | TimeLimited of Duration
        | GoalLimitedTotal of int
        | GoalLimitedTeam of int
    
    type t = 
        | Tick
        | Reset
        | StartGame of Team * Time
        | EndGame of time : Time * gametime : Duration
        | Goal of EventMetaData
        | ThrowIn of EventMetaData
        | ThrowInAfterGoal of EventMetaData
        | ThrowInAfterEscape of EventMetaData
        override m.ToString() = 
            match m with
            | Goal x -> x.print "Goal"
            | ThrowIn x -> x.print "Throw In"
            | ThrowInAfterGoal x -> x.print "Throw In After Goal"
            | ThrowInAfterEscape x -> x.print "Throw In After Escape"
            | Tick | Reset | StartGame _ | EndGame _ -> sprintf "%A" m
