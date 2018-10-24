namespace Foosball

module Model = 
    open System
    
    type Reading = 
        { id : string
          speed : decimal
          timestamp : DateTimeOffset }
    
    type Team = 
        | Black
        | White
    
    type EventMetaData = 
        { team : Team
          speed : decimal
          timestamp : DateTimeOffset }
        member m.print event = sprintf "[%A]: %-25s [time: %O] [speed: %.2f]" m.team event (m.timestamp.ToString("HH:mm:ss")) m.speed
    
    type t = 
        | Tick
        | Reset
        | StartGame of Team * DateTimeOffset
        | EndGame of time : DateTimeOffset * gametime : DateTimeOffset
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
