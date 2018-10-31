namespace Foosball

module Model = 
    open System
    
    type Time = DateTimeOffset
    
    type Duration = TimeSpan
    
    type Reading = 
        { id : string
          speed : decimal
          timestamp : Time }
    
    type IntegratedAccount = 
        | Slack of username : string
    
    type Card = 
        | Card of string
    
    type Player = 
        { firstName : string
          lastName : string
          integrations : IntegratedAccount list }
        static member zero = 
            { firstName = "---"
              lastName = "---"
              integrations = [] }
    
    and TeamColor = 
        | Black
        | White
    
    and Team = 
        { attack : Player
          defense : Player
          color : TeamColor }
        
        static member zero color = 
            { attack = Player.zero
              defense = Player.zero
              color = color }
        
        static member create color attack defense = 
            { attack = attack
              defense = defense
              color = color }
        
        static member black = Team.zero TeamColor.Black
        static member white = Team.zero TeamColor.White
    
    and EventMetaData = 
        { team : Team
          speed : decimal
          timestamp : Time
          gametime : Duration
          id : Guid }
        member m.print event = sprintf "[%A]: %-25s [time: %O] [game time: %O] [speed: %.2fm/s %.2fkm/h]" m.team event (m.timestamp.ToString("HH:mm:ss")) (m.gametime) m.speed (m.speed * 3.6m)
    
    type Registration = 
        { card : Card
          player : Player
          goals : EventMetaData list }
    
    type GameConfig = 
        | GameTimeLimited of Duration
        | TimeLimited of Duration
        | GoalLimitedTotal of int
        | GoalLimitedTeam of int
    
    type GameEvent = 
        | Tick
        | Undo
        | Configure of GameConfig
        | Register of Registration
        | RegisterTeam of Team
        | Substitution of Team
        | StartGame of Team * Time
        | EndGame of time : Time * gametime : Duration
        | ScoredLastGoal of Player
        | Goal of EventMetaData
        | ThrowIn of EventMetaData
        | ThrowInAfterGoal of EventMetaData
        | ThrowInAfterEscape of EventMetaData
