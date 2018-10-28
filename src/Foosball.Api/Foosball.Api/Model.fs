namespace Foosball

module Model = 
    open System
    
    type Time = DateTimeOffset
    
    type Duration = TimeSpan
    
    type Reading = 
        { id : string
          speed : decimal
          timestamp : Time }
    
    type Player = 
        { firstName : string
          lastName : string
          card : string
          goals : EventMetaData list }
        static member zero = 
            { firstName = "---"
              lastName = "---"
              card = ""
              goals = [] }
    
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
    
    type GameConfig = 
        | GameTimeLimited of Duration
        | TimeLimited of Duration
        | GoalLimitedTotal of int
        | GoalLimitedTeam of int
    
    type GameEvent = 
        | Tick
        | Undo
        | Configure of GameConfig
        | Register of Player
        | RegisterTeam of Team
        | Substitution of Team
        | StartGame of Team * Time
        | EndGame of time : Time * gametime : Duration
        | ScoredLastGoal of Player
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
            | _ -> sprintf "%A" m

module GameState = 
    open Model
    
    type t = 
        | Registration of Team * Team
        | Registered of Team * Team
        | NotConfigured of Team * Team
        | Configured of Team * Team * GameConfig
        | Playing
        | Paused
        | Ended
    
    let (|Configure|_|) = 
        function 
        | NotConfigured(a, b), GameEvent.Configure config -> Configured(a, b, config) |> Some
        | _ -> None
    
    let (|MissingDefender|_|) team = 
        match team with
        | { defense = d } when d = Player.zero -> Some team
        | _ -> None
    
    let (|MissingCenterForward|_|) team = 
        match team with
        | { attack = d } when d = Player.zero -> Some team
        | _ -> None
    
    let (|Register|_|) = 
        function 
        | Registration(MissingDefender a, b), GameEvent.Register player -> Registration({ a with defense = player }, b) |> Some
        | Registration(MissingCenterForward a, b), GameEvent.Register player -> Registration({ a with attack = player }, b) |> Some
        | Registration(a, MissingDefender b), GameEvent.Register player -> Registration(a, { b with defense = player }) |> Some
        | Registration(a, MissingCenterForward b), GameEvent.Register player -> Registered(a, { b with attack = player }) |> Some
        | Registered(a, b), _ -> NotConfigured(a, b) |> Some
        | _ -> None
    
    let (|RegistrationFlow|) = 
        function 
        | a & Registration(_) -> true
        | a & Registered _ -> true
        | _ -> false
    
    let (|ConfigurationFlow|) = 
        function 
        | a & NotConfigured(_) -> true
        | a & Configured _ -> true
        | _ -> false
    
    let apply state event = 
        match state, event with
        | Register state -> state
        | Configure state -> state
        | _ -> state
