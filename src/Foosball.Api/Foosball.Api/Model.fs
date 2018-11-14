namespace Foosball

module Utils = 
    open System
    open Newtonsoft.Json
    open FSharp.Data
    
    let settings = 
        let settings = new JsonSerializerSettings()
        settings.ReferenceLoopHandling <- ReferenceLoopHandling.Ignore
        settings.PreserveReferencesHandling <- PreserveReferencesHandling.None
        settings.Formatting <- Formatting.Indented
        settings
    
    let post url json = Http.RequestString(url, headers = [ (FSharp.Data.HttpRequestHeaders.ContentType HttpContentTypes.Json) ], body = TextRequest(json)) |> ignore
    let serialize input = Newtonsoft.Json.JsonConvert.SerializeObject(input, settings)
    let deserialize<'a> input = Newtonsoft.Json.JsonConvert.DeserializeObject<'a>(input, settings)
    
    let pathGetOrCreate dir file input = 
        let appData = Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData)
        let appFolder = IO.Path.Combine(appData, "foosballski", dir)
        let file = IO.Path.Combine(appFolder, file)
        if IO.Directory.Exists(appFolder) |> not then IO.Directory.CreateDirectory(appFolder) |> ignore
        if IO.File.Exists(file) |> not then 
            IO.File.Create(file).Close()
            let json = JsonConvert.SerializeObject(input, Formatting.Indented)
            IO.File.WriteAllText(file, json)
        file

module Model = 
    open System
    
    type Id = System.Guid
    
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
    
    and ThrowinMetaData = 
        { color : TeamColor
          timestamp : Time
          gametime : Duration }
    
    and GoalMetaData = 
        { speed : decimal
          timestamp : Time
          gametime : Duration
          id : Id }
    
    type Registration = 
        { card : Card
          player : Player
          goals : GoalMetaData list }
    
    type GameConfig = 
        | GameTimeLimited of Duration
        | TimeLimited of Duration
        | GoalLimitedTotal of int
        | GoalLimitedTeam of int
    
    type Reg = 
        { whiteDefense : Registration option
          blackDefense : Registration option
          whiteCenterForward : Registration option
          blackCenterForward : Registration option }
        static member zero = 
            { whiteDefense = None
              whiteCenterForward = None
              blackDefense = None
              blackCenterForward = None }
    
    type GameData = 
        { id : Id
          config : GameConfig
          started : Time
          timer : Duration
          timestamp : Time
          throwins : ThrowinMetaData list
          whiteDefense : Registration * GoalMetaData list
          blackDefense : Registration * GoalMetaData list
          whiteCenterForward : Registration * GoalMetaData list
          blackCenterForward : Registration * GoalMetaData list }
        
        static member start id conf a b c d = 
            { id = id
              config = conf
              started = Time.Now
              timestamp = Time.Now
              timer = Duration.Zero
              throwins = []
              whiteDefense = a, []
              whiteCenterForward = b, []
              blackDefense = c, []
              blackCenterForward = d, [] }
        
        static member stamp (x : GameData) = { x with timestamp = Time.Now }
        static member update x = 
            let time = Time.Now
            { x with timer = x.timer.Add(time - x.timestamp)
                     timestamp = time }
    
    type GameCommand = 
        | Tick
        | NewGame of Id
        | EndGame
        | Undo
        | Configure of GameConfig
        | Register of Registration
        | RegisterTeam of Team
        | Substitution of Team
        | Swap of TeamColor
        | StartGame of Team * Time
        | ScoredLastGoal of Player
        | Goal of TeamColor * GoalMetaData
        | ThrowIn of ThrowinMetaData
        | ThrowInAfterGoal of ThrowinMetaData
        | ThrowInAfterEscape of ThrowinMetaData
    
    type GameEvent = 
        | RejectGoal
        | Paused of Time * Duration
        | Initialized of Id
        | Configured of GameConfig
        | RegisteredPlayer of Registration
        | RegisteredTeam of Team
        | Substituted of Team
        | Swapped of TeamColor
        | StartedGame of Team * Time
        | EndedGame of time : Time * gametime : Duration
        | PlayerScoredLastGoal of Player
        | ScoredGoal of TeamColor * GoalMetaData
        | ThrownIn of ThrowinMetaData
    
    type State = 
        | ErrorState of string
        | Zero
        | New of Id
        | Configuration of Id * GameConfig
        | Registration of Id * GameConfig * Reg
        | Pause of GameData
        | WaitingThrowIn of TeamColor * GameData
        | Playing of GameData
        | GoalRegistration of TeamColor * GoalMetaData * GameData
        | Result of GameData
        static member zero = Zero
        static member init = Guid.NewGuid() |> New
