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
    
    and EventMetaData = 
        { team : Team
          speed : decimal
          timestamp : Time
          gametime : Duration
          id : Id }
    
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
          goals : EventMetaData list }
    
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
        static member update x = { x with timer = x.timer.Add(Time.Now - x.timestamp) } |> GameData.stamp
    
    type GameCommand = 
        | Tick
        | NewGame of Id
        | Undo
        | Configure of GameConfig
        | Register of Registration
        | RegisterTeam of Team
        | Substitution of Team
        | Swap of TeamColor
        | StartGame of Team * Time
        | EndGame of time : Time * gametime : Duration
        | ScoredLastGoal of Player
        | Goal of EventMetaData
        | ThrowIn of EventMetaData
        | ThrowInAfterGoal of EventMetaData
        | ThrowInAfterEscape of EventMetaData
    
    type GameEvent = 
        | RejectGoal
        | Paused
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
        | Registr of Id * GameConfig * Reg
        | Pause of GameData
        | WaitingThrowIn of TeamColor * GameData
        | Playing of GameData
        | GoalRegistration of TeamColor * GoalMetaData * GameData
        | Result of GameData
        static member zero = Zero
        static member init = Guid.NewGuid() |> New
    
    let apply (state : State) event = 
        match event with
        | Initialized id -> 
            match state with
            | Zero -> New(id)
            | _ -> ErrorState "Can only be applied in Zero state"
        | Configured conf -> 
            match state with
            | New(id) -> Configuration(id, conf)
            | _ -> ErrorState "Can only be configured when game is new."
        | Swapped(White) -> 
            match state with
            | (WaitingThrowIn(_, state)) & (WaitingThrowIn(_, { whiteDefense = wd; whiteCenterForward = wcf })) -> 
                { state with whiteDefense = wcf
                             whiteCenterForward = wd }
                |> fun c -> WaitingThrowIn(Black, c)
            | _ -> ErrorState "Can only be configured when game is new."
        | ThrownIn meta -> 
            match state with
            | WaitingThrowIn(color, state) when meta.color = color -> GameData.stamp { state with throwins = meta :: state.throwins } |> Playing
            | Playing state -> { state with throwins = meta :: state.throwins } |> Playing
            | _ -> ErrorState "Can only be configured when game is new."
        | Paused -> 
            match state with
            | Playing state -> GameData.update state |> Pause
            | _ -> ErrorState "Can only be configured when game is new."
        | ScoredGoal(color, meta) -> 
            match state with
            | Playing state -> GameData.update state |> fun x -> (color, meta, x) |> GoalRegistration
            | _ -> ErrorState "Can only be configured when game is new."
        | PlayerScoredLastGoal(p) -> 
            match state with
            | GoalRegistration(color, _, state) -> 
                WaitingThrowIn((if color = White then Black
                                else White), state)
            | _ -> ErrorState "Can only be configured when game is new."
        | RejectGoal -> 
            match state with
            | GoalRegistration(color, _, state) -> 
                WaitingThrowIn((if color = White then Black
                                else White), state)
            | _ -> ErrorState "Can only be configured when game is new."
        | RegisteredPlayer p -> 
            match state with
            | Configuration(id, config) -> Registr(id, config, { Reg.zero with whiteDefense = Some p })
            | (Registr(_, _, r)) & (Registr(id, config, { whiteCenterForward = None })) -> Registr(id, config, { r with whiteCenterForward = Some p })
            | (Registr(_, _, r)) & (Registr(id, config, { blackDefense = None })) -> Registr(id, config, { r with blackDefense = Some p })
            | (Registr(id, config, { blackDefense = Some bd; whiteCenterForward = Some wcf; whiteDefense = Some wd })) -> State.WaitingThrowIn(Black, GameData.start id config wd wcf bd p)
            | _ -> ErrorState "Can only be configured when game is new."
        | _ -> state
    
    let handle state = 
        let apply event = 
            match apply state event with
            | ErrorState s -> Error s
            | _ -> Ok event
        function 
        | NewGame id -> Initialized id |> apply
        | Configure data -> Configured data |> apply
        | Register data -> RegisteredPlayer data |> apply
        | ScoredLastGoal data -> PlayerScoredLastGoal data |> apply
        | Swap data -> Swapped data |> apply
        | Goal data -> 
            ScoredGoal(data.team.color, 
                       { speed = data.speed
                         gametime = data.gametime
                         timestamp = data.timestamp
                         id = data.id })
            |> apply
    
    type Aggregate<'TState, 'TCommand, 'TEvent> = 
        { zero : 'TState
          apply : 'TState -> 'TEvent -> 'TState
          exec : 'TState -> 'TCommand -> Result<'TEvent, string> }
    
    let makeHandler (aggregate : Aggregate<'TState, 'TCommand, 'TEvent>) (load : Id -> 'TEvent list, commit : Id * int -> ('TEvent -> unit)) = 
        fun (id, version) command -> 
            let state = load id |> List.fold aggregate.apply aggregate.zero
            let event = aggregate.exec state command
            match event with
            | Ok event -> event |> commit (id, version)
            | Error message -> printfn "%s" message
    
    type GameSubscription<'a>() = 
        static let event = (new Event<'a>())
        static member Observable = event.Publish |> Observable.map id
        static member Update msg = 
            try 
                event.Trigger(msg)
            with e -> 
                printfn "message: %A" msg
                printfn "error: %s" e.Message
            ()

module p = 
    open System
    open Model
    
    let agg = 
        { zero = Model.State.zero
          apply = Model.apply
          exec = Model.handle }
    
    let path id = Utils.pathGetOrCreate "games" (sprintf "game-%s.json" (id.ToString())) []
    
    let load (id : Model.Id) : Model.GameEvent list = 
        path id
        |> IO.File.ReadAllText
        |> Utils.deserialize<GameEvent list>
    
    let commit (id : Model.Id, v : int) (e : Model.GameEvent) = 
        let path = path id
        let events = load id
        let json = Utils.serialize (e :: events)
        IO.File.WriteAllText(path, json)
        GameSubscription<Model.GameEvent>.Update e
        ()
    
    let handler = makeHandler agg (load, commit)
    let id = Model.Id.NewGuid()
    
    GameSubscription<Model.GameEvent>.Observable |> Observable.subscribe (printfn "event from sub: %A")
    handler (id, 0) (Model.GameCommand.NewGame id)
    handler (id, 0) (Model.GameCommand.Configure(Model.GameConfig.GoalLimitedTotal 10))
    handler (id, 0) (Model.GameCommand.Register())
