namespace Foosball

module Utils = 
    open System
    open Newtonsoft.Json
    open FSharp.Data
    
    let post url json = Http.RequestString(url, headers = [ (FSharp.Data.HttpRequestHeaders.ContentType HttpContentTypes.Json) ], body = TextRequest(json)) |> ignore
    
    let serialize input = 
        let settings = new JsonSerializerSettings()
        settings.ReferenceLoopHandling <- ReferenceLoopHandling.Ignore
        settings.PreserveReferencesHandling <- PreserveReferencesHandling.None
        settings.Formatting <- Formatting.Indented
        Newtonsoft.Json.JsonConvert.SerializeObject(input, settings)
    
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

module Settings = 
    open System
    open Newtonsoft.Json
    open Model
    
    type sensor = 
        { comPort : string
          baudrate : int }
    
    type CardUser = 
        { from : DateTimeOffset
          player : Player }
    
    type t = 
        { timestamp : Time
          signalr : string
          app : string
          slackWebHookUrl : string option
          sensor : sensor
          players : Map<string, CardUser list> }
        static member zero = 
            { timestamp = Time.Now
              signalr = "http://localhost:8070"
              app = "http://localhost:8080/"
              slackWebHookUrl = None
              players = Map.empty
              sensor = 
                  { comPort = "COM5"
                    baudrate = 250000 } }
    
    type private SettingsMessage = 
        | Save of t
        | Load of AsyncReplyChannel<t>
    
    type SettingsAgent() = 
        
        let load() = 
            let file = Utils.pathGetOrCreate "" "settings.json" (t.zero)
            IO.File.ReadAllText(file) |> JsonConvert.DeserializeObject<t>
        
        let save (s : t) = 
            let file = Utils.pathGetOrCreate "" "settings.json" (s)
            IO.File.WriteAllText(file, JsonConvert.SerializeObject({ s with timestamp = Time.Now }, Formatting.Indented))
        
        let agent = 
            MailboxProcessor<SettingsMessage>.Start(fun inbox -> 
                let rec loop old = 
                    async { 
                        let! msg = inbox.Receive()
                        match msg with
                        | Save data -> 
                            save (data)
                            return! loop data
                        | Load repl -> 
                            repl.Reply(old)
                            return! loop old
                    }
                loop (load()))
        
        member this.Save(msg : t) = agent.Post(Save msg)
        member this.Load() = agent.PostAndReply(Load)
    
    let registerPlayer (settings : SettingsAgent) card player = 
        let data = settings.Load()
        let registrations = data.players |> Map.tryFind card
        
        let registration = 
            { from = Time.Now
              player = player }
        
        let result = 
            match registrations |> Option.map (List.sortByDescending (fun x -> x.from)) with
            | None | Some [] -> [ registration ]
            | Some list -> registration :: list
        
        { data with players = data.players |> Map.add card result } |> settings.Save
        ()
    
    let (|PlayerFromCard|_|) (settings, card) = 
        if settings.players |> Map.containsKey card then 
            settings.players
            |> Map.find card
            |> List.sortByDescending (fun x -> x.from)
            |> List.tryHead
        else None
    
    let current = SettingsAgent()
