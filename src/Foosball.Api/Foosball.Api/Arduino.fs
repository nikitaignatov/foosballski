namespace Foosball

module Arduino = 
    open Newtonsoft.Json
    
    type PinState = 
        | On
        | Off
    
    type SensorState = 
        | Connected
        | Disconnected
    
    type Command = 
        | Start
        | Stop
        | Test
    
    type Event = 
        | Reset
        | Started
        | Stopped
        | PinReading of id : int * PinState
        | SensorReading of id : string * SensorState * time : int64
    
    type t() = 
        static let event = (new Event<Model.Time * Event>())
        static member Observable = event.Publish |> Observable.map id
        static member Update msg = 
            try 
                let ev = JsonConvert.DeserializeObject<Event>(msg)
                // printfn "event: %A" ev
                event.Trigger(Model.Time.Now, ev)
            with e -> 
                printfn "message: %s" msg
                printfn "error: %s" e.Message
            ()
