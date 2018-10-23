namespace Foosball

module Arduino = 
    open System
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
        | Started
        | Stopped
        | PinReading of id : int * PinState
        | SensorReading of id : string * SensorState * time : int
    
    type t() = 
        static let event = (new Event<DateTimeOffset * Event>())
        static member Observable = event.Publish |> Observable.map (fun c -> c)
        static member Update msg = 
            try 
                let ev = JsonConvert.DeserializeObject<Event>(msg)
                // printfn "event: %A" ev
                event.Trigger(DateTimeOffset.Now, ev)
            with e -> 
                printfn "message: %s" msg
                printfn "error: %s" e.Message
            ()
