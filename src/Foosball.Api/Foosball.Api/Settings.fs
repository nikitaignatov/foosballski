namespace Foosball

module Settings = 
    type sensor = 
        { com_port : string
          baud_rate : int }
    
    type t = 
        { signalr : string
          app : string
          sensor : sensor }
        static member zero = 
            { signalr = "http://localhost:8070"
              app = "http://localhost:8081/"
              sensor = 
                  { com_port = "COM3"
                    baud_rate = 250000 } }
