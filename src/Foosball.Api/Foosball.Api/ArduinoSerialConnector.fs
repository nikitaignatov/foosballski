namespace Foosball

module ArduinoSerialConnector = 
    open System.IO.Ports
    open Arduino
    
    type t = 
        { start : unit -> unit
          stop : unit -> unit
          test : unit -> unit
          close : unit -> unit }
    
    let connect (settings : Settings.sensor) f = 
        let port = new SerialPort(settings.com_port, settings.baud_rate)
        
        let read (sender : obj) (_) = 
            let reader = sender :?> SerialPort
            Arduino.t.Update(reader.ReadLine().Trim())
        
        let send (command : Command) = port.WriteLine(command.ToString().ToLower())
        
        let close() = 
            if port.IsOpen then 
                send Stop
                port.Close()
        
        let api = 
            { start = fun () -> send Start
              stop = fun () -> send Stop
              test = fun () -> send Test
              close = close }
        
        let rec prompt predicate = 
            let print f = 
                let value = f()
                printfn "%s" value
                value
            match print f with
            | value when predicate value -> api.close()
            | "start" -> 
                api.start()
                prompt predicate
            | "stop" -> 
                api.stop()
                prompt predicate
            | "test" -> 
                api.test()
                prompt predicate
            | value -> (prompt predicate)
        
        let exit x = x = "exit"
        
        let init() = 
            port.DataReceived.AddHandler(new SerialDataReceivedEventHandler(read))
            port.Open()
            Printf.printfn """
You are now connected, following commands are available:

start - will turn on the sensors.
stop  - turns off the sensors.
test  - reads current status of the sensors.
exit  - turns off the sensors and disconnects from the port.

            """
            api.start()
            prompt exit
        api, init
