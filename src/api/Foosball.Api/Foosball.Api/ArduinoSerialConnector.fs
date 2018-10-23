namespace Foosball

module ArduinoSerialConnector = 
    open System.IO.Ports
    open Arduino
    
    let connect (port) f = 
        let port = new SerialPort(port, 9600)
        
        let read (sender : obj) (_) = 
            let reader = sender :?> SerialPort
            Arduino.t.Update(reader.ReadLine().Trim())
        
        let send (command : Command) = port.WriteLine(command.ToString().ToLower())
        let start() = send Start
        let stop() = send Stop
        let test() = send Test
        
        let close() = 
            if port.IsOpen then 
                stop()
                port.Close()
        
        let rec prompt predicate = 
            let print f = 
                let value = f()
                printfn "%s" value
                value
            match print f with
            | value when predicate value -> close()
            | "start" -> 
                start()
                prompt predicate
            | "stop" -> 
                stop()
                prompt predicate
            | "test" -> 
                test()
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
            prompt exit
        init()
