// load packages
#r @"..\packages\FSharp.Data.3.0.0\lib\net45\FSharp.Data.dll"
#r @"..\packages\System.Reactive.4.1.2\lib\net46\System.Reactive.dll"
//
#r @"..\packages\Dynamitey.2.0.9.136\lib\net40\Dynamitey.dll"
#r @"..\packages\FSharp.Control.Reactive.4.1.0\lib\net46\FSharp.Control.Reactive.dll"
#r @"..\packages\FSharp.Data.3.0.0\lib\net45\FSharp.Data.dll"
#r @"..\packages\FSharp.Interop.Dynamic.4.0.3.130\lib\net40\FSharp.Interop.Dynamic.dll"
#r @"..\packages\Newtonsoft.Json.11.0.2\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\PCSC.4.0.1\lib\net40\PCSC.dll"
#r @"..\packages\PCSC.Iso7816.4.0.1\lib\net40\PCSC.Iso7816.dll"
#r @"..\packages\Owin.1.0\lib\net40\Owin.dll"
#r @"..\packages\Microsoft.AspNet.SignalR.Core.2.3.0\lib\net45\Microsoft.AspNet.SignalR.Core.dll"
#r @"..\packages\Microsoft.Owin.4.0.0\lib\net451\Microsoft.Owin.dll"
#r @"..\packages\Microsoft.AspNet.SignalR.Core.2.3.0\lib\net45\Microsoft.AspNet.SignalR.Core.dll"
#r @"..\packages\Microsoft.Owin.4.0.0\lib\net451\Microsoft.Owin.dll"
#r @"..\packages\Microsoft.Owin.Cors.4.0.0\lib\net451\Microsoft.Owin.Cors.dll"
#r @"..\packages\Microsoft.AspNet.Cors.5.2.6\lib\net45\System.Web.Cors.dll"
#r @"..\packages\Microsoft.Owin.Diagnostics.4.0.0\lib\net451\Microsoft.Owin.Diagnostics.dll"
#r @"..\packages\Microsoft.Owin.FileSystems.4.0.0\lib\net451\Microsoft.Owin.FileSystems.dll"
#r @"..\packages\Microsoft.Owin.Host.HttpListener.4.0.0\lib\net451\Microsoft.Owin.Host.HttpListener.dll"
#r @"..\packages\Microsoft.Owin.Hosting.4.0.0\lib\net451\Microsoft.Owin.Hosting.dll"
#r @"..\packages\Microsoft.Owin.Security.4.0.0\lib\net451\Microsoft.Owin.Security.dll"
#r @"..\packages\Microsoft.Owin.StaticFiles.4.0.0\lib\net451\Microsoft.Owin.StaticFiles.dll"
// TODO: Send refs to F# interactive
// load files
#load "Arduino.fs"
#load "ArduinoSerialConnector.fs"
#load "Model.fs"
#load "Sensor.fs"
#load "Achievement.fs"
#load "ConsolePrinter.fs"
#load "GameLogic.fs"
#load "Signalr.fs"
#load "Nfc.fs"

open FSharp.Control.Reactive
open Foosball
open Foosball.Model
open Nfc.Reader
open PCSC.Monitoring
open Foosball

let print = function 
    | a -> printfn "%O" a
let obs2, (monitor : SCardMonitor) = CardReader.execute()

let registrationNfc = 
    obs2
    |> Observable.map (fun x -> 
           match x with
           | Nfc.Reader.CardReader.Inserted x -> (GameEvent.Register { Player.zero with card = x }) |> Some
           | _ -> None)
    |> Observable.choose id

let registration = 
    [ "a"; "b"; "c"; "d" ]
    |> List.map (fun x -> (GameEvent.Register { Player.zero with card = x }))
    |> List.map Observable.single
    |> Observable.mergeSeq

let ticks = (Observable.interval (Duration.FromSeconds 1.) |> Observable.map (fun _ -> GameEvent.Tick))
let scanner stream = stream |> Observable.scanInit (GameState.Registration(Team.black, Team.white)) GameState.apply
let filter stream = stream |> Observable.takeWhile (GameState.``|RegistrationFlow|``)

let flow = 
    ticks
    |> Observable.merge registration
    |> scanner
    |> Observable.distinct

let result = flow |> Observable.subscribe (print)

result.Dispose()
monitor.Cancel()
