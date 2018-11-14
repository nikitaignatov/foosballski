namespace FSharpTests

open NUnit.Framework
open System
open FsCheck
open FsCheck.NUnit
open Foosball
open Foosball.Model
open Foosball.GameLogic

[<TestFixture>]
type ``Game time specification``() = 
    
    let resetTime data = 
        { data with timestamp = Time.MinValue
                    timer = Duration.Zero }
    
    let throwin seconds = 
        let now = Time.MinValue.AddSeconds seconds
        { timestamp = now
          gametime = Duration.Zero.Add(now - Time.MinValue)
          color = Black }
        |> ThrownIn
    
    [<Property(Arbitrary = [| typeof<PositiveNonZero> |])>]
    member __.``Count gametime while playing`` (data : GameData, seconds : int) = 
        // arrange
        let seconds = float seconds
        let state = Playing(resetTime data)
        let expected = seconds
        // act
        match apply state (throwin seconds) with
        | State.Playing(game) -> game.timer.TotalSeconds = expected
        | _ -> false
    
    [<Property(Arbitrary = [| typeof<PositiveNonZero> |])>]
    member __.``Don't count gametime when the game is paused`` (data : GameData, seconds : int) = 
        // arrange
        let seconds = float seconds
        let state = Pause(resetTime data)
        let expected = Duration.Zero.TotalSeconds
        // act
        match apply state (throwin seconds) with
        | State.Playing(game) -> game.timer.TotalSeconds = expected
        | _ -> false
    
    [<Property(Arbitrary = [| typeof<PositiveNonZero> |])>]
    member __.``Don't count gametime when while waiting throwin`` (data : GameData, seconds : int) = 
        // arrange
        let seconds = float seconds
        let state = WaitingThrowIn(Black, resetTime data)
        let expected = Duration.Zero.TotalSeconds
        // act
        match apply state (throwin seconds) with
        | State.Playing(game) -> game.timer.TotalSeconds = expected
        | _ -> false
