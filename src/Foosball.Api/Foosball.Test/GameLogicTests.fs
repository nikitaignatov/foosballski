namespace FSharpTests

open System
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Foosball
open Foosball.Model

type Positive = 
    static member Double() = Arb.Default.Int32() |> Arb.mapFilter abs (fun t -> t >= 0)

type PositiveNonZero = 
    static member Double() = Arb.Default.Int32() |> Arb.mapFilter abs (fun t -> t > 0)

[<TestFixture>]
type Specification() = 
    let init = State.init
    let event = Configured(GoalLimitedTotal 1)
    
    let registerPlayer card p = 
        { card = Card card
          player = p
          goals = [] }
    
    let player name = { Player.zero with firstName = name }
    let register card = registerPlayer card (player (card + card)) |> RegisteredPlayer
    
    let events = 
        [ "a"; "b"; "c"; "d" ]
        |> List.map register
        |> List.append [ event ]
    
    let throwin color = 
        { color = color
          gametime = Duration.Zero
          timestamp = Time.Now }
        |> ThrownIn
    
    let goal speed = 
        { gametime = Duration.Zero
          timestamp = Time.Now
          speed = speed
          id = Guid.NewGuid() }
        |> fun x -> ScoredGoal(Black, x)
    
    [<Property>]
    member __.``configure game should track gameid`` (id : Guid) = 
        let state = New id
        match apply state event with
        | State.Configuration(gameId, _) -> gameId = id
        | _ -> false
    
    [<Property>]
    member __.``register first 3 players`` (id : Guid) = 
        let state = New id
        
        let events = 
            [ "a"; "b"; "c" ]
            |> List.map register
            |> List.append [ event ]
        match events |> List.fold apply state with
        | Registr(gameId, _, 
                  { whiteDefense = Some { card = Card "a"; player = { firstName = "aa" } }; whiteCenterForward = Some { card = Card "b"; player = { firstName = "bb" } }; 
                    blackDefense = Some { card = Card "c"; player = { firstName = "cc" } }; blackCenterForward = None }) -> gameId = id
        | _ -> false
    
    [<Property>]
    member __.``register all game players`` (id : Guid) = 
        let state = New id
        match events |> List.fold apply state with
        | WaitingThrowIn(_, 
                         { id = gameId; whiteDefense = { card = Card "a"; player = { firstName = "aa" } }, []; whiteCenterForward = { card = Card "b"; player = { firstName = "bb" } }, []; 
                           blackDefense = { card = Card "c"; player = { firstName = "cc" } }, []; blackCenterForward = { card = Card "d"; player = { firstName = "dd" } }, [] }) -> gameId = id
        | _ -> false
    
    [<Property>]
    member __.``start game`` (id : Guid) = 
        let state = New id
        let throwin = throwin Black
        match (events @ [ throwin ]) |> List.fold apply state with
        | Playing({ id = gameId; whiteDefense = { card = Card "a"; player = { firstName = "aa" } }, []; whiteCenterForward = { card = Card "b"; player = { firstName = "bb" } }, []; 
                    blackDefense = { card = Card "c"; player = { firstName = "cc" } }, []; blackCenterForward = { card = Card "d"; player = { firstName = "dd" } }, [] }) -> gameId = id
        | _ -> false
    
    [<Property>]
    member __.``score goal`` (id : Guid) = 
        let state = New id
        let throwin = throwin Black
        let goal = goal 5m
        match (events @ [ throwin ] @ [ goal ]) |> List.fold apply state with
        | GoalRegistration(color, meta, { id = gameId }) -> gameId = id
        | _ -> false
    
    [<Property(Arbitrary = [| typeof<PositiveNonZero> |])>]
    member __.``configure game to be limited by total number of goals`` (goals : int) = 
        let state = init
        let event = Configured(GoalLimitedTotal goals)
        match apply state event with
        | Configuration(_, GoalLimitedTotal x) -> x = goals
        | _ -> false
    
    [<Property(Arbitrary = [| typeof<PositiveNonZero> |])>]
    member __.``configure game to be limited by number of goals to be scored by one of the teams`` (goals : int) = 
        let state = init
        let event = Configured(GoalLimitedTeam goals)
        match apply state event with
        | Configuration(_, GoalLimitedTeam x) -> x = goals
        | _ -> false
    
    [<Property(Arbitrary = [| typeof<PositiveNonZero> |])>]
    member __.``configure game to be limited by game time.`` (seconds : int) = 
        let state = init
        let event = Configured(GameTimeLimited(Duration.FromSeconds(float seconds)))
        match apply state event with
        | Configuration(_, GameTimeLimited x) -> x.TotalSeconds = float seconds
        | _ -> false
    
    [<Property(Arbitrary = [| typeof<PositiveNonZero> |])>]
    member __.``configure game to be limited by time.`` (seconds : int) = 
        let state = init
        let event = Configured(TimeLimited(Duration.FromSeconds(float seconds)))
        match apply state event with
        | Configuration(_, TimeLimited x) -> x.TotalSeconds = float seconds
        | _ -> false
