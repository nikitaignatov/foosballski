namespace Foosball

module SlackIntegration = 
    open FSharp.Data
    open Model
    
    type private hook = JsonProvider< """ {
        "text":"Live Game",
        "attachments": [{
            "actions": [{
                "type": "button",
                "text": "Bet on White",
                "url": "link" }],
            "fields": [{
                "title": "White",
                "short": true }] }]
    } """ >
    
    let private startJson x y [ wa; ba; wd; bd ] = 
        let betOn team url = hook.Action("button", (sprintf "Bet on %s" team), url)
        let field text = hook.Field(text, true)
        
        let fields = 
            [| field "White"
               field "Black"
               field (sprintf "Center Forward: <@%s>" wa)
               field (sprintf "Center Forward: <@%s>" ba)
               field (sprintf "Defense: <@%s>" wd)
               field (sprintf "Defense: <@%s>" bd) |]
        
        let actions = 
            [| betOn "White" x
               betOn "Black" y |]
        
        hook.Root("", [| hook.Attachment(actions, fields) |])
    
    let postGameStart post (players : Player list) (settings : Settings.t) = 
        let extract p = 
            match p.integrations with
            | [ Slack id ] -> Some id
            | _ -> None
        match (players |> List.choose (extract), settings.slackWebHookUrl) with
        | [], _ | _, None -> ()
        | [ a; b; c; d ], Some url -> 
            let bet id team = (sprintf "%s/bet/%s/%s" settings.app id team)
            let json = startJson (bet "_" "white") (bet "_" "black") [ a; b; c; d ]
            post url (json.JsonValue.ToString())
        | _ -> ()
    
    let tryPostGameStart post players settings = 
        try 
            postGameStart post players settings |> Ok
        with e -> Error(e)
