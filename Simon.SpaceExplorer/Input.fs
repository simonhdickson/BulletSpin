module SpaceExplorer.Input

open SDLKeyboard

let handleMovement (player, gameState) =
    let horizontal =
        if gameState.pressedKeys |> Set.contains ScanCode.D then
            Some Right
        else if gameState.pressedKeys |> Set.contains ScanCode.A then
            Some Left
        else 
            None
    let vertical =
        if gameState.pressedKeys |> Set.contains ScanCode.W then
            Some Up
        else if gameState.pressedKeys |> Set.contains ScanCode.S then
            Some Down
        else
            None
    { player with moveDirection = horizontal, vertical }, gameState
    
let updateShots (player:Player, gameState) =
    if gameState.pressedKeys |> Set.contains ScanCode.Right then
        { player with fireDirection = Some <| FireHorizontal Right }, gameState
    else if gameState.pressedKeys |> Set.contains ScanCode.Left then
        { player with fireDirection = Some <| FireHorizontal Left }, gameState
    else if gameState.pressedKeys |> Set.contains ScanCode.Up then
        { player with fireDirection = Some <| FireVertical Up }, gameState
    else if gameState.pressedKeys |> Set.contains ScanCode.Down then
        { player with fireDirection = Some <| FireVertical Down }, gameState
    else
        { player with fireDirection = None }, gameState

let update gameState =
    match gameState.screen with
    | Level level -> 
        let player, gameState =
            (level.player, gameState)
            |> handleMovement
        { gameState with screen = Level { level with player = player } }
    | _ -> gameState
