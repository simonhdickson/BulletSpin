module SpaceExplorer.Physics

open SDLUtility
open System

let fireBullet gameState =
    match gameState.screen with
    | Level ({ player = { fireDirection = Some direction; nextFireTime = time } as player } as level)
        when time < DateTime.UtcNow ->
        let velocity =
            match direction with
            | FireVertical Up      -> { X = 0.; Y = -20. }
            | FireVertical Down    -> { X = 0.; Y = 20. }
            | FireHorizontal Left  -> { X = -20.; Y = 0. }
            | FireHorizontal Right -> { X = 20.; Y = 0. }
        let projectile = { texture = null; location = player.location; motion = Velocity velocity; owner = Player }
        { gameState with screen = Level { level with projectiles = Set.add projectile level.projectiles
                                                     player = { player with nextFireTime = DateTime.UtcNow.AddSeconds 0.25 } } }
    | _ -> gameState

let force step (horzontal, vertical) =
    let x =
        match horzontal with
        | Some Left  -> -10.
        | Some Right -> 10.
        | None       -> 0.
    let y =
        match vertical with
        | Some Up   -> -10.
        | Some Down -> 10.
        | None      -> 0.
    { X = x * step; Y = y * step }

let updatePlayer step (gameState:GameState) =
    gameState
    |> GameState.updatePlayer (fun p -> { p with location = p.location + force step p.moveDirection })

let applyMotion projectile step =
    match projectile.motion with
    | Velocity velocity -> projectile.location + (velocity * step)
    | _ -> projectile.location

let updateProjectiles step (gameState:GameState) =
    gameState
    |> GameState.updateProjectiles (Set.map (fun p -> { p with location = applyMotion p step }))

let update (gameState:GameState) =
    let dt = getTicks() - gameState.lastFrameTime
    let step = min (float dt) (1. / 30.)
    gameState
    |> updatePlayer step
    |> updateProjectiles step
    |> fireBullet
