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

let cleanUpProjectiles (gameState:GameState) =
    match gameState.screen with
    | Level level ->
        let projectiles =
            level.projectiles
            |> Set.filter (fun p -> p.location.X > -30. && p.location.Y > -30. && p.location.X < (double screenWidth) + 30. && p.location.Y < (double screenHeight) + 30.)
        let level = { level with projectiles = projectiles }
        { gameState with screen = Level level }
    | _ -> gameState

let handleCollisionDetection gameState =
    match gameState.screen with
    | Level { player = player; projectiles = projectiles } ->
        let hit =
            projectiles
            |> Set.exists (fun projectile ->
                let rectA = projectile.AsRect
                let rectB = player.AsRect
                rectA.Left < rectB.Right && rectA.Right > rectB.Left && rectA.Top < rectB.Bottom && rectA.Bottom > rectB.Top)
        if hit then
            { gameState with screen = GameOver }
        else
            gameState
    | _ -> gameState

let update (gameState:GameState) =
    let dt = getTicks() - gameState.lastFrameTime
    let step = min (float dt) (1. / 30.)
    gameState
    |> updatePlayer step
    |> updateProjectiles step
    |> cleanUpProjectiles
    |> handleCollisionDetection
    |> fireBullet
