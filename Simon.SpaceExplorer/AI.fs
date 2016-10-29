module SpaceExplorer.AI

open System

let update gameState =
    match gameState.screen with
    | Level ({ enemy = { nextFireTime = time } as enemy } as level)
        when time < DateTime.UtcNow ->
        match enemy.fireState.fireMode with
        | MultipleFire angles ->
            let projectiles =
                angles |> Set.map (fun a -> { texture = null; location = enemy.location; motion = Velocity (enemy.fireDirection.Rotate a); owner = Enemy })
            let enemy =
                { enemy with
                    nextFireTime = DateTime.UtcNow.AddSeconds enemy.fireState.delay
                    fireDirection = enemy.fireDirection.Rotate enemy.fireState.nextShotDegrees } 
            let level =
                { level with
                    projectiles = level.projectiles |> Set.union projectiles
                    enemy = enemy }
            { gameState with screen = Level level }
    | _ -> gameState
