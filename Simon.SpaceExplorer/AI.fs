module SpaceExplorer.AI

open System

let update gameState =
    match gameState.screen with
    | Level ({ enemy = { nextFireTime = time } as enemy } as level)
        when time < DateTime.UtcNow ->
        let projectile1 = 
            { texture = null; location = enemy.location; motion = Velocity enemy.fireDirection; owner = Enemy }
        let projectile2 = 
            { texture = null; location = enemy.location; motion = Velocity (enemy.fireDirection.Rotate 180.); owner = Enemy }
        let enemy =
            { enemy with
                nextFireTime = DateTime.UtcNow.AddSeconds 0.35
                fireDirection = enemy.fireDirection.Rotate 10. } 
        let level =
            { level with
                projectiles = level.projectiles |> Set.add projectile1 |> Set.add projectile2
                enemy = enemy }
        { gameState with screen = Level level }
    | _ -> gameState
