module SpaceExplorer.AI

open System

let update gameState =
    match gameState.screen with
    | Level ({ enemy = { fireState = ({ stateChange = time }) } as enemy } as level)
        when time < DateTime.UtcNow ->
        let fireState = List.random gameState.chaos fireStates
        let enemy =
            { enemy with fireState = { fireState with stateChange = DateTime.UtcNow.AddSeconds 5. } }
        let level =
            { level with enemy = enemy }
        { gameState with screen = Level level }
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
