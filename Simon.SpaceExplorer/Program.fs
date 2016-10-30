module SpaceExplorer.Program

open System.Threading

open SDLUtility
open SDLPixel
open SDLRender
open SDLKeyboard
open SDLGameController
open SDLGeometry

open SpaceExplorer.Common

let rec eventPump (renderHandler:'TState->unit) (eventHandler:SDLEvent.Event->'TState->'TState option) (update:'TState->'TState) (state:'TState) : unit =
    match SDLEvent.pollEvent() with
    | Some(SDLEvent.Other(_)) ->   
        eventPump renderHandler eventHandler update state
    | Some event ->
        match state |> eventHandler event with
        | Some newState -> eventPump renderHandler eventHandler update newState
        | None -> ()
    | None -> 
        let state = update state
        renderHandler state
        eventPump renderHandler eventHandler update state

let handleEvent (event:SDLEvent.Event) (state:GameState) : GameState option =
    match event with
    | SDLEvent.KeyDown keyDetails when keyDetails.Keysym.Scancode = ScanCode.Escape ->
        None
    | SDLEvent.Quit _ -> 
        None
    | SDLEvent.ControllerButtonDown event  ->
        if event.Which = 0 then 
            Some({ state with controllers = Set.add (enum<ControllerButton>(int event.Button)) (fst state.controllers), (snd state.controllers) } )
        else
            Some({ state with controllers = (fst state.controllers), Set.add (enum<ControllerButton>(int event.Button))(snd state.controllers) } )
    | SDLEvent.ControllerButtonUp event  ->
        if event.Which = 0 then 
            Some({ state with controllers = Set.remove (enum<ControllerButton>(int event.Button)) (fst state.controllers), (snd state.controllers) } )
        else
            Some({ state with controllers = (fst state.controllers), Set.remove (enum<ControllerButton>(int event.Button))(snd state.controllers) } )
    | SDLEvent.KeyDown keyDetails -> 
        Some( { state with pressedKeys = Set.add keyDetails.Keysym.Scancode state.pressedKeys} )
    | SDLEvent.KeyUp keyDetails -> 
        Some( { state with pressedKeys = Set.remove keyDetails.Keysym.Scancode state.pressedKeys} )
    | _ -> Some state

let render(context:RenderingContext) (state:GameState) =
    context.renderer |> SDLRender.clear |> ignore

    context.surface
    |> SDLSurface.fillRect None { Red=0uy; Green=0uy; Blue=0uy; Alpha=255uy }
    |> ignore
    
    context.texture
    |> SDLTexture.update None context.surface
    |> ignore

    context.renderer |> SDLRender.copy context.texture None None |> ignore
    
    match state.screen with
    | Level level ->
        level.projectiles
        |> Set.iter (fun i -> context.renderer |> SDLRender.drawRect i.AsRect |> ignore)
        context.renderer |> SDLRender.setDrawColor (0uy, 255uy, 0uy, 255uy) |> ignore
        context.renderer |> SDLRender.drawRect level.player.AsRect |> ignore
        context.renderer |> SDLRender.setDrawColor (255uy, 0uy, 0uy, 255uy) |> ignore
        context.renderer |> SDLRender.drawRect level.enemy.AsRect |> ignore
        context.renderer |> SDLRender.setDrawColor (0uy, 0uy, 255uy, 255uy) |> ignore
    | _ -> ()

    context.renderer |> SDLRender.present
    
let update =
    Input.update
    >> AI.update
    >> Physics.update

[<EntryPoint>]
let main argv = 
    use system = new SDL.System(SDL.Init.Everything)
    use mainWindow = SDLWindow.create "Space Explorer" 100<px> 100<px> screenWidth screenHeight 0u //(uint32 SDLWindow.Flags.FullScreen)
    use mainRenderer = SDLRender.create mainWindow -1 SDLRender.Flags.Accelerated
    use surface = SDLSurface.createRGB (screenWidth, screenHeight, 32<bit/px>) (0x00FF0000u ,0x0000FF00u, 0x000000FFu, 0x00000000u)
    
    use mainTexture = mainRenderer |> SDLTexture.create SDLPixel.RGB888Format SDLTexture.Access.Streaming (screenWidth,screenHeight)
    let context =  { renderer = mainRenderer; texture = mainTexture; surface = surface; lastFrameTick = getTicks() }
    let state = GameState.empty

    eventPump (render context) handleEvent update state
    0
