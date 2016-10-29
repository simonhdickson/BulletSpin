[<AutoOpen>]
module SpaceExplorer.Common

open System

open SDLUtility
open SDLPixel
open SDLRender
open SDLKeyboard
open SDLGameController
open SDLGeometry

let screenWidth = 1024<px>
let screenHeight = 768<px>

let cellWidth = 16
let cellHeight = 16
let cellWidthf = 16.0
let cellHeightf = 16.0

let mapWidth = 64
let mapHeight = 48
let mapWidthf = 64.0
let mapHeightf = 48.0

type Vector2 = {
    X: double 
    Y: double }
    with
    static member (+) (pointa , pointb) = 
        {X = pointa.X + pointb.X ; Y= pointa.Y + pointb.Y}
    static member (*) (pointa , amount) = 
        {X = pointa.X * amount ; Y= pointa.Y * amount }
    member this.GridX = int(this.X / cellWidthf)
    member this.GridY = int(this.Y / cellHeightf)
    member this.CentreGridX = int <| (this.X + cellWidthf / 2.0) / cellWidthf
    member this.CentreGridY = int <| (this.Y + cellHeightf /2.0) / cellHeightf
    member this.Grid = this.GridX, this.GridY
    member this.Rotate(degrees) =
        this.RotateRadians(degrees * (Math.PI / 180.))
    member this.RotateRadians(radians) =
        let ca = Math.Cos radians
        let sa = Math.Sin radians
        { X = ca * this.X - sa * this.Y; Y = sa * this.X + ca * this.Y }

type Horizontal =
    | Left
    | Right

type Vertical =
    | Up
    | Down

type FireDirection = FireHorizontal of Horizontal | FireVertical of Vertical

type Player = {
    location : Vector2
    moveDirection : Horizontal option * Vertical option
    fireDirection : FireDirection option
    nextFireTime : DateTime }
    with
    member this.Size =
        cellWidth * 1<px>,cellHeight * 1<px>

    member this.AsRect =
        let w, h = this.Size
        { 
            X = (this.location.X |> int)*1<px> 
            Y = (this.location.Y |> int)*1<px>
            Width = w
            Height = h 
        }

type ProjectileOwner =
     | Player
     | Enemy

type Motion =
    | Velocity of Vector2
    | Bezier of Vector2[] 

type Projectile = {
    location : Vector2
    motion : Motion
    owner : ProjectileOwner
    texture : string }
    with
    member this.Size =
        cellWidth * 1<px>,cellHeight * 1<px>

    member this.AsRect =
        let w, h = this.Size
        { 
            X = (this.location.X |> int)*1<px> 
            Y = (this.location.Y |> int)*1<px>
            Width = w
            Height = h 
        }

type Enemy = { 
    location : Vector2
    fireDirection : Vector2
    nextFireTime : DateTime }
    with
    member this.Size =
        cellWidth * 2<px>,cellHeight * 2<px>

    member this.AsRect =
        let w, h = this.Size
        { 
            X = (this.location.X |> int)*1<px> 
            Y = (this.location.Y |> int)*1<px>
            Width = w
            Height = h 
        }

type Level = { player:Player; enemy:Enemy; projectiles:Projectile Set }

type Screen =
    | Title
    | Level of level:Level

type GameState = {
    treatsLookup : Set<int*int> 
    pressedKeys : Set<ScanCode> 
    controllers : Set<ControllerButton> * Set<ControllerButton>
    sprites : Map<string, SDLTexture.Texture>
    turkeyAngle : float
    screen: Screen
    chaos : System.Random      
    lastFrameTime: uint32 }
      
type RenderingContext = {
    renderer:SDLRender.Renderer
    texture:SDLTexture.Texture
    surface:SDLSurface.Surface
    mutable lastFrameTick : uint32 }

let defaultLevel () =
    let player = { 
        location = { X = 0.; Y = 0.}
        moveDirection = None, None
        fireDirection = None
        nextFireTime = DateTime.MinValue }
    Level {
        player = player
        projectiles = Set.empty
        enemy = { location = { X = float screenWidth / 2.; Y = float screenHeight / 2. }; fireDirection = { X = 10.; Y = 0. }; nextFireTime = DateTime.MinValue }
    }

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module GameState =
    let updatePlayer f gameState =
        match gameState.screen with
        | Level ({ player = player } as level) ->
            { gameState with screen = Level { level with player = f player} }
        | _ -> gameState

    let updateEnemy f gameState =
        match gameState.screen with
        | Level ({ enemy = enemy } as level) ->
            { gameState with screen = Level { level with enemy = f enemy} }
        | _ -> gameState

    let updateProjectiles f gameState =
        match gameState.screen with
        | Level ({ projectiles = projectiles } as level) ->
            { gameState with screen = Level { level with projectiles = f projectiles} }
        | _ -> gameState

    let empty = {
        treatsLookup = Set.empty
        pressedKeys = Set.empty
        sprites = Map.empty
        controllers = Set.empty, Set.empty
        turkeyAngle = 0.0
        screen = defaultLevel ()
        chaos = System.Random(System.DateTime.Now.Millisecond)
        lastFrameTime = getTicks() }
