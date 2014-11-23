module WebGLView where

import String
import Text
import Http (..)
import Maybe (isJust)

import GameModel
import GameUpdate
import GameView
import MapGen
import Grid

import Math.Vector2 (Vec2, vec2)
import Math.Vector3 (..)
import Math.Vector4 (Vec4, vec4)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Generator
import Generator.Standard

type Vertex = { position:Vec2, coord:Vec2 }
type Point = (Float, Float)

even : Int -> Bool
even n = n % 2 == 0

responseToMaybe : Response a -> Maybe a
responseToMaybe response =
    case response of
        Success a -> Just a
        _         -> Nothing

justs : [Maybe a] -> [a]
justs ms =
    let js = filter isJust ms
        just m = case m of
                    Just x -> x
    in  map just js

-- Higher level API

texture : Signal (Maybe Texture)
texture = responseToMaybe <~ loadTexture "/sprite_sheet1.png"

xScale : Float
xScale = 16

yScale : Float
yScale = 16

tile : (Int, Int) -> Mat4 -> Texture -> GameModel.Tile -> Entity
tile (x, y) perspective texture t =
    case t of
        GameModel.Floor -> floorTile texture perspective <| vec2 (toFloat x) (toFloat y)
        GameModel.Wall  -> wallTile texture perspective <| vec2 (toFloat x) (toFloat y)

fogTiles : (Int, Int) -> Mat4 -> GameModel.Visibility -> Maybe Entity
fogTiles (x, y) perspective t =
    case t of
        GameModel.Unexplored -> Just <| fogTile perspective <| vec2 (toFloat x) (toFloat y)
        GameModel.Explored   -> Nothing --Just <| exploredTile perspective <| vec2 (toFloat x) (toFloat y)
        GameModel.Visible    -> Nothing

baseTile : [Triangle Vertex]
baseTile = quad (-1, 1) (1, 1) (-1, -1) (1, -1)

texturedTile : Int -> Int -> Texture -> Mat4 -> Vec2 -> Entity
texturedTile x y texture perspective offset =
    let (x', y') = (toFloat x, toFloat y)
    in  entity vertexShaderTex fragmentShaderTex baseTile {perspective = perspective, offset = offset, color = fromRGB black, texture = texture, sprite = vec3 x' y' 0}

coloredTile : Color -> Mat4 -> Vec2 -> Entity
coloredTile color perspective offset =
    let color' = fromRGB color
    in  entity vertexShader fragmentShader baseTile {perspective = perspective, offset = offset, color = color'}

wallTile : Texture -> Mat4 -> Vec2 -> Entity
wallTile = texturedTile 3 2

floorTile : Texture -> Mat4 -> Vec2 -> Entity
floorTile = texturedTile 14 2

fogTile : Mat4 -> Vec2 -> Entity
fogTile = coloredTile black

exploredTile : Mat4 -> Vec2 -> Entity
exploredTile = coloredTile (rgba 0 0 0 0.7)

playerTile : Texture -> Mat4 -> Vec2 -> Entity
playerTile = texturedTile 2 0

enemyTile : Texture -> Mat4 -> Vec2 -> Entity
enemyTile = texturedTile 5 6

fogger : Grid.Grid GameModel.Visibility -> [Entity]
fogger level =
    let grid = Grid.toList level
        (w, h) = (level.size.width, level.size.height)
        (w' , h')= (w // 2, h // 2)
        (left, right) = case even w of
                            True  -> (toFloat (-w - 1), toFloat w - 1)
                            False -> (toFloat (-w), toFloat w)
        (top, bottom) = case even h of
                            True  -> (toFloat (-h - 1), toFloat h - 1)
                            False -> (toFloat (-h), toFloat h)
        perspective = makeOrtho2D left right top bottom

        row : Int -> [GameModel.Visibility] -> [Entity]
        row y ts = justs <| map (\(t, x) -> fogTiles (x, y) perspective t) <| zip ts [-w'..w' + 1]

        tiles : [Entity]
        tiles = concatMap (\(r, y) -> row y r) <| zip grid (reverse [-h' - 1..h'])
    in  tiles

background : Grid.Grid GameModel.Tile -> Maybe Texture -> (Int, Int) -> Mat4 -> [Entity]
background level texture (w, h) perspective =
    let grid = Grid.toList level

        row : Texture -> Int -> [GameModel.Tile] -> [Entity]
        row texture y ts = map (\(t, x) -> tile (x, y) perspective texture t) <| zip ts [-w..w + 1]

        tiles : Maybe Texture -> [Entity]
        tiles texture = case texture of
            Just tex -> concatMap (\(r, y) -> row tex y r) <| zip grid (reverse [-h - 1..h])
            Nothing  -> []
    in  tiles texture

backgroundElement : Grid.Grid GameModel.Tile -> Maybe Texture -> Element
backgroundElement level texture =
    let grid = Grid.toList level
        (w, h) = (level.size.width, level.size.height)
        (w' , h')= (w // 2, h // 2)
        (left, right) = case even w of
                            True  -> (toFloat (-w - 1), toFloat w - 1)
                            False -> (toFloat (-w), toFloat w)
        (top, bottom) = case even h of
                            True  -> (toFloat (-h - 1), toFloat h - 1)
                            False -> (toFloat (-h), toFloat h)
        perspective = makeOrtho2D left right top bottom
        w'' = (toFloat w) * xScale |> round
        h'' = (toFloat h) * yScale |> round
        dimensions = (w'', h'')

        row : Texture -> Int -> [GameModel.Tile] -> [Entity]
        row texture y ts = map (\(t, x) -> tile (x, y) perspective texture t) <| zip ts [-w'..w' + 1]

        tiles : Maybe Texture -> [Entity]
        tiles texture = case texture of
            Just tex -> concatMap (\(r, y) -> row tex y r) <| zip grid (reverse [-h' - 1..h'])
            Nothing  -> []
    in  color black <| webgl dimensions (tiles texture)

drawPlayer : GameModel.Player -> Maybe Texture -> (Int, Int) -> Mat4 -> [Entity]
drawPlayer player texture (w, h) perspective =
    let {x, y} = player.location
        x' = x - w
        y' = y - h
    in case texture of
            Just tex -> [ playerTile tex perspective <| vec2 (toFloat x') (toFloat -y')
                        , coloredTile black perspective <| vec2 (toFloat x') (toFloat -y')
                        ]
            Nothing  -> []

drawEnemy : GameModel.Enemy -> Maybe Texture -> (Int, Int) -> Mat4 -> [Entity]
drawEnemy enemy texture (w, h) perspective =
    let {x, y} = enemy.location
        x' = x - w
        y' = y - h
    in case texture of
            Just tex -> [ enemyTile tex perspective <| vec2 (toFloat x') (toFloat -y')
                        , coloredTile black perspective <| vec2 (toFloat x') (toFloat -y')
                        ]
            Nothing  -> []

display : Signal GameModel.State -> Signal Element
display state =
    let guys = display' <~ state ~ texture
        level = .level <~ state |> dropRepeats
        bg = backgroundElement <~ level ~ texture
    in  (\a b -> layers [a, b]) <~ bg ~ guys

display' : GameModel.State -> Maybe Texture -> Element
display' state texture =
    let (w, h) = (state.level.size.width, state.level.size.height)
        (w' , h')= (w // 2, h // 2)
        (left, right) = case even w of
                            True  -> (toFloat (-w - 1), toFloat w - 1)
                            False -> (toFloat (-w), toFloat w)
        (top, bottom) = case even h of
                            True  -> (toFloat (-h - 1), toFloat h - 1)
                            False -> (toFloat (-h), toFloat h)
        perspective = makeOrtho2D left right top bottom
        w'' = (toFloat w) * xScale |> round
        h'' = (toFloat h) * yScale |> round
        dimensions = (w'', h'')

        player = drawPlayer state.player texture (w', h') perspective
        enemies = concatMap (\enemy -> drawEnemy enemy texture (w', h') perspective) state.enemies
        --fog = fogger state.explored
    in  flow down [webgl dimensions (player ++ enemies), asText state.player]

-- Shaders

vertexShader : Shader { attr | position:Vec2 } {unif | perspective:Mat4, offset:Vec2} {}
vertexShader = [glsl|

attribute vec2 position;
uniform mat4 perspective;
uniform vec2 offset;

void main () {
    vec2 stuff = (2.0 * offset) + position;
    gl_Position = perspective * vec4(stuff, 0.0, 1.0);
}

|]

fragmentShader : Shader {} {unif | color:Vec4 } {}
fragmentShader = [glsl|

precision mediump float;
uniform vec4 color;

void main () {
    gl_FragColor = color;
}

|]

vertexShaderTex : Shader { attr | position:Vec2, coord:Vec2 } {unif | perspective:Mat4, offset:Vec2} { vcoord:Vec2 }
vertexShaderTex = [glsl|

attribute vec2 position;
attribute vec2 coord;
uniform mat4 perspective;
uniform vec2 offset;
varying vec2 vcoord;

void main () {
    vec2 stuff = (2.0 * offset) + position;
    gl_Position = perspective * vec4(stuff, 0.0, 1.0);
    vcoord = coord;
}

|]

fragmentShaderTex : Shader {} {unif | color:Vec4, texture:Texture, sprite:Vec3} { vcoord:Vec2 }
fragmentShaderTex = [glsl|

precision mediump float;
uniform vec4 color;
uniform sampler2D texture;
uniform vec3 sprite;
varying vec2 vcoord;

void main () {
    vec2 spritecoord = vcoord + sprite.xy;
    vec2 coord = vec2(spritecoord.x, 16.0 - spritecoord.y) / 16.0;
    vec4 tcolor = texture2D(texture, coord);
    if (tcolor.a < 0.1) {
        discard;
    } else {
        gl_FragColor = tcolor;
    }
}

|]

-- Shape constructors

quad : Point -> Point -> Point -> Point -> [Triangle Vertex]
quad (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
    let topLeft     = Vertex (vec2 x1 y1) (vec2 0 0)
        topRight    = Vertex (vec2 x2 y2) (vec2 1 0)
        bottomLeft  = Vertex (vec2 x3 y3) (vec2 0 1)
        bottomRight = Vertex (vec2 x4 y4) (vec2 1 1)
    in  [ ( topLeft, topRight, bottomLeft)
        , ( bottomLeft, topRight, bottomRight)
        ]

fromRGB : Color -> Vec4
fromRGB color =
    let {red, green, blue, alpha} = toRgb color
        div x = toFloat x / 255
    in  vec4 (div red) (div green) (div blue) alpha