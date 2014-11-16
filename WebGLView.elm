module WebGLView where

import String
import Text
import Http (..)

import GameModel
import GameUpdate
import GameView
import MapGen
import Grid

import Math.Vector2 (Vec2, vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

import Generator
import Generator.Standard

type Vertex = { position:Vec3, offset:Vec3, color:Vec3, coord:Vec3 }
type Point = (Float, Float)

even : Int -> Bool
even n = n % 2 == 0

responseToMaybe : Response a -> Maybe a
responseToMaybe response =
    case response of
        Success a -> Just a
        _         -> Nothing

-- Higher level API

texture : Signal (Maybe Texture)
texture = responseToMaybe <~ loadTexture "/sprite_sheet1.png"

xScale : Float
xScale = 15

yScale : Float
yScale = 20

tile : (Int, Int) -> Mat4 -> Texture -> GameModel.Tile -> Entity
tile (x, y) perspective texture t =
    case t of
        GameModel.Floor -> floorTile texture perspective <| vec3 (toFloat x) (toFloat y) 0.0
        GameModel.Wall  -> wallTile texture perspective <| vec3 (toFloat x) (toFloat y) 0.0

texturedTile : Int -> Int -> Texture -> Mat4 -> Vec3 -> Entity
texturedTile x y texture perspective offset =
    let (x', y') = (toFloat x, toFloat y)
        black' = fromRGB black
        triangles = quad (-1, 1) (1, 1) (-1, -1) (1, -1) offset black'
    in  entity vertexShaderTex fragmentShaderTex triangles {perspective = perspective, texture = texture, sprite = vec3 x' y' 0}

wallTile : Texture -> Mat4 -> Vec3 -> Entity
wallTile = texturedTile 3 2

floorTile : Texture -> Mat4 -> Vec3 -> Entity
floorTile = texturedTile 14 2

fogTile : Texture -> Mat4 -> Vec3 -> Entity
fogTile texture perspective offset =
    let black' = fromRGB black
        triangles = quad (-1, 1) (1, 1) (-1, -1) (1, -1) offset black'
    in  entity vertexShader fragmentShader triangles {perspective = perspective}

background : Grid.Grid GameModel.Tile -> Maybe Texture -> ((Int, Int), [Entity])
background level texture =
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

        row : Texture -> Int -> [GameModel.Tile] -> [Entity]
        row texture y ts = map (\(t, x) -> tile (x, y) perspective texture t) <| zip ts [-w'..w' + 1]

        tiles : Maybe Texture -> [Entity]
        tiles texture = case texture of
            Just tex -> concatMap (\(r, y) -> row tex y r) <| zip grid [-h'..h' + 1]
            Nothing  -> []

        w'' = (toFloat w) * xScale |> round
        h'' = (toFloat h) * yScale |> round

    in  ((w'', h''), (tiles texture))

display : Signal GameModel.State -> Signal Element
display state = display' <~ state ~ texture

display' : GameModel.State -> Maybe Texture -> Element
display' state texture =
    let (dimensions, bg) = background state.level texture
    in  color black <| webgl dimensions bg

-- Demo

dimensions : (Int, Int)
dimensions = (30, 20)

gen : GameModel.Random
gen = Generator.Standard.generator 10023

setExplored : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Visibility
setExplored level =
    let grid = Grid.toList level
    in map (\row -> map (\_ -> GameModel.Unexplored) row) grid |> Grid.fromList

initialPlayer : GameModel.Random -> (GameModel.Player, GameModel.Random)
initialPlayer gen =
    let elem = "@"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
    in  GameModel.player elem "You" gen

initialEnemy : GameModel.Random -> (GameModel.Enemy, GameModel.Random)
initialEnemy gen =
    let elem = "e"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
    in GameModel.enemy elem "enemy" gen

initialState : GameModel.State
initialState =
    let (player, gen') = initialPlayer gen
        (enemy, gen'') = initialEnemy gen'
        (firstMap, gen''') = MapGen.randomCave dimensions gen''
        firstExplored = setExplored firstMap
    in  GameModel.State
                    player
                    [enemy]
                    firstMap
                    firstExplored
                    ["you enter the dungeon"]
                    gen'''
                        |> GameUpdate.placeEntities |> GameUpdate.reveal

main = display (constant initialState)

-- Shaders

vertexShader : Shader { attr | position:Vec3, offset:Vec3, color:Vec3 } {unif | perspective:Mat4} { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 offset;
attribute vec3 color;
uniform mat4 perspective;
varying vec3 vcolor;

void main () {
    vec3 stuff = (2.0 * offset) + position;
    gl_Position = perspective * vec4(stuff, 1.0);
    vcolor = color;
}

|]

fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]

vertexShaderTex : Shader { attr | position:Vec3, offset:Vec3, color:Vec3, coord:Vec3 } {unif | perspective:Mat4} { vcolor:Vec3, vcoord:Vec2 }
vertexShaderTex = [glsl|

attribute vec3 position;
attribute vec3 offset;
attribute vec3 color;
attribute vec3 coord;
uniform mat4 perspective;
varying vec3 vcolor;
varying vec2 vcoord;

void main () {
    vec3 stuff = (2.0 * offset) + position;
    gl_Position = perspective * vec4(stuff, 1.0);
    vcolor = color;
    vcoord = coord.xy;
}

|]

fragmentShaderTex : Shader {} {unif | texture:Texture, sprite:Vec3} { vcolor:Vec3, vcoord:Vec2 }
fragmentShaderTex = [glsl|

precision mediump float;
uniform sampler2D texture;
uniform vec3 sprite;
varying vec3 vcolor;
varying vec2 vcoord;

void main () {
    vec2 spritecoord = vcoord + sprite.xy;
    vec2 coord = vec2(spritecoord.x, 16.0 - spritecoord.y) / 16.0;
    gl_FragColor = texture2D(texture, coord);
}

|]

-- Shape constructors

quad : Point -> Point -> Point -> Point -> Vec3 -> Vec3 -> [Triangle Vertex]
quad (x1, y1) (x2, y2) (x3, y3) (x4, y4) offset color =
    let topLeft     = Vertex (vec3 x1 y1 0) offset color (vec3 0 0 0)
        topRight    = Vertex (vec3 x2 y2 0) offset color (vec3 1 0 0)
        bottomLeft  = Vertex (vec3 x3 y3 0) offset color (vec3 0 1 0)
        bottomRight = Vertex (vec3 x4 y4 0) offset color (vec3 1 1 0)
    in  [ ( topLeft, topRight, bottomLeft)
        , ( bottomLeft, topRight, bottomRight)
        ]

fromRGB : Color -> Vec3
fromRGB color =
    let {red, green, blue, alpha} = toRgb color
        div x = toFloat x / 255
    in  vec3 (div red) (div green) (div blue)
