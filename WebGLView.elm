module WebGLView where

import String

import GameModel
import GameView
import Grid

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

type Vertex = { position:Vec3, offset:Vec3, color:Vec3 }
type Point = (Float, Float)

even : Int -> Bool
even n = n % 2 == 0

-- Higher level API

xScale : Float
xScale = 15

yScale : Float
yScale = 20

tile : (Int, Int) -> Mat4 -> GameModel.Tile -> Entity
tile (x, y) perspective t =
    case t of
        GameModel.Floor -> floorTile perspective <| vec3 (toFloat x) (toFloat y) 0.0
        GameModel.Wall  -> wallTile perspective <| vec3 (toFloat x) (toFloat y) 0.0

wallTile : Mat4 -> Vec3 -> Entity
wallTile perspective offset =
    let black' = fromRGB black
        grey' = fromRGB grey
        triangles = quad (-0.15, 0.5) (-0.05, 0.5) (-0.3, -0.5) (-0.2, -0.5) offset black'
                 ++ quad (0.2, 0.5) (0.3, 0.5) (0.05, -0.5) (0.15, -0.5) offset black'
                 ++ quad (-0.5, 0.2) (0.5, 0.2) (-0.5, 0.1) (0.5, 0.1) offset black'
                 ++ quad (-0.5, -0.1) (0.5, -0.1) (-0.5, -0.2) (0.5, -0.2) offset black'
                 ++ quad (-1, 1) (1, 1) (-1, -1) (1, -1) offset grey'
    in  entity vertexShader fragmentShader triangles {perspective = perspective}

floorTile : Mat4 -> Vec3 -> Entity
floorTile perspective offset =
    let black' = fromRGB black
        white' = fromRGB white
        triangles = quad (-0.125, 0.125) (0.125, 0.125) (-0.125, -0.125) (0.125, -0.125) offset white'
                 ++ quad (-1, 1) (1, 1) (-1, -1) (1, -1) offset black'
    in  entity vertexShader fragmentShader triangles {perspective = perspective}

background : Grid.Grid GameModel.Tile -> Element
background level =
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

        row : Int -> [GameModel.Tile] -> [Entity]
        row y ts = map (\(t, x) -> tile (x, y) perspective t) <| zip ts [-w'..w' + 1]

        tiles = concatMap (\(r, y) -> row y r) <| zip grid [-h'..h' + 1]
        w'' = (toFloat w) * xScale |> round
        h'' = (toFloat h) * yScale |> round
    in  color black <| webgl (w'', h'') tiles

-- Create the scene

initialLevel : Grid.Grid GameModel.Tile
initialLevel =
    let toTile c = case c of
                        ' ' -> GameModel.Floor
                        '#' -> GameModel.Wall
                        '+' -> GameModel.Door
                        '~' -> GameModel.Acid
        s = [ "###################"
            , "#        #        #"
            , "#        #        #"
            , "#                 #"
            , "#        #        #"
            , "#        #        #"
            , "###################"
            ]
    in  Grid.fromList <| map (\x -> map toTile <| String.toList x) s

main : Element
main = scene

scene : Element
scene = flow down [background initialLevel, spacer 10 10, GameView.background initialLevel]

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

-- Shape constructors

quad : Point -> Point -> Point -> Point -> Vec3 -> Vec3 -> [Triangle Vertex]
quad (x1, y1) (x2, y2) (x3, y3) (x4, y4) offset color =
    [ ( Vertex (vec3 x1 y1 0) offset color
      , Vertex (vec3 x2 y2 0) offset color
      , Vertex (vec3 x3 y3 0) offset color
      )
    , ( Vertex (vec3 x3 y3 0) offset color
      , Vertex (vec3 x2 y2 0) offset color
      , Vertex (vec3 x4 y4 0) offset color
      )
    ]

fromRGB : Color -> Vec3
fromRGB color =
    let {red, green, blue, alpha} = toRgb color
        div x = toFloat x / 255
    in  vec3 (div red) (div green) (div blue)
