module WebGLView where

import String

import GameModel
import Grid

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

-- Higher level API

tile : (Int, Int) -> GameModel.Tile -> Entity
tile (x, y) t =
    let color = case t of
                    GameModel.Wall  -> vec3 0.5 0.5 0.5
                    GameModel.Floor -> vec3 0 0 0
    in  entity vertexShader fragmentShader (square (toFloat x, toFloat y) color) { scale = scale 0.04, camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)}

background : Grid.Grid GameModel.Tile -> Element
background level =
    let grid = Grid.toList level
        (w, h) = (level.size.width, level.size.height)
        (w' , h')= (w // 2, h // 2)

        row : Int -> [GameModel.Tile] -> [Entity]
        row y ts = map (\(t, x) -> tile (x, y) t) <| zip ts [-w'..w' + 1]

        tiles = concatMap (\(r, y) -> row y r) <| zip grid [-h'..h' + 1]
    in  webgl (600, 600) tiles

-- Create a mesh with two triangles

type Vertex = { position:Vec3, color:Vec3 }

square : (Float, Float) -> Vec3 -> [Triangle Vertex]
square (x, y) color =
    let x' = 2 * x
        y' = 2 * y in
    [ ( Vertex (vec3 (x' - 1) (y' - 1) 0) color
      , Vertex (vec3 (x' + 1) (y' + 1) 0) color
      , Vertex (vec3 (x' - 1) (y' + 1) 0) color
      )
    , ( Vertex (vec3 (x' - 1) (y' - 1) 0) color
      , Vertex (vec3 (x' + 1) (y' + 1) 0) color
      , Vertex (vec3 (x' + 1) (y' - 1) 0) color
      )
    ]

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

scale : Float -> Mat4
scale n = makeScale (vec3 n n n)

main : Element
main = scene

scene : Element
scene = background initialLevel

-- Shaders

vertexShader : Shader { attr | position:Vec3, color:Vec3 } {unif | scale:Mat4, camera:Mat4} { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 scale;
uniform mat4 camera;
varying vec3 vcolor;

void main () {
    gl_Position = scale * camera * vec4(position, 1.0);
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
