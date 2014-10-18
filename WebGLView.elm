module WebGLView where

import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

-- Higher level API

tile : (Int, Int) -> Entity
tile (x, y) = entity vertexShader fragmentShader (square (toFloat x, toFloat y) (vec3 1 0 0)) { scale = scale 0.05}


-- Create a mesh with two triangles

type Vertex = { position:Vec3, color:Vec3 }

square : (Float, Float) -> Vec3 -> [Triangle Vertex]
square (x, y) color =
    let x' = 2 * x
        y' = 2 * y in
    [ ( Vertex (vec3 (x' - 1) (y' - 1) 0) color
      , Vertex (vec3 (x' + 1) (y' + 1) 0) (vec3 0 1 0)
      , Vertex (vec3 (x' - 1) (y' + 1) 0) color
      )
    , ( Vertex (vec3 (x' - 1) (y' - 1) 0) color
      , Vertex (vec3 (x' + 1) (y' + 1) 0) (vec3 0 1 0)
      , Vertex (vec3 (x' + 1) (y' - 1) 0) color
      )
    ]

-- Create the scene

scale : Float -> Mat4
scale n = makeScale (vec3 n n n)

main : Element
main = scene

scene : Element
scene = color blue <|
    webgl (800,800)
    [ tile (0, 0), tile (0, 1)
    ]

-- Shaders

vertexShader : Shader { attr | position:Vec3, color:Vec3 } {unif | scale:Mat4} { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 scale;
varying vec3 vcolor;

void main () {
    gl_Position = scale * vec4(position, 1.0);
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
