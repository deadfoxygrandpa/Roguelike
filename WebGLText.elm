module WebGLText where

import Http (..)
import Maybe (isJust)
import String

import Math.Vector2 (Vec2, vec2)
import Math.Vector3 (..)
import Math.Vector4 (Vec4, vec4)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

type Vertex = { position:Vec2, coord:Vec2 }
type Point = (Float, Float)
type Character = Texture -> Color -> Float -> (Int, Int) -> Mat4 -> Entity

texture : Signal (Maybe Texture)
texture = responseToMaybe <~ loadTexture "/sprite_sheet1.png"

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

quad : Point -> Point -> Point -> Point -> [Triangle Vertex]
quad (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
    let topLeft     = Vertex (vec2 x1 y1) (vec2 0 0)
        topRight    = Vertex (vec2 x2 y2) (vec2 1 0)
        bottomLeft  = Vertex (vec2 x3 y3) (vec2 0 1)
        bottomRight = Vertex (vec2 x4 y4) (vec2 1 1)
    in  [ ( topLeft, topRight, bottomLeft)
        , ( bottomLeft, topRight, bottomRight)
        ]

fromRGB : Color -> Vec3
fromRGB color =
    let {red, green, blue, alpha} = toRgb color
        div x = toFloat x / 255
    in  vec3 (div red) (div green) (div blue)

scene maybeTexture = case maybeTexture of
    Just texture -> color black <| webgl (500, 500) <| write "hi jamie!" (-5, 0) purple 0.5 texture (makeOrtho2D -20 20 -20 20)
    Nothing -> empty

main = scene <~ texture

-- Layout?

write : String -> (Int, Int) -> Color -> Float -> Texture -> Mat4 -> [Entity]
write text (x, y) color spacing texture perspective =
    let chars = String.toList text
        len = String.length text
        letter c x' = (pickChar c) texture color spacing (x', y) perspective
    in  map (\(c, a) -> letter c a) <| zip chars [x..x + len + 1]

pickChar : Char -> Character
pickChar c =
    case c of
        ' ' -> space
        '!' -> exclamationPoint
        '"' -> doubleQuote
        'a' -> lowercaseA
        'b' -> lowercaseB
        'c' -> lowercaseC
        'd' -> lowercaseD
        'e' -> lowercaseE
        'f' -> lowercaseF
        'g' -> lowercaseG
        'h' -> lowercaseH
        'i' -> lowercaseI
        'j' -> lowercaseJ
        'k' -> lowercaseK
        'l' -> lowercaseL
        'm' -> lowercaseM
        'n' -> lowercaseN
        'o' -> lowercaseO
        'p' -> lowercaseP
        'q' -> lowercaseQ
        'r' -> lowercaseR
        's' -> lowercaseS
        't' -> lowercaseT
        'u' -> lowercaseU
        'v' -> lowercaseV
        'w' -> lowercaseW
        'x' -> lowercaseX
        'y' -> lowercaseY
        'z' -> lowercaseZ
        _   -> unknown

-- Character base tiles

characterTile : [Triangle Vertex]
characterTile = quad (-1, 1) (1, 1) (-1, -1) (1, -1)

makeLetter : Int -> Int -> Character
makeLetter spritex spritey texture color spacing (x, y) perspective =
    entity vertexShader fragmentShader characterTile { perspective = perspective
                                                     , offset = vec3 (toFloat x) (toFloat y) 0
                                                     , texture = texture
                                                     , sprite = vec3 (toFloat spritex) (toFloat spritey) 0
                                                     , color = fromRGB color
                                                     , spacing = spacing
                                                     }

unknown : Character
unknown = makeLetter 14 0

space : Character
space = makeLetter 0 0

exclamationPoint : Character
exclamationPoint = makeLetter 1 2

doubleQuote : Character
doubleQuote = makeLetter 2 2

lowercaseA : Character
lowercaseA = makeLetter 1 6

lowercaseB : Character
lowercaseB = makeLetter 2 6

lowercaseC : Character
lowercaseC = makeLetter 3 6

lowercaseD : Character
lowercaseD = makeLetter 4 6

lowercaseE : Character
lowercaseE = makeLetter 5 6

lowercaseF : Character
lowercaseF = makeLetter 6 6

lowercaseG : Character
lowercaseG = makeLetter 7 6

lowercaseH : Character
lowercaseH = makeLetter 8 6

lowercaseI : Character
lowercaseI = makeLetter 9 6

lowercaseJ : Character
lowercaseJ = makeLetter 10 6

lowercaseK : Character
lowercaseK = makeLetter 11 6

lowercaseL : Character
lowercaseL = makeLetter 12 6

lowercaseM : Character
lowercaseM = makeLetter 13 6

lowercaseN : Character
lowercaseN = makeLetter 14 6

lowercaseO : Character
lowercaseO = makeLetter 15 6

lowercaseP : Character
lowercaseP = makeLetter 0 7

lowercaseQ : Character
lowercaseQ = makeLetter 1 7

lowercaseR : Character
lowercaseR = makeLetter 2 7

lowercaseS : Character
lowercaseS = makeLetter 3 7

lowercaseT : Character
lowercaseT = makeLetter 4 7

lowercaseU : Character
lowercaseU = makeLetter 5 7

lowercaseV : Character
lowercaseV = makeLetter 6 7

lowercaseW : Character
lowercaseW = makeLetter 7 7

lowercaseX : Character
lowercaseX = makeLetter 8 7

lowercaseY : Character
lowercaseY = makeLetter 9 7

lowercaseZ : Character
lowercaseZ = makeLetter 10 7


-- Shaders

vertexShader : Shader { attr | position:Vec2, coord:Vec2 } { unif | perspective:Mat4, offset:Vec3, spacing:Float } { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec2 position;
attribute vec2 coord;
uniform mat4 perspective;
uniform vec3 offset;
uniform float spacing;
varying vec2 vcoord;

void main () {
    vec2 stuff = (spacing * 2.0 * offset.xy) + position;
    gl_Position = perspective * vec4(stuff, 0.0, 1.0);
    vcoord = coord;
}

|]

fragmentShader : Shader {} { unif | texture:Texture, sprite:Vec3, color:Vec3 } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D texture;
uniform vec3 color;
uniform vec3 sprite;
varying vec2 vcoord;

void main () {
    vec2 spritecoord = vcoord + sprite.xy;
    vec2 coord = vec2(spritecoord.x, 16.0 - spritecoord.y) / 16.0;
    vec4 c = texture2D(texture, coord);
    if (c.a < 0.1) {
        discard;
    } else {
        gl_FragColor = vec4(color, 1.0) * c;
    }
}

|]