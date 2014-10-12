module Benchmarks where

import String
import Text
import Either

import GameModel
import GameUpdate
import MapGen
import Grid

import Generator
import Generator.Standard
import Benchmark

gen = Generator.Standard.generator 25060

listOf2 f gen n = Generator.listOf f n gen

initialLevel : Grid.Grid GameModel.Tile
initialLevel =
    let toTile c = case c of
                        ' ' -> GameModel.Floor
                        '#' -> GameModel.Wall
                        '+' -> GameModel.Door
                        '~' -> GameModel.Acid
        s = [ "####################"
            , "#        #         #"
            , "#        #         #"
            , "#                  #"
            , "#        #         #"
            , "#        #         #"
            , "####################"
            ]
    in  Grid.fromList <| map (\x -> map toTile <| String.toList x) s

initialExplored : Grid.Grid GameModel.Visibility
initialExplored =
    let grid = Grid.toList initialLevel
    in  map (\row -> map (\_ -> GameModel.Unexplored) row) grid |> Grid.fromList

initialPlayer : GameModel.Random -> (GameModel.Player, GameModel.Random)
initialPlayer gen =
    let elem = "@"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
    in  GameModel.player (elem, "You", gen)

initialEnemy : GameModel.Random -> (GameModel.Enemy, GameModel.Random)
initialEnemy gen =
    let elem = "e"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
    in GameModel.enemy (elem, "enemy", gen)

state : GameModel.State
state =
    let (player, gen') = initialPlayer gen
        (enemy, gen'') = initialEnemy gen'
    in  GameModel.State
                    player
                    [enemy]
                    initialLevel
                    initialExplored
                    ["you enter the dungeon"]
                    gen''
                        |> GameUpdate.reveal

newState state =
    let player = state.player
        player' = {player| health <- player.health - 1}
    in  {state| player <- player'}

newPlayer player = {player| health <- player.health - 1}

stateTest : Int ->  Either.Either GameModel.State GameModel.Player -> Either.Either GameModel.State GameModel.Player
stateTest n x =
    case x of
        Either.Left state -> Either.Left <| head <| map (\_ -> newState state) [1..n]
        Either.Right player -> Either.Right <| head <| map (\_ -> newPlayer player) [1..n]

maps : [Grid.Grid GameModel.Tile]
maps =
    let gen = Generator.Standard.generator 1492
        mkMap dimensions = MapGen.randomMap dimensions gen |> fst |> MapGen.iterate2
    in  map mkMap [ (20, 10)
                  , (20, 20)
                  , (20, 30)
                  , (30, 30)
                  ]

mapElements : [Element]
mapElements = map render2 maps

main = Benchmark.run [ Benchmark.logic "new state" (uncurry stateTest) [(500, Either.Left state), (500, Either.Right state.player)]
                     , Benchmark.render "alternate render maps" render2 maps
                     , Benchmark.logic "alternate render maps - logic" render2 maps
                     , Benchmark.render "render maps" render maps
                     , Benchmark.logic "render maps - logic" render maps
                     , Benchmark.render "draw prerendered maps" identity mapElements
                     ]

-- rendering information pulled from GameView for modification

render : Grid.Grid GameModel.Tile -> Element
render level =
    let grid = Grid.toList level
        (w, h) = (level.size.width * xScale, level.size.height * yScale)

        xOffset : Int -> Float
        xOffset n      = ((toFloat n) - (toFloat level.size.width) / 2) * (toFloat xScale)

        yOffset : Int -> Float
        yOffset n      = ((toFloat n) - (toFloat level.size.height) / 2) * (toFloat yScale)

        mkLayer : [[a]] -> ((Int, [a]) -> [Form]) -> Element
        mkLayer grid mapRow =
                         let rows  = zip (reverse [0..level.size.height - 1]) grid
                             forms = concatMap mapRow rows
                         in  collage (w + xScale) (h + yScale) forms

        row : (a -> Form) -> (Int, [a]) -> [Form]
        row mkTile (n, tiles) = let tiles' = zip [0..level.size.width - 1] tiles
                                    makeTile (n', t) = move (xOffset n', yOffset n) <| mkTile t
                                in  map makeTile tiles'
    in layers [mkLayer grid (row tile), mkLayer grid (row tileOverlay)]

render2 : Grid.Grid GameModel.Tile -> Element
render2 level =
    let grid = Grid.toList level
        (w, h) = (level.size.width * xScale, level.size.height * yScale)
        (w', h') = (level.size.width, level.size.height)
        bg = mkLayer grid (row w' h' tile) level.size.height (w, h)
        overlay = mkLayer grid (row w' h' tileOverlay) level.size.height (w, h)
    in  layers [bg, overlay]

xScale : Int
xScale = 15

yScale : Int
yScale = 20

tile : GameModel.Tile -> Form
tile t =
    case t of
        GameModel.Floor -> floor
        GameModel.Wall  -> wall
        GameModel.Door  -> door
        GameModel.Acid  -> acid

tileOverlay : GameModel.Tile -> Form
tileOverlay t =
    case t of
        GameModel.Floor -> floorOverlay
        GameModel.Wall  -> wallOverlay
        GameModel.Door  -> doorOverlay
        GameModel.Acid  -> acidOverlay

floor : Form
floor = rect (toFloat xScale) (toFloat yScale) |> filled black

floorOverlay : Form
floorOverlay = guy {avatar = "." |> toText |> monospace |> Text.color white |> centered} GameModel.Visible

wall : Form
wall = rect (toFloat xScale) (toFloat yScale) |> filled grey

wallOverlay : Form
wallOverlay = guy {avatar = "#" |> toText |> monospace |> Text.color black |> centered} GameModel.Visible

door : Form
door = rect (toFloat xScale) (toFloat yScale) |> filled purple

doorOverlay : Form
doorOverlay = noForm

acid : Form
acid = rect (toFloat xScale) (toFloat yScale) |> filled darkGreen

acidOverlay : Form
acidOverlay = noForm

noForm : Form
noForm = toForm empty

guy : {r| avatar : Element} -> GameModel.Visibility -> Form
guy r visibility =
    case visibility of
        GameModel.Visible -> let form = r.avatar |> toForm
                                 (xSize, ySize) = sizeOf r.avatar
                                 x /// y = toFloat x / toFloat y -- This is a convenience function to divide two ints
                                 factor = min (xScale /// xSize) (yScale /// ySize)
                             in  scale factor form
        _                 -> noForm

xOffset : Int -> Int -> Float
xOffset n w = ((toFloat n) - (toFloat w) / 2) * (toFloat xScale)

yOffset : Int -> Int -> Float
yOffset n h = ((toFloat n) - (toFloat h) / 2) * (toFloat yScale)

mkLayer : [[a]] -> ((Int, [a]) -> [Form]) -> Int -> (Int, Int) -> Element
mkLayer grid mapRow h' (w, h) =
                 let rows  = zip (reverse [0..h' - 1]) grid
                     forms = concatMap mapRow rows
                 in  collage (w + xScale) (h + yScale) forms

row : Int -> Int -> (a -> Form) -> (Int, [a]) -> [Form]
row w h mkTile (n, tiles) = let tiles' = zip [0..w - 1] tiles
                                makeTile (n', t) = move (xOffset n' w, yOffset n h) <| mkTile t
                            in  map makeTile tiles'
