module MapGen where

import Graphics.Input as Input

import Grid
import Generator
import Generator.Standard

import GameModel
import GameView

neighborhood : Grid.Coordinate -> [Grid.Coordinate]
neighborhood {x, y} = map (\(a, b) -> Grid.Coordinate a b)
    [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
    , (x - 1, y),     (x, y),     (x + 1, y)
    , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
    ]

neighborhood2 : Grid.Coordinate -> [Grid.Coordinate]
neighborhood2 {x, y} = map (\(a, b) -> Grid.Coordinate a b)
    [ (x - 2, y - 2), (x - 1, y - 2), (x, y - 2), (x + 1, y - 2), (x + 2, y - 2)
    , (x - 2, y - 1), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 2, y - 1)
    , (x - 2, y),     (x - 1, y),     (x, y),     (x + 1, y),     (x + 2, y)
    , (x - 2, y + 1), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1)
    , (x - 2, y + 2), (x - 1, y + 2), (x, y + 2), (x + 1, y + 2), (x + 2, y + 2)
    ]

getNeighborsOrElse : a -> Grid.Grid a -> Grid.Coordinate -> [a]
getNeighborsOrElse x grid coord =
    map (\c -> Grid.getOrElse x c grid) <| neighborhood coord

getNeighborsOrElse2 : a -> Grid.Grid a -> Grid.Coordinate -> [a]
getNeighborsOrElse2 x grid coord =
    map (\c -> Grid.getOrElse x c grid) <| neighborhood2 coord

getNeighbors : Grid.Grid GameModel.Tile -> Grid.Coordinate -> [GameModel.Tile]
getNeighbors = getNeighborsOrElse GameModel.Wall

getNeighbors2 : Grid.Grid GameModel.Tile -> Grid.Coordinate -> [GameModel.Tile]
getNeighbors2 = getNeighborsOrElse2 GameModel.Wall

numberOfWalls : Grid.Grid GameModel.Tile -> Grid.Coordinate -> Int
numberOfWalls grid coord =
    getNeighbors grid coord
        |> filter (\t -> t == GameModel.Wall)
        |> length

numberOfWalls2 : Grid.Grid GameModel.Tile -> Grid.Coordinate -> Int
numberOfWalls2 grid coord =
    getNeighbors2 grid coord
        |> filter (\t -> t == GameModel.Wall)
        |> length

randomTile : GameModel.Random -> (GameModel.Tile, GameModel.Random)
randomTile gen =
    let (p, gen') = Generator.float gen
        tile = if p < 0.40 then GameModel.Wall else GameModel.Floor
    in  (tile, gen')

randomMap : (Int, Int) -> GameModel.Random -> (Grid.Grid GameModel.Tile, GameModel.Random)
randomMap (w, h) gen =
    let row gen = Generator.listOf randomTile w gen
        x = scanl (\_ (ts, g) -> row g) (row gen) [1..h]
        rows = map fst x
        level = Grid.fromList rows
    in  (level, x |> last |> snd)

iterate : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
iterate grid =
    let coords = Grid.toCoordinates grid
        x = map (\coord -> (coord, if numberOfWalls grid coord >= 5 then GameModel.Wall else GameModel.Floor)) coords
    in  foldl (\(coord, a) grid -> Grid.set coord a grid) grid x

iterate2 : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
iterate2 grid =
    let coords = Grid.toCoordinates grid
        rule coord = if | numberOfWalls grid coord >= 5 -> GameModel.Wall
                        | numberOfWalls2 grid coord <= 2 -> GameModel.Wall
                        | otherwise -> GameModel.Floor
        x = map (\coord -> (coord, rule coord)) coords
    in  foldl (\(coord, a) grid -> Grid.set coord a grid) grid x

main = lift display state

display state =
    let level = fst state
    in  flow down <| button :: map (\x -> GameView.background x `above` spacer 10 10)
            [level |> iterate2 |> iterate2 |> iterate2 |> iterate2 |> iterate |> iterate |> iterate]

seed : Int
seed = 2013

gen : GameModel.Random
gen = Generator.Standard.generator seed

input = Input.input ()

button = Input.button input.handle () "clickar"

state : Signal (Grid.Grid GameModel.Tile, GameModel.Random)
state = foldp (\a state' -> randomMap dimensions (snd state')) (randomMap dimensions gen) input.signal

dimensions = (40, 30)
