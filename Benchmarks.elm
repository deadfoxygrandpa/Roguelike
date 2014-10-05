module Benchmarks where

import String
import Text
import Either

import GameModel
import GameUpdate
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

main = Benchmark.run [ Benchmark.logic "listOf floats" (listOf2 Generator.float gen) [1, 10, 100, 1000, 2000]
                     , Benchmark.logic "listOf ints" (listOf2 Generator.int32 gen) [1, 10, 100, 1000, 2000]
                     , Benchmark.logic "new state" (uncurry stateTest) [(500, Either.Left state), (500, Either.Right state.player)]
                     ]