module Main where

import Keyboard
import String
import Text

import Grid
import Generator
import Generator.Standard

import GameModel
import GameUpdate
import WebGLView
import MapGen

port title : String
port title = "Chimera"

seed : Int
seed = 2015

dimensions : (Int, Int)
dimensions = (30, 20)

gen : GameModel.Random
gen = Generator.Standard.generator seed

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

inputs : Signal GameModel.Input
inputs = GameModel.handle <~ Keyboard.lastPressed

state : Signal GameModel.State
state = foldp GameUpdate.update initialState inputs

main : Signal Element
main = WebGLView.display state
