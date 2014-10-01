module Main where

import Keyboard
import String
import Text

import Grid
import Generator
import Generator.Standard

import GameModel
import GameUpdate
import GameView

port title : String
port title = "Chimera"

seed : Int
seed = 2014

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

initialPlayer : GameModel.Random -> (GameModel.Player, GameModel.Random)
initialPlayer gen =
    let elem = "@"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
    in  GameModel.player (elem, gen)

initialEnemy : GameModel.Random -> (GameModel.Enemy, GameModel.Random)
initialEnemy gen =
    let elem = "e"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
    in GameModel.enemy (elem, gen)

initialState : GameModel.State
initialState = 
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

inputs : Signal GameModel.Input
inputs = GameModel.handle <~ Keyboard.lastPressed

state : Signal GameModel.State
state = foldp GameUpdate.update initialState inputs

main : Signal Element
main = GameView.display <~ state
