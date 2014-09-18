module Main where

import Keyboard
import String
import Text

import Grid

import GameModel
import GameUpdate
import GameView

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

initialPlayer : GameModel.Player
initialPlayer =
    "@"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
        |> GameModel.player

initalEnemy : GameModel.Enemy
initalEnemy =
    "e"
        |> toText
        |> monospace
        |> Text.color white
        |> centered
        |> GameModel.enemy

initialState : GameModel.State
initialState = GameModel.State initialPlayer initalEnemy initialLevel ["you enter the dungeon"]

inputs : Signal GameModel.Input
inputs = GameModel.handle <~ Keyboard.lastPressed

state : Signal GameModel.State
state = foldp GameUpdate.update initialState inputs

main : Signal Element
main = GameView.display <~ state
