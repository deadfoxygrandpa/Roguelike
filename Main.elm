module Main where

import Keyboard
import String

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
initialPlayer = GameModel.player << centered << monospace << toText <| "@"

initalEnemy : GameModel.Enemy
initalEnemy = GameModel.enemy << centered << monospace << toText <| "e"

initialState : GameModel.State
initialState = GameModel.State initialPlayer initalEnemy initialLevel ["you enter the dungeon"]

inputs : Signal GameModel.Input
inputs = GameModel.handle <~ Keyboard.lastPressed

state : Signal GameModel.State
state = foldp GameUpdate.update initialState inputs

xscale = 15
yscale = 20

display : GameModel.State -> Element
display state =
    let row x = flow right <| map (\t -> container xscale yscale middle << GameModel.showTile <| t) x
        player = flow down [spacer 1 (yscale * state.player.location.y), flow right [spacer (xscale * state.player.location.x) 1, container xscale yscale middle <| state.player.avatar]]
        enemy = flow down [spacer 1 (yscale * state.enemy.location.y), flow right [spacer (xscale * state.enemy.location.x) 1, container xscale yscale middle <| state.enemy.avatar]]
    in  flow right [(GameModel.interface state).info, flow down [layers [flow down <| map row (Grid.toList state.level), player, enemy], flow down <| map plainText (take 5 state.log)]]

main : Signal Element
main = GameView.display <~ state

