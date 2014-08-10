module Main where

import Keyboard

import Grid

import GameModel
import GameUpdate

initialPlayer : GameModel.Player
initialPlayer = GameModel.player <| asText '@'

initialInterface : GameModel.Interface
initialInterface = GameModel.Interface ["you enter the dungeon"] <| container 100 100 midTop (plainText "roguelike")

initialState : GameModel.State
initialState = GameModel.State initialPlayer (Grid.initialize (Grid.Size 5 5) 0) initialInterface

inputs : Signal GameModel.Input
inputs = GameModel.handle <~ Keyboard.lastPressed

state : Signal GameModel.State
state = foldp GameUpdate.update initialState inputs

display : GameModel.State -> Element
display state =
    let row x = flow right <| map (\t -> container 20 20 middle . asText <| t) x
        player = flow down [spacer 1 (20 * state.player.location.y), flow right [spacer (20 * state.player.location.x) 1, container 20 20 middle <| state.player.avatar]]
    in  flow right [state.interface.info, flow down [layers [flow down <| map row (Grid.toList state.level), player], flow down <| map plainText (take 5 state.interface.log)]]

main : Signal Element
main = display <~ state

