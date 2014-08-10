module Main where

import Keyboard

import Grid
import Logging

import GameModel
import GameUpdate

inputs : Signal GameModel.Input
inputs = GameModel.handle <~ Keyboard.lastPressed

state : Signal GameModel.State
state = foldp GameUpdate.update GameModel.initialState inputs

display : GameModel.State -> Element
display state =
    let gameState = Logging.get state
        row x = flow right <| map (\t -> container 20 20 middle . asText <| t) x
        player = flow down [spacer 1 (20 * gameState.player.location.y), flow right [spacer (20 * gameState.player.location.x) 1, container 20 20 middle <| gameState.player.avatar]]
        interface = GameModel.interface state
    in  flow right [interface.info, flow down [layers [flow down <| map row (Grid.toList gameState.level), player], flow down <| map plainText (take 5 interface.log)]]

main : Signal Element
main = display <~ state

