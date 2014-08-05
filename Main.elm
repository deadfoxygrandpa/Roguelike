module Main where

import Array
import Keyboard
import Char (KeyCode)

import Grid

type State = { player : Player
             , level : Grid.Grid Tile
             , interface : Interface
             }

type Player = { location : Grid.Coordinate
              , avatar : Char
              }

type Tile = Int

type Interface = { log : [String]
                 , info : Element
                 }

initialPlayer : Player
initialPlayer = Player (Grid.Coordinate 2 2) '@'

initialInterface : Interface
initialInterface = Interface ["you enter the dungeon"] <| container 100 100 midTop (plainText "roguelike")

initialState : State
initialState = State initialPlayer (Grid.initialize (Grid.Size 5 5) 0) initialInterface

data Input = Up | Down | Left | Right | Nop

update : Input -> State -> State
update input state =
    let player = state.player
        location = player.location in
    case input of
        Up    -> {state| player <- {player| location  <- {location| y <- location.y - 1}}}
        Down  -> {state| player <- {player| location  <- {location| y <- location.y + 1}}}
        Left  -> {state| player <- {player| location  <- {location| x <- location.x - 1}}}
        Right -> {state| player <- {player| location  <- {location| x <- location.x + 1}}}
        Nop   -> state

handle : KeyCode -> Input
handle key =
    case key of
        37 -> Left
        38 -> Up
        39 -> Right
        40 -> Down
        _  -> Nop

inputs : Signal Input
inputs = handle <~ Keyboard.lastPressed

state : Signal State
state = foldp update initialState inputs

display : State -> Element
display state =
    let row x = flow right <| map (\t -> container 20 20 middle . asText <| t) (Array.toList x)
        player = flow down [spacer 1 (20 * state.player.location.y), flow right [spacer (20 * state.player.location.x) 1, container 20 20 middle . asText <| state.player.avatar]]
    in  flow right [state.interface.info, flow down [layers [flow down <| map row (Array.toList state.level.grid), player], flow down <| map plainText (take 5 state.interface.log)]]

main = display <~ state

