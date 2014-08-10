module GameModel where

import Char (KeyCode)

import Grid

type State = { player : Player
             , level : Grid.Grid Tile
             , log : [String]
             }

type Player = { location : Location
              , avatar : Element
              }


type Location = Grid.Coordinate
type Tile = Int

type Interface = { info : Element }

data Input = Up | Down | Left | Right | Nop

player : Element -> Player
player elem = Player (Grid.Coordinate 0 0) elem

location : Int -> Int -> Location
location = Grid.Coordinate

handle : KeyCode -> Input
handle key =
    case key of
        37 -> Left
        38 -> Up
        39 -> Right
        40 -> Down
        _  -> Nop

validLocation : Location -> State -> Bool
validLocation location state = Grid.inGrid location state.level

interface : State -> Interface
interface state = Interface <| container 100 100 midTop (plainText "roguelike")
