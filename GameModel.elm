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
data Tile = Floor
          | Wall
          | Door
          | Acid

type Interface = { info : Element }

data Input = Up | Down | Left | Right | Nop

player : Element -> Player
player elem = Player (Grid.Coordinate 2 2) elem

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

pathable : Location -> State -> Bool
pathable location state =
    let level = state.level
        tile  = Grid.get location level
    in  case tile of
            Nothing -> False
            Just Floor  -> True
            Just _  -> False

interface : State -> Interface
interface state = Interface <| container 100 100 midTop (plainText "roguelike")

showTile : Tile -> Element
showTile tile =
    let c = case tile of
                Floor -> " "
                Wall  -> "#"
                Door  -> "+"
                Acid  -> "~"
    in  centered . monospace . toText <| c
