module GameModel where

import Char (KeyCode)

import Grid

type State = { player : Player
             , enemies : [Enemy]
             , level : Grid.Grid Tile
             , explored : Grid.Grid Bool
             , log : [String]
             }

type Player = { location : Location
              , avatar : Element
              , health : Int
              }

type Enemy = { location : Location
             , avatar : Element
             , health : Int
             }

type Location = Grid.Coordinate
data Tile = Floor
          | Wall
          | Door
          | Acid

type Interface = { info : Element }

data Input = Up | Down | Left | Right | Nop

player : Element -> Player
player elem = Player (Grid.Coordinate 2 2) elem 10

enemy : Element -> Enemy
enemy elem = Enemy (Grid.Coordinate 14 4) elem 10

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
interface state = Interface <| container 100 100 midTop (flow down [plainText "roguelike", plainText <| "your hp: " ++ show state.player.health])

showTile : Tile -> Element
showTile tile =
    let c = case tile of
                Floor -> " "
                Wall  -> "#"
                Door  -> "+"
                Acid  -> "~"
    in  centered << monospace << toText <| c
