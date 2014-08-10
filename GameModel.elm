module GameModel where

import Char (KeyCode)

import Grid
import Logging

type State = Logging.Logger GameState

type GameState = { player : Player
                 , level : Grid.Grid Tile
                 }

type Player = { location : Location
              , avatar : Element
              }


type Location = Grid.Coordinate
type Tile = Int

type Interface = { log : [String]
                 , info : Element
                 }

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

validLocation : Location -> GameState -> Bool
validLocation location state = Grid.inGrid location state.level

interface : State -> Interface
interface (state, log) = Interface log <| container 100 100 midTop (plainText "roguelike")

getState : State -> GameState
getState = Logging.get

makeState : Player -> Grid.Grid Tile -> State
makeState player grid = (GameState player grid, ["you enter the dungeon"])

initialPlayer : Player
initialPlayer = player <| asText '@'

initialGameState : GameState
initialGameState = GameState initialPlayer (Grid.initialize (Grid.Size 5 5) 0)

initialState : State
initialState = (initialGameState, ["you enter the dungeon"])
