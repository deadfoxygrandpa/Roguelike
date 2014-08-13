module GameUpdate where

import GameModel

log : String -> GameModel.State -> GameModel.State
log s state = {state| log <- s :: state.log}

update : GameModel.Input -> GameModel.State -> GameModel.State
update input state =
    let player = state.player
        location = player.location in
    case input of
        GameModel.Up    -> {state| player <- moveY -1 state player}
        GameModel.Down  -> {state| player <- moveY  1 state player}
        GameModel.Left  -> {state| player <- moveX -1 state player}
        GameModel.Right -> {state| player <- moveX  1 state player}
        GameModel.Nop   -> state

move : (Int, Int) -> GameModel.State -> {a| location : GameModel.Location} -> {a| location : GameModel.Location}
move (x, y) state a =
    let location = GameModel.location (a.location.x + x) (a.location.y + y)
    in  case GameModel.pathable location state of
            False -> a
            True  -> {a| location <- location}

moveX : Int -> GameModel.State -> {a| location : GameModel.Location} -> {a| location : GameModel.Location}
moveX x = move (x, 0)

moveY : Int -> GameModel.State -> {a| location : GameModel.Location} -> {a| location : GameModel.Location}
moveY y = move (0, y)
