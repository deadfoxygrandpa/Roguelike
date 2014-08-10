module GameUpdate where

import GameModel
import Logging

tell : String -> GameModel.State
tell s = (GameModel.initialGameState, [s])

update : GameModel.Input -> GameModel.State -> GameModel.State
update input state =
    let gameState = GameModel.getState state
        player = gameState.player
        location = player.location in
    case input of
        GameModel.Up    -> tell "moved u" `Logging.and` (Logging.set state {gameState| player <- moveY -1 gameState player})
        GameModel.Down  -> tell "moved d" `Logging.and` (Logging.set state {gameState| player <- moveY  1 gameState player})
        GameModel.Left  -> tell "moved l" `Logging.and` (Logging.set state {gameState| player <- moveX -1 gameState player})
        GameModel.Right -> tell "moved r" `Logging.and` (Logging.set state {gameState| player <- moveX  1 gameState player})
        GameModel.Nop   -> state

move : (Int, Int) -> GameModel.GameState -> {a| location : GameModel.Location} -> {a| location : GameModel.Location}
move (x, y) state a =
    let location = GameModel.location (a.location.x + x) (a.location.y + y)
    in  case GameModel.validLocation location state of
            False -> a
            True  -> {a| location <- location}

moveX : Int -> GameModel.GameState -> {a| location : GameModel.Location} -> {a| location : GameModel.Location}
moveX x = move (x, 0)

moveY : Int -> GameModel.GameState -> {a| location : GameModel.Location} -> {a| location : GameModel.Location}
moveY y = move (0, y)
