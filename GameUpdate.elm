module GameUpdate where

import GameModel

log : String -> GameModel.State -> GameModel.State
log s state = {state| log <- s :: state.log}

update : GameModel.Input -> GameModel.State -> GameModel.State
update input state =
    let player   = state.player
        {x, y}   = player.location
        (x', y') = case input of
                        GameModel.Up    -> (0    , 0 - 1)
                        GameModel.Down  -> (0    , 0 + 1)
                        GameModel.Left  -> (0 - 1, 0    )
                        GameModel.Right -> (0 + 1, 0    )
                        GameModel.Nop   -> (0    , 0    )
        (x''', y''') = (x + x', y + y')
        enemy = state.enemy
        (x'', y'') = (enemy.location.x, enemy.location.y)
        state' =  if | (x'', y'') == (x''', y''') -> let (player', enemy', msg) = attack player enemy
                                                     in  log msg {state| player <- player', enemy <- enemy'}
                     | otherwise                  -> {state| player <- move (x', y') state player}
    in  cleanup state'


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

attack : {a| health : Int} -> {b| health : Int} -> ({a| health : Int}, {b| health : Int}, String)
attack dude1 dude2 =
    let hp1 = dude1.health - 1
        hp2 = dude2.health - 2
        msg = "you hit the enemy for 2 dmg. he hit you for 1 dmg"
    in  ({dude1| health <- hp1}, {dude2| health <- hp2}, msg)

cleanup : GameModel.State -> GameModel.State
cleanup state =
    let enemy = state.enemy
        enemy' = if enemy.health <= 0 then {enemy| location <- GameModel.location 0 0, avatar <- spacer 0 0} else enemy
        msg = if enemy'.location == enemy.location then Nothing else (Just "the enemy died")
    in  case msg of
            Nothing -> state
            Just m  -> log m {state| enemy <- enemy'}
