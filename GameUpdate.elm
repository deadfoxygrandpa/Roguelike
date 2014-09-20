module GameUpdate where

import Grid
import Generator

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
        (x'', y'') = (x + x', y + y')
        enemy = case filter (\enemy -> enemy.location == GameModel.location x'' y'') state.enemies of
                    (enemy::es) -> Just enemy
                    [enemy]     -> Just enemy
                    []          -> Nothing
        state' =  case enemy of
                    Just enemy -> let (player', enemy', msg, gen) = attack player enemy state.generator
                                  in  log msg {state| player <- player', enemies <- enemy' :: tail state.enemies, generator <- gen}
                    Nothing    -> {state| player <- move (x', y') state player}
    in  state' |> reveal |> cleanup


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

attack : {a| health : Int} -> {b| health : Int} -> GameModel.Random -> ({a| health : Int}, {b| health : Int}, String, GameModel.Random)
attack dude1 dude2 generator =
    let hp1 = dude1.health - 1
        (dmg, gen) = Generator.int32Range (1, 5) generator
        hp2 = dude2.health - dmg
        msg = "you hit the enemy for " ++ show dmg ++  " dmg"
    in  ({dude1| health <- hp1}, {dude2| health <- hp2}, msg, gen)

cleanup : GameModel.State -> GameModel.State
cleanup state =
    let enemies' = filter alive state.enemies
        alive enemy = enemy.health > 0
        msg = if length enemies' == length state.enemies then Nothing else (Just "the enemy died")
    in  case msg of
            Nothing -> state
            Just m  -> log m {state| enemies <- enemies'}

-- Right now this just reveals a box around the player
reveal : GameModel.State -> GameModel.State
reveal state =
    let explored  = Grid.map (\t -> if t == GameModel.Visible then GameModel.Explored else t) state.explored
        explored' = foldl (\l explored -> Grid.set l GameModel.Visible explored) explored (GameModel.visible state)
    in {state| explored <- explored'}
