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
                                  in  log msg { state| player <- player'
                                                     , enemies <- enemy' :: tail state.enemies
                                                     , generator <- gen
                                              }
                    Nothing    -> {state| player <- move (x', y') state player}
    in  state' |> cleanup |> ai |> reveal


move : (Int, Int) -> GameModel.State -> {a| location : GameModel.Location, initiative : Int} -> {a| location : GameModel.Location, initiative : Int}
move (x, y) state a =
    let location = GameModel.location (a.location.x + x) (a.location.y + y)
        initiative = a.initiative + 100
    in  case GameModel.pathable location state of
            False -> a
            True  -> {a| location <- location, initiative <- initiative}

attack : {a| coordination : Int, power : Int, initiative : Int, name : String}
      -> {b| stealth : Int, protection : Int, armor : Int, health : Int, name : String}
      -> GameModel.Random
      -> ({a| coordination : Int, power : Int, initiative : Int, name : String}, {b| stealth : Int, protection : Int, armor : Int, health : Int, name : String}, String, GameModel.Random )
attack dude1 dude2 generator =
    let (roll1, gen) = Generator.int32Range (1, 100) generator
        (roll2, gen') = Generator.int32Range (1, 100) gen
        hit = if roll1 > dude1.coordination - dude2.stealth then False else True
        guard = if dude1.coordination - dude2.stealth > 100 then dude2.protection - (dude1.coordination - dude2.stealth `rem` 100) else dude2.protection
        block = if hit == True && roll2 < guard then True else False
        dmg = if | hit && not block -> dude1.power
                 | hit && block     -> max 0 (dude1.power - dude2.armor)
                 | not hit          -> 0
        result = dude2.health - dmg
        msg = if not hit then dude1.name ++ " miss" else dude1.name ++ " hit " ++ dude2.name ++ " for " ++ show dmg ++  " dmg"
    in  ({dude1| initiative <- dude1.initiative + 100}, {dude2| health <- result}, msg, gen')

cleanup : GameModel.State -> GameModel.State
cleanup state =
    let dead = filter (\enemy -> enemy.health <= 0) state.enemies
        alive = filter (\enemy -> enemy.health > 0) state.enemies
        msg = if length dead == 0 then Nothing else Just (foldl (++) "" <| map (\enemy -> enemy.name ++ " died. ") dead)
    in  case msg of
            Nothing -> state
            Just m  -> log m {state| enemies <- alive}

ai : GameModel.State -> GameModel.State
ai state =
    let enemy = case filter (\enemy -> enemy.initiative <= state.player.initiative) state.enemies of
                    (enemy::es) -> Just enemy
                    [enemy]     -> Just enemy
                    []          -> Nothing
    in  case enemy of
                    Just enemy  -> let state'' = attackIfClose enemy state
                                   in ai state''
                    Nothing     -> state

attackIfClose : GameModel.Enemy -> GameModel.State -> GameModel.State
attackIfClose enemy state =
    case filter (\location -> location == state.player.location) (Grid.neighborhood enemy.location) of
                    [location] -> let (enemy', player', msg, gen) = attack enemy state.player state.generator
                                  in  log msg { state| player <- player'
                                                     , enemies <- enemy' :: tail state.enemies
                                                     , generator <- gen
                                              }
                    []         -> let ([x, y], gen') = Generator.listOf (Generator.int32Range (-1, 1)) 2 state.generator
                                      enemy' = move (x, y) state enemy
                                  in  {state| enemies <- enemy' :: tail state.enemies, generator <- gen'}

-- Right now this just reveals a box around the player
reveal : GameModel.State -> GameModel.State
reveal state =
    let explored  = Grid.map (\t -> if t == GameModel.Visible then GameModel.Explored else t) state.explored
        explored' = foldl (\l explored -> Grid.set l GameModel.Visible explored) explored (GameModel.visible state)
    in {state| explored <- explored'}
