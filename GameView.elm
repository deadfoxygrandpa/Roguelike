module GameView where

import GameModel
import Grid

xScale : Int
xScale = 15

yScale : Int
yScale = 20

floor : Form
floor = filled black <| rect (toFloat xScale) (toFloat yScale)

wall : Form
wall = filled grey <| rect (toFloat xScale) (toFloat yScale)

door : Form
door = filled purple <| rect (toFloat xScale) (toFloat yScale)

acid : Form
acid = filled darkGreen <| rect (toFloat xScale) (toFloat yScale)

tile : GameModel.Tile -> Form
tile t =
    case t of
        GameModel.Floor -> floor
        GameModel.Wall  -> wall
        GameModel.Door  -> door
        GameModel.Acid  -> acid

player : Form
player = filled red <| circle (toFloat xScale / 2)

enemy : Form
enemy = filled green <| circle (toFloat xScale / 2)

display : GameModel.State -> Element
display state =
    let (w, h) = (state.level.size.width * xScale, state.level.size.height * yScale)
        xOffset n = ((toFloat n) - (toFloat state.level.size.width) / 2) * (toFloat xScale)
        yOffset n = ((toFloat n) - (toFloat state.level.size.height) / 2) * (toFloat yScale)
        row n tiles = map (\(n', t) -> move (xOffset n', yOffset n) <| tile t) <| zip [0..state.level.size.width - 1] tiles
        pl = (xOffset state.player.location.x, 0 - yOffset (state.player.location.y + 1))
        player' = move pl player
        el = (xOffset state.enemy.location.x, 0 - yOffset (state.enemy.location.y + 1))
        enemy' = move el enemy
        grid = Grid.toList state.level
        bg = collage w h <|(concatMap (\(n, t) -> row n t) <| zip [0..state.level.size.height - 1] grid)
        pg = collage w h [player', enemy']
    in  flow down [layers [bg, pg], flow down <| map asText state.log]
