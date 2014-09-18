module GameView where

import GameModel
import Grid

xScale : Int
xScale = 15

yScale : Int
yScale = 20

floor : Form
floor = rect (toFloat xScale) (toFloat yScale) |> filled black

wall : Form
wall = rect (toFloat xScale) (toFloat yScale) |> filled grey

door : Form
door = rect (toFloat xScale) (toFloat yScale) |> filled purple

acid : Form
acid = rect (toFloat xScale) (toFloat yScale) |> filled darkGreen

tile : GameModel.Tile -> Form
tile t =
    case t of
        GameModel.Floor -> floor
        GameModel.Wall  -> wall
        GameModel.Door  -> door
        GameModel.Acid  -> acid

player : Form
player = circle (toFloat xScale / 2) |> filled red

enemy : Form
enemy = circle (toFloat xScale / 2) |> filled green

display : GameModel.State -> Element
display state =
    let (w, h)         = (state.level.size.width * xScale, state.level.size.height * yScale)
        xOffset n      = ((toFloat n) - (toFloat state.level.size.width) / 2) * (toFloat xScale)
        yOffset n      = ((toFloat n) - (toFloat state.level.size.height) / 2) * (toFloat yScale)
        row n tiles    = let tiles' = zip [0..state.level.size.width - 1] tiles
                             makeTile (n', t) = move (xOffset n', yOffset n) <| tile t
                         in  map makeTile tiles'
        playerLocation = (xOffset state.player.location.x, 0 - yOffset (state.player.location.y + 1))
        player'        = move playerLocation player
        enemyLocation  = (xOffset state.enemy.location.x, 0 - yOffset (state.enemy.location.y + 1))
        enemy'         = move enemyLocation enemy
        grid           = Grid.toList state.level
        bg             = collage (w + xScale) (h + yScale) <|(concatMap (\(n, t) -> row n t) <| zip [0..state.level.size.height - 1] grid)
        pg             = collage (w + xScale) (h + yScale) [player', enemy']
    in  flow down [ layers [bg, pg]
                  , flow down <| map (toText >> monospace >> centered) state.log
                  ]
