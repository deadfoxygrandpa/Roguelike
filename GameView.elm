module GameView where

import Text

import GameModel
import Grid

xScale : Int
xScale = 15

yScale : Int
yScale = 20

floor : Form
floor = group [ rect (toFloat xScale) (toFloat yScale) |> filled black
              , guy {avatar = "." |> toText |> monospace |> Text.color white |> centered} -- rect (toFloat xScale) (toFloat yScale) |> filled black
              ]

wall : Form
wall = group [ rect (toFloat xScale) (toFloat yScale) |> filled grey
             , guy {avatar = "#" |> toText |> monospace |> Text.color white |> centered}
             ]

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

guy : {r| avatar : Element} -> Form
guy r =
    let form = r.avatar |> toForm
        (xSize, ySize) = sizeOf r.avatar
        x /// y = toFloat x / toFloat y
        factor = min (xScale /// xSize) (yScale /// ySize)
    in  scale factor form

display : GameModel.State -> Element
display state =
    let (w, h)         = (state.level.size.width * xScale, state.level.size.height * yScale)
        xOffset n      = ((toFloat n) - (toFloat state.level.size.width) / 2) * (toFloat xScale)
        yOffset n      = ((toFloat n) - (toFloat state.level.size.height) / 2) * (toFloat yScale)
        row (n, tiles) = let tiles' = zip [0..state.level.size.width - 1] tiles
                             makeTile (n', t) = move (xOffset n', yOffset n) <| tile t
                         in  map makeTile tiles'
        playerLocation = (xOffset state.player.location.x, 0 - yOffset (state.player.location.y + 1))
        player'        = guy state.player |> move playerLocation
        enemyLocation  = (xOffset state.enemy.location.x, 0 - yOffset (state.enemy.location.y + 1))
        enemy'         = guy state.enemy |> move enemyLocation
        grid           = Grid.toList state.level
        bg             = let rows  = zip [0..state.level.size.height - 1] grid
                             forms = concatMap row rows
                         in  collage (w + xScale) (h + yScale) forms
        pg             = collage (w + xScale) (h + yScale) [player', enemy']
    in  flow down [ layers [bg, pg]
                  , flow down <| map (toText >> monospace >> centered) state.log
                  ]
