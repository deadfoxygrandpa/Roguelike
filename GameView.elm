module GameView where

import Text

import GameModel
import Grid

xScale : Int
xScale = 15

yScale : Int
yScale = 20

noForm : Form
noForm = toForm empty

floor : Form
floor = group [ rect (toFloat xScale) (toFloat yScale) |> filled black
              , guy {avatar = "." |> toText |> monospace |> Text.color white |> centered} GameModel.Visible
              ]

wall : Form
wall = group [ rect (toFloat xScale) (toFloat yScale) |> filled grey
             , guy {avatar = "#" |> toText |> monospace |> Text.color black |> centered} GameModel.Visible
             ]

door : Form
door = rect (toFloat xScale) (toFloat yScale) |> filled purple

acid : Form
acid = rect (toFloat xScale) (toFloat yScale) |> filled darkGreen

fog : Form
fog = rect (toFloat xScale) (toFloat yScale) |> filled (rgba 0 0 0 1)

halfFog : Form
halfFog = rect (toFloat xScale) (toFloat yScale) |> filled (rgba 0 0 0 0.7)

tile : GameModel.Tile -> Form
tile t =
    case t of
        GameModel.Floor -> floor
        GameModel.Wall  -> wall
        GameModel.Door  -> door
        GameModel.Acid  -> acid

fogT : GameModel.Visibility -> Form
fogT visibility =
    case visibility of
        GameModel.Visible  -> noForm
        GameModel.Explored -> halfFog
        GameModel.Unexplored -> fog

player : Form
player = circle (toFloat xScale / 2) |> filled red

enemy : Form
enemy = circle (toFloat xScale / 2) |> filled green

guy : {r| avatar : Element} -> GameModel.Visibility -> Form
guy r visibility =
    case visibility of
        GameModel.Visible -> let form = r.avatar |> toForm
                                 (xSize, ySize) = sizeOf r.avatar
                                 x /// y = toFloat x / toFloat y
                                 factor = min (xScale /// xSize) (yScale /// ySize)
                             in  scale factor form
        _                 -> noForm

text = toText >> monospace >> Text.color white >> centered

mainScreen : GameModel.State -> Element
mainScreen state =
    let (w, h)         = (state.level.size.width * xScale, state.level.size.height * yScale)
        xOffset n      = ((toFloat n) - (toFloat state.level.size.width) / 2) * (toFloat xScale)
        yOffset n      = ((toFloat n) - (toFloat state.level.size.height) / 2) * (toFloat yScale)
        row (n, tiles) = let tiles' = zip [0..state.level.size.width - 1] tiles
                             makeTile (n', t) = move (xOffset n', yOffset n) <| tile t
                         in  map makeTile tiles'
        fogRow (n, tiles) = let tiles' = zip [0..state.level.size.width - 1] tiles
                                makeTile (n', t) = move (xOffset n', yOffset n) <| fogT t
                            in  map makeTile tiles'
        location r     = (xOffset r.location.x, 0 - yOffset (r.location.y + 1))
        player'        = guy state.player GameModel.Visible |> move (location state.player)
        enemy'         = group <| map (\enemy -> guy enemy (GameModel.visibility state enemy.location) |> move (location enemy)) state.enemies
        grid           = Grid.toList state.level
        bg             = let rows  = zip (reverse [0..state.level.size.height - 1]) grid
                             forms = concatMap row rows
                         in  collage (w + xScale) (h + yScale) forms
        pg             = collage (w + xScale) (h + yScale) [player', enemy']
        fogger         = let rows  = zip (reverse [0..state.level.size.height - 1]) (Grid.toList state.explored)
                             forms = concatMap fogRow rows
                         in  collage (w + xScale) (h + yScale) forms
    in  flow down [ layers [bg, pg, fogger]
                  , (flow down <| map text (take 3 state.log))
                  ]

sidebar : GameModel.State -> Element
sidebar state =
    let x = 5
        bar = flow down [ flow right [state.player.avatar, text ": You, Level ", text <| show state.player.level]
                        , flow right [text "Health: ", text <| show state.player.health]
                        ]
    in  container (widthOf bar + 20) (heightOf bar) midTop bar

display : GameModel.State -> Element
display state = flow right [sidebar state, mainScreen state] |> color black
