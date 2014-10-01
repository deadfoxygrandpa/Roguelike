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
                                 x /// y = toFloat x / toFloat y -- This is a convenience function to divide two ints
                                 factor = min (xScale /// xSize) (yScale /// ySize)
                             in  scale factor form
        _                 -> noForm

text = toText >> monospace >> Text.color white >> centered

mainScreen : GameModel.State -> Element
mainScreen state =
    let (w, h)         = (state.level.size.width * xScale, state.level.size.height * yScale)

        xOffset : Int -> Float
        xOffset n      = ((toFloat n) - (toFloat state.level.size.width) / 2) * (toFloat xScale)

        yOffset : Int -> Float
        yOffset n      = ((toFloat n) - (toFloat state.level.size.height) / 2) * (toFloat yScale)

        location : {r| location : GameModel.Location} -> (Float, Float)
        location r     = (xOffset r.location.x, 0 - yOffset (r.location.y + 1))

        mkLayer : [[a]] -> ((Int, [a]) -> [Form]) -> Element
        mkLayer grid mapRow =
                         let rows  = zip (reverse [0..state.level.size.height - 1]) grid
                             forms = concatMap mapRow rows
                         in  collage (w + xScale) (h + yScale) forms

        row : (a -> Form) -> (Int, [a]) -> [Form]
        row mkTile (n, tiles) = let tiles' = zip [0..state.level.size.width - 1] tiles
                                    makeTile (n', t) = move (xOffset n', yOffset n) <| mkTile t
                                in  map makeTile tiles'

        player'        = guy state.player GameModel.Visible |> move (location state.player)
        enemy'         = let mkEnemy enemy = guy enemy (GameModel.visibility state enemy.location)
                                                |> move (location enemy)
                         in  group <| map mkEnemy state.enemies
        grid           = Grid.toList state.level
        bg             = mkLayer grid (row tile)
        pg             = collage (w + xScale) (h + yScale) [player', enemy']
        fogger         = mkLayer (Grid.toList state.explored) (row fogT)
    in  flow down [ layers [bg, pg, fogger]
                  , (flow down <| map text (take 3 state.log))
                  ]

background : Grid.Grid GameModel.Tile -> Element
background level =
    let grid = Grid.toList level
        (w, h) = (level.size.width * xScale, level.size.height * yScale)

        xOffset : Int -> Float
        xOffset n      = ((toFloat n) - (toFloat level.size.width) / 2) * (toFloat xScale)

        yOffset : Int -> Float
        yOffset n      = ((toFloat n) - (toFloat level.size.height) / 2) * (toFloat yScale)

        mkLayer : [[a]] -> ((Int, [a]) -> [Form]) -> Element
        mkLayer grid mapRow =
                         let rows  = zip (reverse [0..level.size.height - 1]) grid
                             forms = concatMap mapRow rows
                         in  collage (w + xScale) (h + yScale) forms

        row : (a -> Form) -> (Int, [a]) -> [Form]
        row mkTile (n, tiles) = let tiles' = zip [0..level.size.width - 1] tiles
                                    makeTile (n', t) = move (xOffset n', yOffset n) <| mkTile t
                                in  map makeTile tiles'
    in mkLayer grid (row tile)

sidebar : GameModel.State -> Element
sidebar state =
    let x = 5
        bar = flow down [ flow right [state.player.avatar, text ": You"]
                        , flow right [text "Health: ", text <| show state.player.health]
                        , flow right [text "Energy: ", text <| show state.player.energy]
                        , flow right [text "Hunger: ", text <| show state.player.hunger]
                        , flow right [text "Stealth: ", text <| show state.player.stealth ++ "%"]
                        , flow right [text "Armor: ", text <| show state.player.armor]
                        , flow right [text "Protection: ", text <| show state.player.protection ++ "%"]
                        , flow right [text "Coordination: ", text <| show state.player.coordination ++ "%"]
                        , flow right [text "Power: ", text <| show state.player.power]
                        ]
    in  container (widthOf bar + 20) (heightOf bar) midTop bar

display : GameModel.State -> Element
display state = flow right [sidebar state, mainScreen state] |> color black
