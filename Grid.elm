module Grid where

import List
import Array

type Grid a = { grid : Array.Array (Array.Array a), size : Size }
type Size = { width : Int, height : Int }
type Coordinate = { x : Int, y : Int }

initialize : Size -> a -> Grid a
initialize ({width, height} as size) a = Grid (Array.repeat height << Array.repeat width <| a) size

toList : Grid a -> [[a]]
toList = List.map Array.toList << Array.toList << .grid

fromList : [[a]] -> Grid a
fromList xs =
    let row x = Array.fromList x
        grid = Array.fromList <| List.map row xs
    in  Grid grid <| Size (length << head <| xs) (length xs)

toCoordinates : Grid a -> [Coordinate]
toCoordinates gridder =
    let s : Size
        s = gridder.size
        xs : Int -> [(Int, Int)]
        xs y = List.map (\x -> (x, y)) [0..s.width - 1]
        pairs = concatMap xs [0..s.height - 1]
    in  List.map (\(x, y) -> Coordinate x y) pairs

set : Coordinate -> a -> Grid a -> Grid a
set {x, y} a grid =
    if | x < 0 -> grid
       | y < 0 -> grid
       | x >= grid.size.width  -> grid
       | y >= grid.size.height -> grid
       | otherwise -> let row = Array.getOrFail y grid.grid
                          row' = Array.set x a row
                      in  {grid | grid <- Array.set y row' grid.grid}

get : Coordinate -> Grid a -> Maybe a
get {x, y} grid =
    case Array.get y grid.grid of
        Nothing  -> Nothing
        Just row -> Array.get x row

getOrFail : Coordinate -> Grid a -> a
getOrFail {x, y} grid =
    let row = Array.getOrFail y grid.grid
    in  Array.getOrFail x row

getOrElse : a -> Coordinate -> Grid a -> a
getOrElse default coordinate grid =
    case get coordinate grid of
        Nothing -> default
        Just a  -> a

getRow : Int -> Grid a -> Maybe [a]
getRow n grid =
    if | n < 0                 -> Nothing
       | n >= grid.size.height -> Nothing
       | otherwise             -> Just << getRowOrFail n <| grid

getRowOrFail : Int -> Grid a -> [a]
getRowOrFail n = Array.toList << Array.getOrFail n << .grid

getRowOrElse : [a] -> Int -> Grid a -> [a]
getRowOrElse default n grid =
    case getRow n grid of
        Nothing  -> default
        Just row -> row

getColumn : Int -> Grid a -> Maybe [a]
getColumn n grid =
    if | n < 0                -> Nothing
       | n >= grid.size.width -> Nothing
       | otherwise            -> Just << getColumnOrFail n <| grid

getColumnOrFail : Int -> Grid a -> [a]
getColumnOrFail n = List.map (Array.getOrFail n) << Array.toList << .grid

getColumnOrElse : [a] -> Int -> Grid a -> [a]
getColumnOrElse default n grid =
    case getColumn n grid of
        Nothing     -> default
        Just column -> column

inGrid : Coordinate -> Grid a -> Bool
inGrid {x, y} grid =
    let {height, width} = grid.size
    in  if | x < 0       -> False
           | x >= width  -> False
           | y < 0       -> False
           | y >= height -> False
           | otherwise   -> True

map : (a -> b) -> Grid a -> Grid b
map f grid =
    let grid' = Array.map (\row -> Array.map f row) grid.grid
    in  {grid| grid <- grid'}

neighborhood : Coordinate -> [Coordinate]
neighborhood {x, y} = List.map (\(a, b) -> Coordinate a b)
    [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
    , (x - 1, y),     (x, y),     (x + 1, y)
    , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
    ]

neighborhood2 : Coordinate -> [Coordinate]
neighborhood2 {x, y} = List.map (\(c, b) -> Coordinate c b)
    [ (x - 2, y - 2), (x - 1, y - 2), (x, y - 2), (x + 1, y - 2), (x + 2, y - 2)
    , (x - 2, y - 1), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 2, y - 1)
    , (x - 2, y),     (x - 1, y),     (x, y),     (x + 1, y),     (x + 2, y)
    , (x - 2, y + 1), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1)
    , (x - 2, y + 2), (x - 1, y + 2), (x, y + 2), (x + 1, y + 2), (x + 2, y + 2)
    ]