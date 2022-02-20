module Main where

import Prelude hiding (lookup)
import qualified Prelude as P

import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import PPM hiding (Dimension)

lookup :: (Eq a) => [(a,b)] -> a -> Maybe b
lookup = flip P.lookup

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

type Tile = (Int,Int)
type TileState = (Tile,Bool)
type Dimension = (Int,Int)
type GameState = (Dimension,[TileState])

runGame :: Int -> GameState -> GameState
runGame n init = foldr (\_ acc -> stepGame acc) init [1..n]

stepGame :: GameState -> GameState
stepGame gs =
    let
        ((w,h),grid0) = gs
        grid1 = map (stepTile gs) [(x,y) | x <- [1..w], y <- [1..h]]
    in
        ((w,h),grid1)

stepTile :: GameState -> Tile -> TileState
stepTile gs (x,y) =
    let
        moore  = mooreSum gs (x,y)
        state0 = lookupTile gs (x,y)
        state1 = (state0 && (moore == 2 || moore == 3)) || (not state0 && moore == 3)
    in
        ((x,y),state1)

mooreSum :: GameState -> Tile -> Int
mooreSum gs = length . (filter id) . (map $ lookupTile gs) . surroundings

surroundings :: Tile -> [Tile]
surroundings (x0,y0) = [(x,y) | x <- [x0-1..x0+1], y <- [y0-1..y0+1], (x /= x0 || y /= y0)]

lookupTile :: GameState -> Tile -> Bool
lookupTile (_,tiles) = (fromMaybe False) . (lookup tiles)

glider :: [TileState]
glider =
    [
        ((1,1),False),
        ((2,1),True),
        ((3,1),False),
        ((1,2),False),
        ((2,2),False),
        ((3,2),True),
        ((1,3),True),
        ((2,3),True),
        ((3,3),True)
    ]

blinker :: [TileState]
blinker = [((x,y),y==2) | x <- [1..3], y <- [1..3]]

printGameLine :: GameState -> Int -> IO ()
printGameLine ((w,h),grid) y = putStrLn $ map (\x -> if' x 'x' ' ') [lookupTile ((w,h),grid) (x,y) | x <- [1..w]]

printGame :: GameState -> IO ()
printGame ((w,h),grid) = sequence_ [printGameLine ((w,h),grid) y | y <- [1..h]]


tile2pixel :: TileState -> Pixel
tile2pixel (_,True) = (0,0,0)
tile2pixel (_,False) = (255,255,255)

game2image :: GameState -> Image
game2image ((w,h),tiles) =
    let
        tilesFull = [((x,y),lookupTile ((w,h),tiles) (x,y)) | x <- [1..w], y <- [1..h]]
        pixels    = map tile2pixel tilesFull
    in
        ((w,h),pixels)

testGame :: GameState
testGame = ((20,20),glider)


writeState :: (Int,GameState) -> IO ()
writeState (n,gs) = do
    let filename = printf "state%d.ppm" n
    writeFile filename . writeImage . game2image $ gs

main :: IO ()
main = do
    let states = scanr (\x (_,acc) -> (x,stepGame acc)) (0,testGame) [1..20]
    sequence_ $ map writeState states
