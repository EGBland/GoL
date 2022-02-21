module PPM (
    Pixel, Dimension, Image,
    writeImage, scaleImage
)
where

import Data.List.Split (chunksOf)
import Text.Printf (printf)

type Pixel = (Int,Int,Int)
type Dimension = (Int,Int)
type Image = (Dimension,[Pixel])

scaleImage :: Int -> Image -> Image
scaleImage n ((w,h),pxs) =
    let
        lines = chunksOf w pxs
        linesDuped = map (map $ replicate n) lines
        linesScaled = map concat linesDuped
        pxsScaled = concat . concat $ map (replicate n) linesScaled
    in
        ((w*n,h*n),pxsScaled)

writeImage :: Image -> String
writeImage ((w,h),pxs) =
    let
        header = printf "P3\n%d\t%d\t255\n" w h
        body = foldr (++) "" $ map writePixel pxs
    in header ++ body

writePixel :: Pixel -> String
writePixel (r,g,b) = printf "%d\t%d\t%d\n" r g b