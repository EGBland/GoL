module PPM (
    Pixel, Dimension, Image,
    writeImage
)
where

import Text.Printf (printf)

type Pixel = (Int,Int,Int)
type Dimension = (Int,Int)
type Image = (Dimension,[Pixel])

writeImage :: Image -> String
writeImage ((w,h),pxs) =
    let
        header = printf "P3\n%d\t%d\t255\n" w h
        body = foldr (++) "" $ map writePixel pxs
    in header ++ body

writePixel :: Pixel -> String
writePixel (r,g,b) = printf "%d\t%d\t%d\n" r g b