module Main where

import Codec.Picture

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
    where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

main :: IO ()
main = do
    imageCreator "/Users/Republic/examples/haskell/mandlebrot/test.png"
    print "success"
