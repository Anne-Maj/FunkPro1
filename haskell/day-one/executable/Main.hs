-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified Example
import qualified Hello

import Arithmics
import Codec.Picture
import Codec.Picture.Types
import System.Environment

width :: Int
width = 800

height :: Int
height = 600

generateTrace :: DynamicImage
--generateTrace = ImageRGB8 (generateImage traceRay width height)
--generateTrace = ImageRGB8 (generateImage nicePaper width height)
generateTrace = ImageRGB8 (generateImage traceSphereRay width height)

traceRay :: Int -> Int -> PixelRGB8
traceRay px py =
  if 300 < px && px < 500 && 200 < py && py < 400
  then PixelRGB8 255 128 128
  else PixelRGB8 128 255 128

nicePaper :: Int -> Int -> PixelRGB8
nicePaper px py =
  if (mod px 20 == 0) || (mod py 20 == 0)
  then PixelRGB8 200 250 255
  else PixelRGB8 255 255 255

traceSphereRay :: Int -> Int -> PixelRGB8
traceSphereRay px py =
  case reflectedColor3D line of
    SkyColor -> PixelRGB8 200 230 255
    DarkColor -> PixelRGB8 80 0 0
    BrightColor -> PixelRGB8 255 255 240
  where
    x = ((fromIntegral px) - 400.0)/200.0
    y = (300.0 - (fromIntegral py))/200.0
    lookLine = lineFrom (Vector3D 0 0 1) (Vector3D x 3 y)
    line = reflectedLine3D sphere lookLine


main :: IO ()
main = do
  putStrLn "Starting trace ..."
  savePngImage "hello.png" generateTrace
  putStrLn "Done!"


  {-
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ( Hello.greet name )
  -}

-- main = Example.main
