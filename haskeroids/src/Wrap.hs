module Wrap
( wrap
, wrapPolygon
, wrapCircle
, wrapTranslate
, wrapRotate
, wrapScale
, wrapTriangle
, wrapAsteroid
) where

{-

This Module defines objects that can wrap arround the screen as well as functions to deal with these objects

-}

import Graphics.Gloss

width, height :: Int
width = 400
height = 300

toInt :: Float -> Int
toInt = round

inBound :: Float -> Float -> Float
inBound x y
  | x < - y = x + 2*y
  | x > y =  x - 2*y
  | otherwise = x

--makes sure every pixel is shown within the boundaries of the screen 
wrap :: (Float, Float) -> (Float, Float)
wrap (xi, yi) = (xf, yf)
  where
    xf = inBound xi (fromIntegral width)
    yf = inBound yi (fromIntegral height)

--Draws a point on the screen
drawPoint :: (Float, Float) -> Picture
drawPoint (xi, yi) = translate xf yf (rectangleSolid 1 1)
  where
    (xf, yf) = wrap (xi, yi)

lineEq :: Float -> Float -> Float -> Float -> Float -> Float
lineEq x1 x2 y1 y2 y = (x1-x2)*(y-y2)/(y1-y2) + x2

pointsOnAxis :: Float -> Float -> Float -> Float -> [Float]
pointsOnAxis x1 x2 y1 y2 = map (lineEq x1 x2 y1 y2) [y1, (y1+1)..y2]

range :: Float -> Float -> [Float]
range a b
  | a > b = [b, (b+1)..a]
  | otherwise = [a, (a+1) ..b]

hLargerLine :: Float -> Float -> Float -> Float -> [(Float, Float)]
hLargerLine x1 x2 y1 y2 = zip ab (range y1 y2)
    where
      ab = if y2 >= y1 then pointsOnAxis x1 x2 y1 y2 else pointsOnAxis  x2 x1 y2 y1

wLargerLine :: Float -> Float -> Float -> Float -> [(Float, Float)]
wLargerLine x1 x2 y1 y2 = zip (range x1 x2) ord
    where
      ord = if x2 >= x1 then pointsOnAxis y1 y2 x1 x2 else pointsOnAxis y2 y1 x2 x1

-- Draws a line in between two given points
wrapLine :: (Float, Float) -> (Float, Float) -> Picture
wrapLine (x1, y1) (x2, y2) = pictures $ map drawPoint ps
  where
    h = abs (y2-y1)
    w = abs (x2-x1)
    ps = if h>w then hLargerLine x1 x2 y1 y2 else wLargerLine x1 x2 y1 y2

wrapLines :: [(Float, Float)] -> [Picture]
wrapLines [] = []
wrapLines [_] = []
wrapLines (p1:p2:ps) = wrapLine p1 p2 : wrapLines (p2:ps)

-- Draws a closed polygon over a given path
wrapPolygon :: [(Float, Float)] -> Picture
wrapPolygon ps@(p:s) = pictures cycle
  where
    firstLine = wrapLine p (last s)
    cycle = firstLine : wrapLines ps

-- Draws a circle of a given radius
wrapCircle ::  Float -> [(Float, Float)]
wrapCircle r = zip ab ord
  where
    ab = map calcAb [0..31]
    ord = map calcOrd [0..31]
    calcAb  x' = r * cos( x' *2*pi/ 32)
    calcOrd y' = r * sin( y' *2*pi/ 32)

vectorTranslate :: Float -> Float -> (Float, Float) -> (Float, Float)
vectorTranslate dsx dsy (xi, yi) = (xi+dsx, yi+dsy)

-- Translates a list of points
wrapTranslate :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
wrapTranslate x y = map (vectorTranslate x y)

vectorRotate :: Float -> (Float, Float) -> (Float, Float)
vectorRotate ang (x, y) = (x', y')
  where
    x' = x * cos (-ang) - y * sin (-ang)
    y' = x * sin (-ang) + y * cos (-ang)

-- Rotates a list of points
wrapRotate :: Float -> [(Float, Float)] -> [(Float, Float)]
wrapRotate ang = map (vectorRotate ang)

vectorScale :: Float -> Float -> (Float, Float) -> (Float, Float)
vectorScale fx fy (x, y) = (fx*x, fy*y)

wrapScale :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
wrapScale fx fy = map (vectorScale fx fy)

wrapTriangle :: [(Float, Float)]
wrapTriangle = [(-10, -20), (0, 20), (10, -20)]

wrapAsteroid :: [(Float, Float)]
wrapAsteroid = [(1.0184048,-0.33803117),(0.6781589,-0.85806495),(-3.553705e-2,-0.39837918),(-0.7087101,-0.8714452),(-1.1164334,-0.3721128),(-1.1164334,0.37722918),(-0.7085196,0.89140975),(0.2619333,0.89140975),(0.7285556,0.106024064),(0.99858093,0.5719606)]
