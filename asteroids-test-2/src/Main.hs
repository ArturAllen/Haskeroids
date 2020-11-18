module Main where

import Graphics.Gloss.Interface.Pure.Game
import System.Random

--           ((x,y),           vx     vy       acc    ang,   angVel, pic)
type Model = ((Float, Float), (Float, Float), Float, Float, Float,  Picture)

type Bullet = ((Float, Float), Float, Float, Picture)

type Asteroid = ((Float, Float), Float, Float, Float)

type World = (Model, [Asteroid], [Bullet])

width, height :: Int
width = 400
height = 300

wrap :: (Float, Float) -> (Float, Float)
wrap (x, y) = (x', y')
  where
    x'
      | x > fWidth = x - 2*fWidth
      | x < -fWidth = x + 2*fWidth
      | otherwise = x
    y'
      | y > fHeight = y - 2*fHeight
      | y < -fHeight = y + 2*fHeight
      | otherwise = y
    fWidth = fromIntegral width
    fHeight = fromIntegral height


degree2Radian :: Float -> Float
degree2Radian x = pi*x/180

drawModel :: Model -> Picture
drawModel ((x, y), _, _, ang, _, p) =
  (translate x y . rotate ang)  p

drawBullet :: Bullet -> Picture
drawBullet ((x, y), _, _, p) = translate x y p

drawAsteroid :: Asteroid -> Picture
drawAsteroid ((x, y), _, _, r) = translate x y (circle r)

drawWorld :: World -> Picture
drawWorld (m, as, bs) = color white $ pictures world
  where
    world = drawModel m : objects
    objects = asteroids ++ bullets
    asteroids = map drawAsteroid as
    bullets = map drawBullet bs

createAsteroid :: (Float, Float) -> Float -> Float -> Asteroid
createAsteroid (x, y) ang rad = ((x, y), vel, ang, rad)
  where
    vel = 14

fixPosition :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float)
fixPosition (mx, my) mSize (ax, ay)
  | dist (mx, my) (ax, ay) < (80 + mSize/2) = (ax+ix, ay+iy)
  | otherwise = (ax, ay)
    where
      (ix, iy) = rescaleVector (ax-mx, ay-my) 100 

createAsteroids :: StdGen -> Model -> [Asteroid]
createAsteroids g (mCoord, _, _, _, _, _) = take 3 $ zipWith3 createAsteroid coords angs rads
  where
    (pGen, angGen) = split g
    (xGen, yGen) = split pGen
    angs = map (360*) (randoms angGen)
    xs = map (\x -> x*800 - 400) (randoms xGen)
    ys = map (\x -> x*400 - 200) (randoms yGen)
    coords = map (fixPosition mCoord mSize) (zip xs ys)
    rads = repeat 80
    mSize = 40

initialWorld :: StdGen -> World 
initialWorld g = (m, as, [])
  where
    m = ( (0, 0), (0, 0), 0, 0, 0, triangle )
    as = createAsteroids g m

updatePosition :: Float -> Model -> Model
updatePosition dt ((x, y), vel@(vx, vy), acc, ang, angVel, p)=
  ( wrap (x + dt*vx, y + dt*vy), vel, acc, ang, angVel, p)

updateAngle :: Float -> Model -> Model
updateAngle dt (pos, vel, acc, ang, angVel, pic) = 
  (pos, vel, acc, ang + dt*angVel, angVel, pic)

vec2scalar :: (Float, Float) -> Float
vec2scalar (x, y) = sqrt $ x^2 + y^2

unitVector :: (Float, Float) -> (Float, Float)
unitVector (x, y) = (ux, uy)
  where
    (ux, uy) = (x/size, y/size)
    size = vec2scalar (x, y)

rescaleVector :: (Float, Float) -> Float -> (Float, Float)
rescaleVector vec size = (size*x, size*y)
  where
    (x, y) = unitVector vec

updateVelocity :: Float -> Model -> Model
updateVelocity dt (pos, vel@(vxi, vyi), acc, ang, angVel, pic) =
  (pos, vf, acc, ang, angVel, pic)
    where
      vf = (vxf, vyf)
      (vxf, vyf)
        | acc > 0 = (vxi + dt*acc*sin rAng, vyi + acc*dt*cos rAng)
        | scaVel < 0.1 = (0, 0)
        | otherwise = (vxi - dt*fx, vyi - dt*fy)
      rAng = degree2Radian ang
      scaVel = vec2scalar vel
      (fx, fy) = rescaleVector vel friction
      friction = 40

updateBulletPosition :: Float -> Bullet -> Bullet
updateBulletPosition dt ((x, y), vel, ang, p) = 
  ((x + dt*vel*sin rAng, y + dt*vel*cos rAng), vel, ang, p)
    where
      rAng = degree2Radian ang

updateAsteroidPosition :: Float -> Asteroid -> Asteroid
updateAsteroidPosition dt ((x, y), vel, ang, r) = 
  (wrap (x + dt*vel*sin rAng, y + dt*vel*cos rAng), vel, ang, r)
    where
      rAng = degree2Radian ang

shipAccControl :: Model -> Float -> Model
shipAccControl (pos, vel, accI, ang, angVel, pic) acc =
  (pos, vel, accF, ang, angVel, pic)
    where
      accF
        | acc > 0 = acc
        | accI > 0 = acc
        | otherwise = accI

shipAngVelControl :: Model -> Float -> Model
shipAngVelControl (pos, vel, acc, ang, _, pic) angVel =
  (pos, vel, acc, ang, angVel, pic)

bulletControl :: Model -> [Bullet] -> Float -> [Bullet]
bulletControl ((x, y), _, _, sAng, _, _) bs bVel
  | length bs >= 5 = bs
  | otherwise = (bPos, bVel, sAng, bPic) : bs
   where
     bPos = (x + 20 * sin (degree2Radian sAng), y + 20 * cos (degree2Radian sAng))
     bPic = rectangleSolid 3 3

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp)    Down _ _) (s, as, bs) = (shipAccControl s 40, as, bs)
inputHandler (EventKey (SpecialKey KeyUp)    Up   _ _) (s, as, bs) = (shipAccControl s 0, as, bs)
inputHandler (EventKey (SpecialKey KeyLeft)  Down _ _) (s, as, bs) = (shipAngVelControl s (-180), as, bs)
inputHandler (EventKey (SpecialKey KeyLeft)  Up   _ _) (s, as, bs) = (shipAngVelControl s 0, as, bs)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (s, as, bs) = (shipAngVelControl s 180, as, bs)
inputHandler (EventKey (SpecialKey KeyRight) Up   _ _) (s, as, bs) = (shipAngVelControl s 0, as, bs)
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) (s, as, bs) = (s, as, bulletControl s bs 500)
inputHandler _ w = w

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

isAsteroidSafe :: [Bullet] -> Asteroid -> Bool
isAsteroidSafe [] _ = True
isAsteroidSafe ((bPos,_,_,_):bs) a@(aPos,_,_,r)
  | dist bPos aPos < r = False
  | otherwise = isAsteroidSafe bs a

bulletOutsideAsteroid :: [Asteroid] -> Bullet -> Bool
bulletOutsideAsteroid [] _ = True
bulletOutsideAsteroid ((aPos,_,_,r):as) b@(bPos,_,_,_)
  | dist aPos bPos < r = False
  | otherwise = bulletOutsideAsteroid as b

removeBullets :: [Asteroid] -> [Bullet] -> [Bullet]
removeBullets as bs
  | null hits = bs
  | otherwise = filter (bulletOutsideAsteroid as) bs
    where
      hits = filter (isAsteroidSafe bs) as

detectHitAsteroids :: [Bullet] -> [Asteroid] -> [(Asteroid, Bool)]
detectHitAsteroids bs as = zip as dHit
  where
    dHit = map (isAsteroidSafe bs) as

addSmallerAsteroids :: [(Asteroid, Bool)] -> [(Asteroid, Bool)]
addSmallerAsteroids [] = []
addSmallerAsteroids ( t@(((x, y), vel, ang, rad),b):as )
  | b = t : addSmallerAsteroids as
  | not b && rad <= 20 = t : addSmallerAsteroids as
  | otherwise = t : newAsteroidLeft : newAsteroidRight : addSmallerAsteroids as
    where
      newAsteroidLeft = ( ( (x, y), vel, ang+90, rad/2 ), True)
      newAsteroidRight = ( ( (x, y), vel, ang-90, rad/2 ), True)

detectHitBullets :: [Asteroid] -> [Bullet] -> [(Bullet, Bool)]
detectHitBullets as bs = zip bs dHits
  where
    dHits = map (bulletOutsideAsteroid as) bs

removeObjects :: [(a, Bool)] -> [a]
removeObjects os = map fst (filter snd os)

detectHits :: ([Asteroid], [Bullet]) -> ([(Asteroid, Bool)], [(Bullet, Bool)])
detectHits (as, bs) = (as', bs')
  where
    bs' = detectHitBullets as bs
    as' = addSmallerAsteroids (detectHitAsteroids bs as)

detectShipHit :: [Asteroid] -> Model -> Bool
detectShipHit [] _ = False
detectShipHit ( ((ax, ay), _, _, rad) : as) m@( (mx, my), _, _, _, _, _)
  | dist (ax, ay) (mx, my) < rad = True
  | otherwise = detectShipHit as m

removeHitObjects :: ([Asteroid], [Bullet]) -> ([Asteroid], [Bullet])
removeHitObjects (as, bs) = (as'', bs'')
  where
    (as', bs') = detectHits (as, bs)
    as'' = removeObjects as'
    bs'' = removeObjects bs'

killBullets :: Bullet -> Bool
killBullets ((x, y), _, _, _) = 
  x > (-fWidth) && x < fWidth && y > (-fHeight) && y < fHeight
    where
      fHeight = fromIntegral height
      fWidth = fromIntegral width

updateWorld :: StdGen -> Float -> World -> World
updateWorld g dt (s, as, bs)
  | detectShipHit as s = initialWorld g
  | otherwise = (s', as', bs')
  where
    s' = (updatePosition dt . updateAngle dt . updateVelocity dt) s
    as'
      | null as = createAsteroids g s'
      | otherwise  = map (updateAsteroidPosition dt) rAs
    bs' = map (updateBulletPosition dt) $ filter killBullets rBs
    (rAs, rBs) = removeHitObjects (as, bs)

triangle :: Picture
triangle = lineLoop [(-15, -20), (0, 20), (15, -20)]

main :: IO ()
main = do
  g <- getStdGen
  
  let world = initialWorld g

  play
    window
    black
    60
    world
    drawWorld
    inputHandler
    (updateWorld g)
      where
        window = InWindow "Asteroids" (800, 600) (10, 10)
