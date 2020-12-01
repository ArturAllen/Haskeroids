module Main where

import Wrap
import Graphics.Gloss.Interface.Pure.Game
import System.Random

{-
  [1] In this section we define types and functions that will be useful throughout the code
-}

type Ship = ((Float, Float), (Float, Float), Float, Float, Float, [(Float, Float)])

type Bullet = ((Float, Float), (Float, Float), Float, Float, Picture)

type Asteroid = ((Float, Float), Float, Float, Float)

type World = (Ship, [Asteroid], [Bullet], StdGen, Bool, Int, Int)

width, height :: Int
width = 400
height = 300

fWidth, fHeight :: Float
fWidth = fromIntegral width
fHeight = fromIntegral height

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 _ [] _ _ _ = []
zipWith4 _ _ [] _ _ = []
zipWith4 _ _ _ [] _ = []
zipWith4 _ _ _ _ [] = []
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds

splashScreen :: Picture
splashScreen = color white $ pictures screen
  where
    screen = haskeroids ++ [txt]
    haskeroids = h ++ a ++ s1 ++ k ++ e ++ r ++ o ++ i ++ d ++ s2
    h = [line [(-237,29),(-237, -30)],line [(-237,0),(-200,0)],line [(-200,29),(-200,-30)]]
    a = [line [(-188,-30),(-188,11),(-169,29),(-150,11),(-150,-30)], line [(-188,-11), (-150,-11)]]
    s1= [line [(-100,29),(-139,29),(-139,0),(-100,0),(-100,-30),(-139,-30)]]
    k = [line [(-90,29),(-90,-30)],line [(-50,29),(-90,0)],line[(-50,-30),(-90,0)]]
    e = [line [(0,29),(-40,29),(-40,-30),(0,-30)],line[(-40,0),(-9,0)]]
    r = [line [(10,-30),(10,29),(49,29),(49,0),(10,0),(49,-30)]]
    o = [lineLoop [(59,-30),(59,29),(99,29),(99,-30)]]
    i = [line [(109,29),(149,29)],line [(129,29),(129,-30)],line[(109,-30),(149,-30)]]
    d = [lineLoop[(159,-30),(159,29),(178,29),(198,10),(198,-12),(178,-30)]]
    s2= [line [(248,29),(208,29),(208,0),(248,0),(248,-30),(208,-30)]]
    txt = (translate (-135) (-80) . scale 0.2 0.2) $ text "Press Space to start"

vec2scalar :: (Float, Float) -> Float
vec2scalar (x, y) = sqrt $ x**2 + y**2

unitVector :: (Float, Float) -> (Float, Float)
unitVector (x, y) = (ux, uy)
  where
    (ux, uy) = (x/size, y/size)
    size = vec2scalar (x, y)

rescaleVector :: (Float, Float) -> Float -> (Float, Float)
rescaleVector vec size = (size*x, size*y)
  where
    (x, y) = unitVector vec

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = sqrt $ (x1-x2)**2 + (y1-y2)**2

{-

  [2] In this section we define the functions used to draw our game on the screen

-}

drawShip :: Ship -> Picture
drawShip ((x, y), _, _, ang, _, ps) =
  wrapPolygon $ (wrapTranslate x y . wrapRotate ang) ps

drawBullet :: Bullet -> Picture
drawBullet ((x, y), _, _, _, p) = translate x' y' p
  where
    (x', y') = wrap (x, y)

drawAsteroid :: Asteroid -> Picture
drawAsteroid ((x, y), _, ang, r) = wrapPolygon $ (wrapTranslate x y . wrapScale r r . wrapRotate ang)  wrapAsteroid

drawWorld :: World -> Picture
drawWorld (_, _, _, _, True, _, _) = splashScreen
drawWorld (m, as, bs, _, _, lvl, lvs) = color white $ pictures world
  where
    world = lives : wave : drawShip m : objects
    objects = asteroids ++ bullets
    asteroids = map drawAsteroid as
    bullets = map drawBullet bs
    wave = (translate (-380) 240 . scale 0.2 0.2) $ text waveMsg
    waveMsg = "Wave: " ++ show (lvl-3)
    lives = (translate (-380) 200 . scale 0.2 0.2) $ text $ "Lives: " ++ show lvs

{-

  [3] In this section we define the start screen and the initial world conditions

-}

start :: StdGen -> World
start g = (m, [], [], g, True, 0, 0)
  where
    m = ((0, 0), (0, 0), 0, 0, 0, [])

createAsteroid :: (Float, Float) -> Float -> Float -> Float -> Asteroid
createAsteroid (x, y) vel ang rad = ((x, y), vel, ang, rad)

fixPosition :: (Float, Float) -> Float -> (Float, Float) -> (Float, Float)
fixPosition (mx, my) mSize (ax, ay)
  | dist (mx, my) (ax, ay) < (40 + mSize/2) = (ax+ix, ay+iy)
  | otherwise = (ax, ay)
    where
      (ix, iy) = rescaleVector (ax-mx, ay-my) 60

createAsteroids :: Int -> StdGen -> Ship -> ([Asteroid], StdGen)
createAsteroids n g (mCoord, _, _, _, _, _) = (take n $ zipWith4 createAsteroid coords vels angs rads, fst $ split g)
  where
    (pvGen, angGen) = split g
    (vGen, pGen) = split pvGen
    (xGen, yGen) = split pGen
    vels = map (\x -> x*80 + 10) (randoms vGen)
    angs = map (2*pi*) (randoms angGen)
    xs = map (\x -> x*800 - 400) (randoms xGen)
    ys = map (\x -> x*400 - 200) (randoms yGen)
    coords = map (fixPosition mCoord mSize) (zip xs ys)
    rads = repeat 40
    mSize = 40

initialWorld :: StdGen -> World
initialWorld g = (m, as, [], g', False, 4, 3)
  where
    m = ( (0, 0), (0, 0), 0, 0, 0, wrapTriangle )
    (as, g') = createAsteroids 3 g m

resetWave :: StdGen -> Int -> Int -> World
resetWave g lvl lvs = (m, as, [], g', False, lvl, lvs-1)
  where
    m = ( (0, 0), (0, 0), 0, 0, 0, wrapTriangle )
    (as, g') = createAsteroids (lvl-1) g m

{-

  [4] In this section we define how the game world reacts to the user input

-}

shipAccControl :: Ship -> Float -> Ship
shipAccControl (pos, vel, accI, ang, angVel, ps) acc =
  (pos, vel, accF, ang, angVel, ps)
    where
      accF
        | acc > 0 = acc
        | accI > 0 = acc
        | otherwise = accI

shipAngVelControl :: Ship -> Float -> Ship
shipAngVelControl (pos, vel, acc, ang, _, ps) angVel =
  (pos, vel, acc, ang, angVel, ps)

bulletControl :: Ship -> [Bullet] -> Float -> [Bullet]
bulletControl ((x, y), _, _, sAng, _, _) bs bVel
  | length bs >= 5 = bs
  | otherwise = (bPos, orgn, bVel, sAng, bPic) : bs
   where
     orgn = bPos
     bPos = (x + 20 * sin sAng, y + 20 * cos sAng)
     bPic = rectangleSolid 3 3

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) (s, as, bs, g, strt, lvl, lvs)
  | strt = initialWorld g
  | otherwise = (s, as, bulletControl s bs 500, g, strt, lvl, lvs)
inputHandler _ w@(_,_,_,_,True,_,_) = w
inputHandler (EventKey (SpecialKey KeyUp)    Down _ _) (s, as, bs, g, strt, lvl, lvs) = (shipAccControl s 80, as, bs, g,strt,lvl, lvs)
inputHandler (EventKey (SpecialKey KeyUp)    Up   _ _) (s, as, bs, g, strt, lvl, lvs) = (shipAccControl s 0, as, bs, g,strt, lvl, lvs)
inputHandler (EventKey (SpecialKey KeyLeft)  Down _ _) (s, as, bs, g, strt, lvl, lvs) = (shipAngVelControl s ((-2)*pi), as, bs, g, strt, lvl, lvs)
inputHandler (EventKey (SpecialKey KeyLeft)  Up   _ _) (s, as, bs, g, strt, lvl, lvs) = (shipAngVelControl s 0, as, bs, g,strt, lvl, lvs)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (s, as, bs, g, strt, lvl, lvs) = (shipAngVelControl s (2*pi), as, bs, g,strt, lvl, lvs)
inputHandler (EventKey (SpecialKey KeyRight) Up   _ _) (s, as, bs, g, strt, lvl, lvs) = (shipAngVelControl s 0, as, bs, g, strt, lvl, lvs)
inputHandler _ w = w

{-

  [5] In this section we define functions that update the game world

-}

{-
    [5.1] In this section we define functions that apply the transformations to the objects on the screen
-}

updatePosition :: Float -> Ship -> Ship
updatePosition dt ((x, y), vel@(vx, vy), acc, ang, angVel, ps)=
  ( wrap (x + dt*vx, y + dt*vy), vel, acc, ang, angVel, ps)

updateAngle :: Float -> Ship -> Ship
updateAngle dt (pos, vel, acc, ang, angVel, ps) =
  (pos, vel, acc, ang + dt*angVel, angVel, ps)

updateVelocity :: Float -> Ship -> Ship
updateVelocity dt (pos, vel@(vxi, vyi), acc, ang, angVel, ps) =
  (pos, vf, acc, ang, angVel, ps)
    where
      vf = (vxf, vyf)
      (vxf, vyf)
        | acc > 0 = (vxi + dt*acc*sin ang, vyi + acc*dt*cos ang)
        | scaVel < 0.1 = (0, 0)
        | otherwise = (vxi - dt*fx, vyi - dt*fy)
      scaVel = vec2scalar vel
      (fx, fy) = rescaleVector vel friction
      friction = 40

updateBulletPosition :: Float -> Bullet -> Bullet
updateBulletPosition dt ((x, y), orgn, vel, ang, p) =
  ((x + dt*vel*sin ang, y + dt*vel*cos ang), orgn, vel, ang, p)

updateAsteroidPosition :: Float -> Asteroid -> Asteroid
updateAsteroidPosition dt ((x, y), vel, ang, r) =
  (wrap (x + dt*vel*sin ang, y + dt*vel*cos ang), vel, ang, r)

updateWorld :: Float -> World -> World
updateWorld dt w@(s, as, bs, g, strt, lvl, lvs)
  | strt = w
  | detectShipHit as s && lvs == 0 = initialWorld (fst $ split g)
  | detectShipHit as s = resetWave (fst $ split g) lvl lvs 
  | otherwise = (s', as', bs', g', strt, lvl', lvs)
  where
    s' = (updatePosition dt . updateAngle dt . updateVelocity dt) s
    as'
      | null as = fst $ createAsteroids lvl g s'
      | otherwise  = map (updateAsteroidPosition dt) rAs
    g'
      | null as = snd $ createAsteroids lvl g s'
      | otherwise = fst $ split g
    bs' = map (updateBulletPosition dt) $ filter killBullets rBs
    (rAs, rBs) = removeHitObjects (snd $ split g) (as, bs)
    lvl'
      | null as = lvl+1
      | otherwise = lvl

{-

  [5.2] In this section we define functions that detect collisions between the objects

-}


isAsteroidSafe :: [Bullet] -> Asteroid -> Bool
isAsteroidSafe [] _ = True
isAsteroidSafe ((bPos,_,_,_,_):bs) a@(aPos@(ax, ay),_,_,r)
  | dist wbPos aPos < r = False
  | ax >  fWidth - r && dist wbPos (ax - 2*fWidth, ay) < r = False
  | ax <  r - fWidth && dist wbPos (ax + 2*fWidth, ay) < r = False
  | ay >  fHeight - r && dist wbPos (ax, ay - 2*fHeight) < r = False
  | ax >  r - fHeight && dist wbPos (ax, ay + 2*fHeight) < r = False
  | otherwise = isAsteroidSafe bs a
    where
      wbPos = wrap bPos

bulletOutsideAsteroid :: [Asteroid] -> Bullet -> Bool
bulletOutsideAsteroid [] _ = True
bulletOutsideAsteroid ((aPos@(ax, ay),_,_,r):as) b@(bPos,_,_,_,_)
  | dist aPos wbPos < r = False
  | ax >  fWidth - r && dist wbPos (ax - 2*fWidth, ay) < r = False
  | ax <  r - fWidth && dist wbPos (ax + 2*fWidth, ay) < r = False
  | ay >  fHeight - r && dist wbPos (ax, ay - 2*fHeight) < r = False
  | ax >  r - fHeight && dist wbPos (ax, ay + 2*fHeight) < r = False
  | otherwise = bulletOutsideAsteroid as b
  where
    wbPos = wrap bPos


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

addSmallerAsteroids :: StdGen -> [(Asteroid, Bool)] -> [(Asteroid, Bool)]
addSmallerAsteroids _ [] = []
addSmallerAsteroids g ( t@(((x, y), _, _, rad),b):as )
  | b = t : addSmallerAsteroids g as
  | not b && rad <= 10 = t : addSmallerAsteroids g as
  | otherwise = t : newAsteroidLeft : newAsteroidRight : addSmallerAsteroids g' as
    where
      (g', lGen) = split g
      [lVel, rVel, lAng, rAng] = take 4 $ randoms lGen
      newAsteroidLeft = ( ( (x, y), lVel*80+10, pi*lAng, rad/2 ), True)
      newAsteroidRight = ( ( (x, y), rVel*80+10, pi*rAng, rad/2 ), True)

detectHitBullets :: [Asteroid] -> [Bullet] -> [(Bullet, Bool)]
detectHitBullets as bs = zip bs dHits
  where
    dHits = map (bulletOutsideAsteroid as) bs

removeObjects :: [(a, Bool)] -> [a]
removeObjects os = map fst (filter snd os)

detectHits :: StdGen -> ([Asteroid], [Bullet]) -> ([(Asteroid, Bool)], [(Bullet, Bool)])
detectHits g (as, bs) = (as', bs')
  where
    bs' = detectHitBullets as bs
    as' = addSmallerAsteroids g (detectHitAsteroids bs as)

detectShipHit :: [Asteroid] -> Ship -> Bool
detectShipHit [] _ = False
detectShipHit ( (aPos@(ax, ay), _, _, r) : as) s@( sPos, _, _, _, _, _)
  | dist aPos sPos < r = True
  | ax >  fWidth - r && dist sPos (ax - 2*fWidth, ay) < r = True
  | ax <  r - fWidth && dist sPos (ax + 2*fWidth, ay) < r = True
  | ay >  fHeight - r && dist sPos (ax, ay - 2*fHeight) < r = True
  | ax >  r - fHeight && dist sPos (ax, ay + 2*fHeight) < r = True
  | otherwise = detectShipHit as s

removeHitObjects :: StdGen -> ([Asteroid], [Bullet]) -> ([Asteroid], [Bullet])
removeHitObjects g (as, bs) = (as'', bs'')
  where
    (as', bs') = detectHits g (as, bs)
    as'' = removeObjects as'
    bs'' = removeObjects bs'

killBullets :: Bullet -> Bool
killBullets (bPos, orgn, _, _, _) = dist bPos orgn < (3*fWidth/2)
  where
    fHeight = fromIntegral height
    fWidth = fromIntegral width

main :: IO ()
main = do
  g <- getStdGen

  let world = start g

  play
    window
    black
    60
    world
    drawWorld
    inputHandler
    updateWorld
      where
        window = InWindow "Haskeroids" (800, 600) (10, 10)

