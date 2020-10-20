module Main where

import Graphics.Gloss

width, height :: Int
width = 100
height = 100

fps :: Float
fps = 25

toInt :: Float -> Int
toInt = round

wrap :: (Int, Int) -> (Int, Int)
wrap (x, y) = (x', y')
  where
    x'
      | x < -width = x + 2*width
      | x >  width = x - 2*width
      | otherwise = x 
    y'
      | y < -width = y + 2*height
      | y >  width = y - 2*height
      | otherwise = y

--Desenha um pixel na tela
ponto :: (Int, Int) -> Picture
ponto (xi, yi) = translate xf yf (rectangleSolid 1 1)
  where
    xf = fromIntegral x'
    yf = fromIntegral y'
    (x', y') = wrap (xi, yi)

eqReta :: Int -> Int -> Int -> Int -> Int -> Int
eqReta x1 x2 y1 y2 y = (x1-x2)*(y-y2)`div`(y1-y2) + x2

pontosNoEixo :: Int -> Int -> Int -> Int -> [Int]
pontosNoEixo x1 x2 y1 y2 = map eq [y1..y2]
    where
      eq = eqReta x1 x2 y1 y2

faixa :: Int -> Int -> [Int]
faixa a b 
  | a > b = [b..a]
  | otherwise = [a..b]

linhaHMaior :: Int -> Int -> Int -> Int -> [(Int, Int)]
linhaHMaior x1 x2 y1 y2 = zip ab (faixa y1 y2)
    where
      ab = if emOrdem then pontosNoEixo x1 x2 y1 y2 else pontosNoEixo x2 x1 y2 y1
      emOrdem = y2 >= y1

linhaWMaior :: Int -> Int -> Int -> Int -> [(Int, Int)]
linhaWMaior x1 x2 y1 y2 = zip (faixa x1 x2) ord
    where
      ord = if emOrdem then pontosNoEixo y1 y2 x1 x2 else pontosNoEixo y2 y1 x2 x1
      emOrdem = x2 >= x1

-- Desenha uma linha entre dois pontos
linha :: (Int, Int) -> (Int, Int) -> Picture
linha (x1, y1) (x2, y2) = pictures $ map ponto ps
  where
    h = abs (y2-y1)
    w = abs (x2-x1)
    ps = if h>w then linhaHMaior x1 x2 y1 y2 else linhaWMaior x1 x2 y1 y2

-- Cria linhas entre pontos consecutivos de uma lista
linhas :: [(Int, Int)] -> [Picture]
linhas [] = []
linhas [p] = []
linhas (p1:p2:ps) = linha p1 p2 : linhas (p2:ps)

-- desenha um poligono fechado passando pelos pontos de uma lista
poligono :: [(Int, Int)] -> Picture
poligono ps@(p:s) = pictures ciclo
  where
    primeiraLinha = linha p (last s)
    ciclo = primeiraLinha : linhas ps

-- desenha um circulo com centro e raio
circulo :: (Int, Int) -> Int -> [(Int, Int)]
circulo (x, y) r = zip ab ord
  where
    ab = map (\x' -> x + calcAb x') [0..31]
    ord = map (\y' -> y + calcOrd y') [0..31]
    calcAb  x' = toInt ( fromIntegral r * cos( fromIntegral x' *2*pi/ 32) )
    calcOrd y' = toInt ( fromIntegral r * sin( fromIntegral y' *2*pi/ 32) )

-- faz translacao de um ponto
translada :: Int -> Int -> (Int, Int) -> (Int, Int)
translada dsx dsy (xi, yi) = (xf, yf)
  where
    xf = xi + toInt ( fromIntegral dsx / fps)
    yf = yi + toInt ( fromIntegral dsy / fps)

mediaCom :: ((Int, Int) -> Int) -> [(Int, Int)] -> Int
mediaCom f ps = sum (map f ps) `div` length ps

pontoMedio :: [(Int, Int)] -> (Int, Int)
pontoMedio ps = (x, y)
  where
    x = mediaCom fst ps
    y = mediaCom snd ps

-- Faz rotacao de um ponto com centro na origem
rotaciona :: Int -> (Int, Int) -> (Int, Int)
rotaciona ang (x, y) = (x', y')
  where
    x' = toInt (fromIntegral x * cos ang' - fromIntegral y * sin ang' )
    y' = toInt (fromIntegral x * sin ang' + fromIntegral y * cos ang' )
    ang' = fromIntegral ang / fps

tempoDisc :: Float -> Int
tempoDisc dt = toInt (fps*dt)

tuplaCom :: (a -> b) -> (a, a) -> (b, b)
tuplaCom f (x, y) = (f x, f y)

animacao :: Float -> Picture
animacao dt = pictures figuras
  where
    figura1 = poligono modeloAtual
    figura2 = poligono ( circulo (x, y) 30 )
    figuras = [figura1, figura2]
    (x, y) = tuplaCom fromIntegral (pontoMedio modeloAtual)
    modeloAtual = map (translada dsx dsy . rotaciona ang) modelo
    dsy = 3 * dsx
    dsx = 10 * tempoDisc dt
    ang = tempoDisc dt
    modelo = [(-20,-13), (0, 27), (20, -13)]

main :: IO ()
main = animate (InWindow "Wrap test" (2*width, 2*height) (10, 10)) 
       white
       animacao
