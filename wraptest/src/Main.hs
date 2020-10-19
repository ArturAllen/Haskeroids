module Main where

import Graphics.Gloss

width, height :: Int
width = 100
height = 100

toInt :: Float -> Int
toInt = round

--Desenha um pixel na tela; corrige a posicao do ponto se estiver fora da area de exibicao
ponto :: (Int, Int) -> Picture
ponto (x, y) = translate x' y' (rectangleSolid 1 1)
  where
    x'
     | x < (-width) = fromIntegral ( 2*width + x)
     | x > width = fromIntegral ( (-2)*width + x)
     | otherwise = fromIntegral x
    y'
     | y < (-height) = fromIntegral ( 2*height + y)
     | y > height = fromIntegral ( (-2)*height + y)
     | otherwise = fromIntegral y

linhaHMaior :: Int -> Int -> Int -> Int -> [(Int, Int)]
linhaHMaior x1 x2 y1 y2 = 
  if emOrdem then zip ab [y1..y2] else zip ab [y2..y1]
    where
      ab = if emOrdem then [ (x1-x2)*(y-y2)`div`(y1-y2) + x2  | y <- [y1..y2] ] else [ (x1-x2)*(y-y2)`div`(y1-y2) + x2  | y <- [y2..y1] ]
      emOrdem = y2 >= y1

linhaWMaior :: Int -> Int -> Int -> Int -> [(Int, Int)]
linhaWMaior x1 x2 y1 y2 = 
  if emOrdem then zip [x1..x2] ord else zip [x2..x1] ord
    where
      ord = if emOrdem then [ (y1-y2)*(x-x2)`div`(x1-x2) + y2  | x <- [x1..x2] ] else [ (y1-y2)*(x-x2)`div`(x1-x2) + y2  | x <- [x2..x1] ]
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
    ab = [x + calcAb x' | x' <- [0..31] ]
    ord = [y + calcOrd y' | y' <- [0..31] ]
    calcAb  x' = toInt ( fromIntegral r * cos( fromIntegral x' *2*pi/ 32) )
    calcOrd y' = toInt ( fromIntegral r * sin( fromIntegral y' *2*pi/ 32) )

-- faz translacao de um ponto
translada :: Int -> Int -> (Int, Int) -> (Int, Int)
translada dsx dsy (x, y) = (x+dsx, y+dsy)

primEl, secEl :: (Int, Int) -> Int
primEl (x, _) = x
secEl  (_, y) = y

pontoMedio :: [(Int, Int)] -> (Int, Int)
pontoMedio ps = (x, y)
  where
    x = sum (map primEl ps) `div` length ps
    y = sum (map secEl ps) `div` length ps

-- Faz rotacao com centro na origem de um ponto
rotaciona :: Int -> (Int, Int) -> (Int, Int)
rotaciona ang (x, y) = (x', y')
  where
    x' = toInt (fromIntegral x * cos (ang'/20) - fromIntegral y * sin (ang'/20) )
    y' = toInt (fromIntegral x * sin (ang'/20) + fromIntegral y * cos (ang'/20) )
    ang' = fromIntegral ang

tempoDisc :: Float -> Int
tempoDisc dt = toInt (20*dt)

animacao :: Float -> Picture
animacao dt = pictures figuras
  where
    figura1 = poligono modeloAtual
    figura2 = poligono ( circulo (x, y) 30 )
    figuras = [figura1, figura2]
    x = fromIntegral $ primEl $ pontoMedio modeloAtual
    y = fromIntegral $ secEl $ pontoMedio modeloAtual
    modeloAtual = map (translada dsx dsy . rotaciona ang) modelo
    dsy = 3 * dsx
    dsx = tempoDisc dt
    ang = dsx
    modelo = [(-20,-13), (0, 27), (20, -13)]

main :: IO ()
main = animate (InWindow "Wrap test" (2*width, 2*height) (10, 10)) 
       white
       animacao
