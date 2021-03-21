-- Exercicio 1:
oor1 :: Bool -> Bool -> Bool
oor1 True True = True
oor1 True False = True
oor1 False True = True
oor1 False False = False

oor2 :: Bool -> Bool -> Bool
oor2 True _ = True
oor2 _ True = True
oor2 False False = False

oor3 :: Bool -> Bool -> Bool
oor3 False a = a
oor3 True _ = True

oor4 :: Bool -> Bool -> Bool
oor4 a b
  | (a == True) = True
  | (b == True) = True
  | otherwise = False

oor5 :: Bool -> Bool -> Bool
oor5 a b =
  if (a == True) then True
  else 
    if (b == True) then True
    else False

-- Exercicio 2:
type Point = (Float, Float, Float)
distance :: Point -> Point -> Float
distance (a, b, c) (x, y, z) = sqrt((a - x)^2 + (b - y)^2 + (c - z)^2)

-- Exercicio 3:
fatorial1 :: Integer -> Integer
fatorial1 x
  | (x == 0) = 1
  | otherwise = x * fatorial1(x - 1)

fatorial2 :: Integer -> Integer
fatorial2 x =
  if (x == 0) then 1
  else x * fatorial2(x - 1)

-- Exercicio 4:
fibo :: Integer -> Integer
fibo n
  | (n == 0) = 0
  | (n == 1) = 1
  | otherwise = fibo(n - 2) + fibo(n - 1)

-- Exercicio 5:
n_tri :: Integer -> Integer
n_tri n
  | (n == 0) = 0
  | otherwise = n_tri(n - 1) + n

-- Exercicio 6:
potencia2 :: Integer -> Integer
potencia2 n
  | (n == 0) = 1
  | otherwise = 2 * potencia2(n - 1)

-- Exercicio 7:
prodIntervalo :: Integer -> Integer -> Integer
prodIntervalo menor maior
  | menor >= maior = menor
  | otherwise = maior * (prodIntervalo menor (maior - 1))
  

fatorialIntervalo :: Integer -> Integer
fatorialIntervalo x = prodIntervalo 1 x

-- Exercicio 8:
restoDiv :: Integer -> Integer -> Integer
restoDiv dividendo divisor
  | dividendo - divisor < divisor = dividendo - divisor
  | otherwise = restoDiv (dividendo - divisor) divisor

divInteira :: Integer -> Integer -> Integer
divInteira dividendo divisor
  | dividendo - divisor < divisor = 1
  | otherwise = 1 + divInteira (dividendo - divisor) divisor

-- Exercicio 9:
mdcGuards :: Integer -> Integer -> Integer
mdcGuards m n
  | n == 0 = m
  | otherwise = mdcGuards n (m `mod` n)

mdcPattern :: Integer -> Integer -> Integer
mdcPattern m 0 = m
mdcPattern m n = mdcPattern n (m `mod` n)

-- Exercicio 10:
binomialGuards :: Integer -> Integer -> Integer
binomialGuards n k
  | k == 0 = 1
  | k == n = 1
  | True = binomialGuards (n - 1) k + binomialGuards (n - 1) (k - 1)

binomialPattern :: Integer -> Integer -> Integer
binomialPattern n 0 = 1
binomialPattern n k = 
  if (k == n) then 1 
  else binomialPattern (n-1) k + binomialPattern (n-1) (k-1)

-- Exercicio 11:
passo :: (Integer, Integer) -> (Integer, Integer)
passo (x, y) = (y, x + y)

fibo2 :: Integer -> (Integer, Integer)
fibo2 n
  | (n == 0) = (0, 1)
  | otherwise = passo (fibo2 (n - 1))