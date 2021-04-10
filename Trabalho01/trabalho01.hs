-- TRABALHO DE PROGRAMAÇÃO FUNCIONAL
-- ALUNOS:
-- GUSTAVO VINÍCIUS ALBA - 11911BCC016
-- OTÁVIO ALMEIDA LEITE - 11911BCC010

-- EXERCICIO 01:
analisa_raizes :: Int -> Int -> Int -> String
analisa_raizes a b c
  | a == 0 = "4-equacao degenerada"
  | b ^ 2 > 4 * a * c = "1-possui duas raizes reais"
  | b ^ 2 == 4 * a * c = "2-possui uma raiz real"
  | b ^ 2 < 4 * a * c = "3-nenhuma raiz real"

-- EXERCICIO 02:
baskara_delta :: (Float, Float, Float) -> Float
baskara_delta (a, b, c) = sqrt (b ^ 2 - 4 * a * c)

equacao :: Float -> Float -> Float -> (Float, Float)
equacao a b c
  | a == 0 = ((-c) / b, a)
  | otherwise = ((- b + baskara_delta (a, b, c)) / (2 * a), (- b - baskara_delta (a, b, c)) / (2 * a))

-- EXERCICIO 03:
type Data = (Int, Int, Int)

bissexto :: Int -> Bool
bissexto ano
  | (mod ano 400 == 0) = True
  | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
  | otherwise = False

valida :: Data -> Bool
valida (d, m, a)
  | (d >= 1 && d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
  | (d >= 1 && d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11) = True
  | (bissexto a == True) && (m == 2 && d >= 1 && d <= 29) = True
  | (bissexto a == False) && (m == 2 && d >= 1 && d <= 28) = True
  | otherwise = False

idade :: Data -> Data -> Int
idade (d, m, a) (dn, mn, an)
  | not (valida (d, m, a)) || not (valida (dn, mn, an)) = -1
  | a >= an && m /= mn && m >= mn = a - an
  | a >= an && m == mn && d >= dn = a - an
  | otherwise = a - an - 1

passagem :: Float -> Data -> Data -> Float
passagem valor (da, ma, aa) (dn, mn, an)
  | idade (da, ma, aa) (dn, mn, an) == -1 = valor
  | idade (da, ma, aa) (dn, mn, an) < 2 = valor * 0.15
  | idade (da, ma, aa) (dn, mn, an) < 10 && idade (da, ma, aa) (dn, mn, an) >=2 = valor * 0.4
  | idade (da, ma, aa) (dn, mn, an) >= 70 = valor * 0.5
  | otherwise = valor

-- EXERCICIO 04:
list = [1..20]

gera1 :: [Int]
gera1 = [x ^ 3 | x <- list, even(x), x > 3, x < 11]

gera2 :: [(Int, Int)]
gera2 = [(x, y) | x <- list, y <- list, x <= 5, y > 5, y < x * 3 ]

l1 = [15, 16]
gera3_aux :: [Int] -> [Int]
gera3_aux [] = []
gera3_aux (x:xs) = [y | y <- [1..x]] ++ gera3_aux xs

gera3 :: [Int]
gera3 = gera3_aux l1

gera4 :: [(Int, Int)]
gera4 = [(x, x + 1) | x <- [1..10], even(x)]

gera5 :: [Int]
gera5 = [x + y | (x, y) <- gera4]

-- EXERCICIO 5:
-- A
contaNegM2 :: [Integer] -> Int
contaNegM2 x = length [ y | y <- x, y >= 0, mod y 3 == 0]

-- B
listaNegM2 :: [Integer] -> [Integer]
listaNegM2 x = [ y | y <- x, y >= 0, mod y 3 == 0]

-- EXERCICIO 6:
fatores :: Integer -> [Integer]
fatores x = [ y | y <- [1..x], mod x y == 0]

primos :: Integer -> Integer -> [Integer]
primos x y = [ n | n <- [x..y], fatores n == [1, n]]

-- EXERCICIO 7:
mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc y (mod x y)

-- MMC de dois numeros
mmc2 :: Integer -> Integer -> Integer
mmc2 x y = div (x*y) (mdc x y) 

-- MMC de tres numeros
mmc :: Integer -> Integer -> Integer -> Integer
mmc x y z = mmc2 (mmc2 x y) (z)

-- EXERCICIO 8:
serie :: Double -> Integer -> Double
serie x 0 = 0
serie x n
  | mod n 2 == 0 = x/(fromIntegral n) + serie x (n-1) -- posicao par
  | otherwise = (fromIntegral n)/x + serie x (n-1) -- posicao impar

-- EXERCICIO 9:
fizzbuzz :: Integer -> [String]
fizzbuzz 1 = ["No"]
fizzbuzz n
  | mod n 2 == 0 && mod n 3 == 0 = fizzbuzz (n-1) ++ ["FizzBuzz"]
  | mod n 2 == 0 = fizzbuzz (n-1) ++ ["Fizz"]
  | mod n 3 == 0 = fizzbuzz (n-1) ++ ["Buzz"]
  | otherwise = fizzbuzz (n-1) ++ ["No"]

-- EXERCICIO 10:
seleciona_multiplos :: Integer -> [Integer] -> [Integer]
seleciona_multiplos n lst = [e | e <-lst, mod e n == 0]

-- EXERCICIO 11:
quantidade_ocorrencia :: Eq t => t -> [t] -> Int
quantidade_ocorrencia a [] = 0
quantidade_ocorrencia a (x:xs)
  | (a == x) = 1 + quantidade_ocorrencia a xs
  | otherwise = 0 + quantidade_ocorrencia a xs

unica_ocorrencia :: Eq t => t -> [t] -> Bool
unica_ocorrencia a x
 | (quantidade_ocorrencia a x == 1) = True
 | otherwise = False

-- EXERCICIO 12:
intercala_aux :: [a] -> [a] -> Int -> Int -> [a]
intercala_aux [] b _ _ = [] ++ b
intercala_aux a [] _ _ = [] ++ a
intercala_aux (a:as) (b:bs) 0 1 = [b] ++ intercala_aux (a:as) bs 1 0
intercala_aux (a:as) (b:bs) 1 0 = [a] ++ intercala_aux as (b:bs) 0 1

intercala :: [a] -> [a] -> [a]
intercala a b = intercala_aux a b 1 0

-- EXERCICIO 13:
zipar:: [a] -> [a] -> [[a]]
zipar _ [] = []
zipar [] _ = []
zipar (x:xl) (y:yl) = ([x, y]:(zipar xl yl))

-- EXERCICIO 14:
type ItemAgenda = (String, String, String, String)
agenda :: [ItemAgenda]
agenda = [("Gustavo", "Av do Cafe", "16922224444", "alba@test.io"),
  ("Otavio", "Av Rondon", "34944442222", "octo@test.io")]

recupera_aux :: String -> [ItemAgenda] -> String
recupera_aux _ [] = "Email desconhecido"
recupera_aux input ((nome, end, tel, email):xs) 
  | input == email = nome
  | otherwise = recupera_aux input xs

recupera_contato :: String -> String
recupera_contato input = recupera_aux input agenda

-- EXERCICIO 15:
type Pessoa = (String, Float, Int, Char) 
pessoas :: [Pessoa]
pessoas = [("Rosa", 1.66, 27,'C'), 
  ("João", 1.85, 26, 'C'), 
  ("Maria", 1.55, 62, 'S'), 
  ("Jose", 1.78, 42, 'C'), 
  ("Paulo", 1.93, 25, 'S'), 
  ("Clara", 1.70, 33, 'C'), 
  ("Bob", 1.45, 21, 'C'), 
  ("Rosana", 1.58,39, 'S'), 
  ("Daniel", 1.74, 72, 'S'), 
  ("Jocileide", 1.69, 18, 'S')]

altura_media_aux :: [Pessoa] -> Float -> Float -> Float
altura_media_aux ((_, alturaPessoa, _, _):xs) soma tamanho = altura_media_aux xs (soma + alturaPessoa) (tamanho + 1)
altura_media_aux [] soma tamanho = soma / tamanho

altura_media :: [Pessoa] -> Float
altura_media x = altura_media_aux x 0 0

idade_menor :: [Pessoa] -> Int
idade_menor [] = 0
idade_menor [(_,_,x,_)] = x
idade_menor ((x1,x2,x,x3):(y1,y2,y,y3):ys)
  | x < y = idade_menor ((x1,x2,x,x3):ys)
  | otherwise = idade_menor ((y1,y2,y,y3):ys)

mais_velho :: [Pessoa] -> (String, Char)
mais_velho [(nome, _, _, civil)] = (nome, civil)
mais_velho ((nome1, altura1, idade1, civil1):(nome2, altura2, idade2, civil2):ys)
  | idade1 > idade2 = mais_velho ((nome1, altura1, idade1, civil1):ys)
  | otherwise = mais_velho ((nome2, altura2, idade2, civil2):ys)

maior_cinq :: [Pessoa] -> [Pessoa]
maior_cinq p = [(nome, altura, idade, civil) | (nome, altura, idade, civil) <- p, idade >= 50]

casadas_aux :: [Pessoa] -> Int -> [Pessoa]
casadas_aux p i = [(nome, altura, idade, civil) | (nome, altura, idade, civil) <- p, idade >= i, civil == 'C']

casadas :: [Pessoa] -> Int -> Int
casadas p i = length (casadas_aux p i)

-- EXERCICIO 16:
insere_ord:: Ord a => a -> [a] -> [a]
insere_ord x [] = [x]
insere_ord x [y]
  | x > y = [y, x]
  | otherwise = [x, y]
insere_ord x (y:l)
  | x > y = (y:(insere_ord x l))
  | otherwise = (x:y:l)

-- EXERCICIO 17:
reverte :: [a] -> [a]
reverte [x] = [x]
reverte (h:t) = reverte t ++ [h]

-- EXERCICIO 18:
elimina_repet :: Eq a => [a] -> [a]
elimina_repet [x] = [x]
elimina_repet (x:t) = [x] ++ [ e | e <- (elimina_repet t), e /= x]

-- EXERCICIO 19:
disponiveis = [1, 2, 5, 10, 20, 50, 100]

notasTroco :: Integer -> [[Integer]]
notasTroco 0 = [[]]
notasTroco x = [ [e] ++ l | e <- disponiveis, e <= x,  l <- notasTroco (x-e)]

-- EXERCICIO 20:
-- Confere se duas rainhas se atacam
ataca :: (Integer, Integer) -> (Integer, Integer) -> Bool
ataca (xl, xc) (yl, yc) =
  (xl == yl) || 
  (xc == yc) ||
  (xl-xc == yl-yc) ||
  (xl+xc == yl+yc)

headNaoAtaca :: [Integer] -> Integer -> Bool
headNaoAtaca [h1] _ = True
headNaoAtaca (h1:h2:t) index = 
  (not (ataca(h1, 1) (h2, index+1))) && 
  headNaoAtaca (h1:t) (index+1)

naoAtacam :: [Integer] -> Bool
naoAtacam [x] = True
naoAtacam (h:t) = headNaoAtaca (h:t) 1 && naoAtacam t


permutacaoAux :: Integer -> Integer -> [[Integer]]
permutacaoAux 0 _ = [[]]
permutacaoAux x xr = [ [e] ++ l | e <- [1..xr], l <- permutacaoAux (x-1) xr]

permutacao :: Integer -> [[Integer]]
permutacao x = permutacaoAux x x

nRainhas :: Integer -> [[Integer]]
nRainhas n = [ mapa | mapa <- permutacao n, naoAtacam mapa]

