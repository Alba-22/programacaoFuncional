-- TRABALHO DE PROGRAMAÇÃO FUNCIONAL
-- ALUNOS:
-- GUSTAVO VINÍCIUS ALBA - 11911BCC016
-- OTÁVIO ALMEIDA LEITE - 11911BCC010

l1=[1..2000] 
l2=[2000,1999..1] 
l3=l1++[0] 
l4=[0]++l2 
l5=l1++[0]++l2 
l6=l2++[0]++l1 
l7=l2++[0]++l2 
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] 
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1] 
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10] 
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11] 
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6] 
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16] 
x7=[20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]


-- EXERCICIO 01
bolha [] = ([],0)
bolha lista = bolhaOrd lista 0 (length lista)

bolhaOrd lista i 0 = (lista,i)
bolhaOrd lista i n = bolhaOrd lst j (n-1)
  where (lst,j) = trocas (lista,i)

trocas ([x],i) = ([x],i)
trocas ((x:y:zs),i)
  | x > y = ((y:lst1),j1)
  | otherwise = ((x:lst2),j2)
    where (lst1,j1) = trocas((x:zs),(i+1))
          (lst2,j2) = trocas((y:zs),(i))

-- Variação 1 Bubble Sort:
bolhaV1 [] = ([],0)
bolhaV1 lista = bolhaOrdV1 lista 0 (length lista)

bolhaOrdV1 lista i 0 = (lista,i)
bolhaOrdV1 lista i n
  | i == j = (lista, j)
  | otherwise = bolhaOrdV1 lst j (n-1)
  where (lst,j) = trocasV1 (lista,i)

trocasV1 ([x],i) = ([x],i)
trocasV1 ((x:y:zs),i)
  | x > y = ((y:lst1),j1)
  | otherwise = ((x:lst2),j2)
    where (lst1,j1) = trocasV1((x:zs),(i+1))
          (lst2,j2) = trocasV1((y:zs),(i))

-- Variação 2 Bubble Sort:
bolhaV2 [] = ([],0)
bolhaV2 lista = bolhaOrdV2 lista 0 (length lista)

bolhaOrdV2 lista i 0 = (lista,i)
bolhaOrdV2 lista i n
  | i == j = (lista, j)
  | otherwise = let tupla = bolhaOrdV2 (take (n-1) lst) j (n-1) in (fst tupla ++ [last lst], snd tupla)
  where (lst,j) = trocasV2 (lista,i)

trocasV2 ([x],i) = ([x],i)
trocasV2 ((x:y:zs),i)
  | x > y = ((y:lst1),j1)
  | otherwise = ((x:lst2),j2)
    where (lst1,j1) = trocasV2((x:zs),(i+1))
          (lst2,j2) = trocasV2((y:zs),(i))


-- EXERCICIO 2 Selection Sort:
selecao :: Ord a => [a] -> ([a], Int)
selecao xs = selecaoAux (xs, 0)

selecaoAux :: (Ord a) => ([a], Int) -> ([a], Int)
selecaoAux ([], c) = ([], c)
selecaoAux (xs, c) = 
  let tupla = selecaoAux ((remove x xs), c)
      x = minimo xs
  in ([x] ++ fst tupla, snd tupla + if x == head (xs) then 0 else 1)

remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
  | a == x = xs
  | otherwise = x : (remove a xs)

minimo :: (Ord a) => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs

-- Variação 1 Selection Sort:
selecaoV1 :: Ord a => [a] -> ([a], Int)
selecaoV1 xs = selecaoAuxV1 (xs, 0)

selecaoAuxV1 :: (Ord a) => ([a], Int) -> ([a], Int)
selecaoAuxV1 ([], c) = ([], c)
selecaoAuxV1 (xs, c) = 
  let (menor, restoLista) = removeMenorV1 xs
      tupla = selecaoAuxV1 (restoLista, c)
  in ([menor] ++ fst tupla, snd tupla + if menor == head (xs) then 0 else 1)

removeMenorV1 :: Ord a => [a] -> (a, [a])
removeMenorV1 [] = undefined
removeMenorV1 [x] = (x, [])
removeMenorV1 (x:xs)
  | x <= menorDoResto = (x, xs)
  | otherwise = (menorDoResto, x:resto)
  where (menorDoResto, resto) = removeMenorV1 xs

-- Variação 2 Selection Sort:
selecaoV2 :: Ord a => [a] -> ([a], Int)
selecaoV2 xs = selecaoAuxV2 (xs, 0)

selecaoAuxV2 :: (Ord a) => ([a], Int) -> ([a], Int)
selecaoAuxV2 ([], c) = ([], c)
selecaoAuxV2 (xs, c) = 
  let tupla = selecaoAuxV2 ((removeV2 x xs), c)
      x = foldr1 (min) xs
  in ([x] ++ fst tupla, snd tupla + if x == head (xs) then 0 else 1)

removeV2 :: (Ord a) => a -> [a] -> [a]
removeV2 a [] = []
removeV2 a (x:xs)
  | a == x = xs
  | otherwise = x : (removeV2 a xs)


-- EXERCICIO 03
insercao :: (Ord a) => [a] -> ([a], Int)
insercao [] = ([], 0)
insercao (x:xs) = (listaOrdenada, contador + contResto)
  where (listaOrdenada, contador) = insereOrd x (restoOrdenada)
        (restoOrdenada, contResto) = insercao xs

insereOrd :: (Ord a) => a -> [a] -> ([a], Int)
insereOrd x [] = ([x], 0)
insereOrd x (y:ys)
  | x <= y = ((x:y:ys), 1)
  | otherwise = (y:(resto), contResto + 1)
  where (resto, contResto) = insereOrd x ys


-- Variação 1 Insertion Sort:
insercaoV1 :: Ord a => [a] -> ([a], Int)
insercaoV1 xs = foldr (\elem -> \(restoOrdenada, contResto) -> 
    let (listaOrdenada, contador) = insereOrd elem (restoOrdenada) 
    in (listaOrdenada, contResto + contador)
  ) ([], 0) xs


-- EXERCICIO 04:
quicksort :: Ord a => [a] -> ([a], Int)
quicksort [] = ([], 0)
quicksort (p:xs) = (firstOrd ++ [p] ++ secondOrd, firstCont + secondCont + (2 * length xs))
  where 
    (firstOrd, firstCont) = quicksort [x | x <- xs, x < p]
    (secondOrd, secondCont) = quicksort [x | x <- xs, x >= p]

-- Variação 1 QuickSort:
quicksortV1 :: Ord a => [a] -> ([a], Int)
quicksortV1 [] = ([], 0)
quicksortV1 (p:xs) = (firstOrd ++ [p] ++ secondOrd, firstCont + secondCont + length xs)
  where 
    (f, s) = divideV1 p xs
    (firstOrd, firstCont) = quicksortV1 f
    (secondOrd, secondCont) = quicksortV1 s


divideV1 :: Ord a => a -> [a] -> ([a], [a])
divideV1 _ [] = ([], [])
divideV1 elem (x:xs)
  | elem > x = ([x] ++ first, second)
  | otherwise = (first, [x] ++ second)
    where 
      (first, second) = divideV1 elem xs

-- Variação 2 QuickSort:
quicksortV2 :: Ord a => [a] -> ([a], Int)
quicksortV2 [] = ([], 0)
quicksortV2 (p1:p2:p3:xs) = (firstOrd ++ [pivo] ++ secondOrd, firstCont + secondCont + (length (r1:r2:xs)) + contPivo)
  where 
    (pivo, contPivo, r1, r2) = procuraPivo p1 p2 p3
    (f, s) = divideV2 pivo (r1:r2:xs)
    (firstOrd, firstCont) = quicksortV2 f
    (secondOrd, secondCont) = quicksortV2 s
quicksortV2 (p:xs) = (firstOrd ++ [p] ++ secondOrd, firstCont + secondCont + length xs)
  where 
    (f, s) = divideV1 p xs
    (firstOrd, firstCont) = quicksortV2 f
    (secondOrd, secondCont) = quicksortV2 s

divideV2 :: Ord a => a -> [a] -> ([a], [a])
divideV2 _ [] = ([], [])
divideV2 elem (x:xs)
  | elem > x = ([x] ++ first, second)
  | otherwise = (first, [x] ++ second)
    where 
      (first, second) = divideV2 elem xs

procuraPivo :: Ord a => a -> a -> a -> (a, Int, a, a)
procuraPivo x y z =
  if x < y then
    if x < z then
      if y < z then (y, 3, x, z)
      else (z, 3, x, y)
    else (x, 2, y, z)
  else
    if y < z then
      if x < z then (x, 3, y, z)
      else (z, 3, x, y)
    else (y, 2, x, z)


-- EXERCICIO 05:
merge :: Ord a => [a] -> [a] -> Int -> ([a], Int)
merge xs [] c = (xs, c)
merge [] ys c = (ys, c)
merge (x:xs) (y:ys) c
  | (x <= y)  = (x:caso1Ord, c1 + 1)
  | otherwise = (y:caso2Ord, c2 + 1)
  where
    (caso1Ord, c1) = merge xs (y:ys) c
    (caso2Ord, c2) = merge (x:xs) ys c

mergesort :: Ord a => [a] -> ([a], Int)
mergesort [] = ([], 0)
mergesort [x] = ([x], 0)
mergesort xs = merge (first) (second) (c1 + c2)
  where 
    (first, c1) = mergesort (firstHalf)
    (second, c2) = mergesort (secondHalf)
    firstHalf = take (length xs `div` 2) xs
    secondHalf = drop (length xs `div` 2) xs


-- EXERCICIO 06:
data Exp = Val Double
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  deriving (Show)

avalia :: Exp -> Double
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2) 
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) / (avalia exp2)

-- ((3+12)*(15-5))/(1*3)
-- FINAL: avalia (Div (Mul (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Mul (Val 1) (Val 3)))

-- - ((6+8-5+1)*(2+6/2))
-- FINAL: avalia (Sub (Val 0) (Mul (Add (Sub (Add (Val 6) (Val 8)) (Val 5)) (Val 1)) (Add (Val 2) (Div (Val 6) (Val 2)))))


-- EXERCICIO 07:
data Jogada = Pedra | Papel | Tesoura deriving (Show)

vence :: Jogada -> Jogada -> Bool
vence Pedra Tesoura = True
vence Tesoura Papel = True
vence Papel Pedra = True
vence _ _ = False

vencedoras :: [(Jogada, Jogada)] -> [Jogada]
vencedoras [] = []
vencedoras ((a, b):xs)
  | vence a b = a:(vencedoras xs)
  | otherwise = b:(vencedoras xs)


-- EXERCICIO 08:
data Nebuloso = Verdadeiro | Falso | Talvez Float deriving (Show)

fuzzifica :: Float -> Nebuloso
fuzzifica x
  | x <= 0 = Falso
  | x >= 1 = Verdadeiro
  | otherwise = Talvez x

verifica_alto :: Float -> Nebuloso
verifica_alto altura = fuzzifica ((altura - 1.7) / 0.2)

verifica_barato :: Float -> Nebuloso
verifica_barato custo = fuzzifica ((50000 - custo) / 20000)


-- EXERCICIO 09:
data Estudante = 
  Colegial {
    ano :: Integer,
    nomeColegio :: String,
    matricula :: String,
    altura :: Float,
    peso :: Float
  }
  | Universitario {
    nomeUniversidade :: String,
    nomeCurso :: String,
    matricula :: String,
    altura :: Float,
    idade :: Integer
  }

baseEstudante :: [Estudante]
baseEstudante = [
  (Colegial 1 "Nacional" "AAAAA" 1.40 70),
  (Colegial 2 "Nacional" "AAAAB" 1.50 70),
  (Colegial 3 "Nacional" "AAAAC" 1.60 70),
  (Colegial 4 "Olimpo" "AAAAD" 1.75 70),
  (Colegial 5 "Olimpo" "AAAAE" 1.76 70),
  (Colegial 6 "Olimpo" "AAABA" 1.77 70),
  (Colegial 7 "Gabarito" "AAABB" 1.85 70),
  (Colegial 8 "Gabarito" "AAABC" 1.86 70),
  (Colegial 9 "Gabarito" "AAABD" 1.97 70),
  (Colegial 10 "Gabarito" "AAABE" 2.02 70),
  (Universitario "UFU" "Computação" "ZZZZZZZA" 1.50 20),
  (Universitario "UNITRI" "Medicina" "ZZZZZZZB" 1.60 22),
  (Universitario "UNA" "Direito" "ZZZZZZZC" 1.66 32),
  (Universitario "UNA" "Medicina" "ZZZZZZZD" 1.72 19),
  (Universitario "UFU" "Computação" "ZZZZZZZE" 1.80 17),
  (Universitario "UFU" "Computação" "ZZZZZZZF" 1.85 22),
  (Universitario "UFU" "Medicina" "ZZZZZZZG" 1.78 60),
  (Universitario "UNA" "Música" "ZZZZZZZH" 1.89 90),
  (Universitario "UNITRI" "Computação" "ZZZZZZZI" 1.95 45),
  (Universitario "UNITRI" "Direito" "ZZZZZZZJ" 1.92 14)
  ]

descobre_altos :: [Estudante] -> [(String, Nebuloso)]
descobre_altos [] = []
descobre_altos (Colegial _ _ matricula altura _:xs) = (matricula, verifica_alto altura):descobre_altos xs
descobre_altos (Universitario _ _ matricula altura _:xs) = (matricula, verifica_alto altura):descobre_altos xs


-- EXERCICIO 10:
data ArvBinInt = 
  Nulo 
  | No Int ArvBinInt ArvBinInt 
  deriving Show

folhas :: ArvBinInt -> [Int]
folhas Nulo = []
folhas (No x Nulo Nulo) = [x]
folhas (No x esq dir) = [] ++ (folhas esq) ++ (folhas dir)

somaNosInternos :: ArvBinInt -> Int
somaNosInternos Nulo = 0
somaNosInternos (No x Nulo Nulo) = 0
somaNosInternos (No x esq dir) = (somaNosInternos esq) + (somaNosInternos dir) + x

pertence :: Int -> ArvBinInt -> Bool
pertence elem Nulo = False
pertence elem (No x esq dir) = pertence elem esq || elem == x || pertence elem dir
