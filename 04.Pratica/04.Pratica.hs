-- Exercicio 02:
-- [5, 4 .. 1]
-- ['a', 'c' .. 'e']
-- [1, 4 .. 16]
-- zip [1, -2 .. -11] [1, 5 .. 17]

-- Exercicio 03.a:
contidosFechado :: Int -> Int -> [Int]
contidosFechado a b
  | a == b = [a]
  | a > b = []
  | otherwise = [a .. b]

-- Exercicio 03.b:
contidosAberto :: Int -> Int -> [Int]
contidosAberto a b
  | a == b || a > b = []
  | even a = [a + 2, a + 4 .. b - 1]
  | otherwise = [a + 1, a + 3 .. b - 1]


-- Exercicio 05:
quadrados :: Int -> Int -> [Int]
quadrados a b = [ x^2 | x <- [a .. b] ]

-- Exercicio 06:
seleciona_impares :: [Int] -> [Int]
seleciona_impares xs = [ i | i <- xs, odd i]

-- Exercicio 07:
tabuada :: Int -> [Int]
tabuada n = [ x * n | x <- [1 .. 10]]

-- Exercicio 08:

bissexto :: Int -> Bool
bissexto ano
    | (mod ano 400 == 0) = True
    | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
    | otherwise = False

bissextos :: [Int] -> [Int]
bissextos xs = [ x | x <- xs, bissexto x ]

-- Exercicio 09:
sublistas :: [[a]] -> [a]
sublistas list = [ x | x <- concat list ]

-- Exercicio 10:
type Data = (Int, Int, Int) 
type Emprestimo = (String, String, Data, Data, String) 
type Emprestimos = [Emprestimo] 

bdEmprestimo :: Emprestimos 
bdEmprestimo = [
  ("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"), 
  ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), 
  ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

valida :: Data -> Bool
valida (d, m, a)
  | (d >= 1 && d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
  | (d >= 1 && d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11) = True
  | (bissexto a == True) && (m == 2 && d >= 1 && d <= 29) = True
  | (bissexto a == False) && (m == 2 && d >= 1 && d <= 28) = True
  | otherwise = False

precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2)
  | not (valida (d1, m1, a1)) || not (valida (d2, m2, a2)) = False
  | a1 < a2 = True
  | a1 == a2 && m1 < m2 = True
  | a1 == a2 && m1 == m2 && d1 < d2 = True
  | otherwise = False

verificaEmprestimo :: Emprestimo -> Data -> Bool
verificaEmprestimo (livro, aluno, (de, me, ae), (dd, md, ad), sit) (d, m, a)
  | not (valida (d, m, a)) = False
  | precede (d, m, a) (dd, md, ad) = True
  | otherwise = False

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados xs (d, m, a) = [ x | x <- xs, verificaEmprestimo x (d, m, a) == False]

-- Exercicio 11:
pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
-- pertence a (x:xs) = if (a == x) then True else pertence a xs
pertence a (x:xs)
  | a == x = True
  | otherwise = pertence a xs

uniaoNRec :: Eq a => [a] -> [a] -> [a]
uniaoNRec as bs = as ++ [ b | b <- bs, not (pertence b as) ]