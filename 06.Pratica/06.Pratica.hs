-- EXERCICIO 01

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos 
bdEmprestimo = [
  ("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"), 
  ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), 
  ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

-- A
bissexto :: Int -> Bool
bissexto ano = expression1 || (expression2 && expression3)
  where
    expression1 = mod ano 400 == 0
    expression2 = mod ano 4 == 0
    expression3 = mod ano 100 /= 0

valida :: Data -> Bool
valida (d, m, a) = expression1 || expression2 || expression3 || expression4
  where
    expression1 = (d >= 1 && d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
    expression2 = (d >= 1 && d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11)
    expression3 = (bissexto a == True) && (m == 2 && d >= 1 && d <= 29)
    expression4 = (bissexto a == False) && (m == 2 && d >= 1 && d <= 28)

-- B
bissextos :: [Int] -> [Int]
bissextos xs = expression
  where
    expression = [ x | x <- xs, bissexto x ]

-- C
precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2) = not expression1 || expression2 || expression3 || expression4
  where
    expression1 = not (valida (d1, m1, a1)) || not (valida (d2, m2, a2))
    expression2 = a1 < a2
    expression3 = a1 == a2 && m1 < m2
    expression4 = a1 == a2 && m1 == m2 && d1 < d2

verificaEmprestimo :: Emprestimo -> Data -> Bool
verificaEmprestimo (livro, aluno, (de, me, ae), (dd, md, ad), sit) (d, m, a) = not expression1 || expression2
  where
    expression1 = not (valida (d, m, a))
    expression2 = precede (d, m, a) (dd, md, ad)

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados xs (d, m, a) = expression
  where
    expression = [ x | x <- xs, verificaEmprestimo x (d, m, a) == False]

-- D
passo :: (Integer, Integer) -> (Integer, Integer)
passo (x, y) = expression
  where
    expression = (y, x + y)

fibo2 :: Integer -> (Integer, Integer)
fibo2 0 = (0, 1)
fibo2 n = expression
  where
    expression = passo (fibo2 (n - 1))

-- E
fatorialIntervalo :: Integer -> Integer
fatorialIntervalo x = prodIntervalo 1 x
  where
    prodIntervalo menor maior
      | menor >= maior = menor
      | otherwise = maior * (prodIntervalo menor (maior - 1))

-- EXERCICIO 02:
-- A
bissexto2 :: Int -> Bool
bissexto2 ano = 
  let expression1 = mod ano 400 == 0
      expression2 = mod ano 4 == 0
      expression3 = mod ano 100 /= 0
  in
  expression1 || (expression2 && expression3)

valida2 :: Data -> Bool
valida2 (d, m, a) = 
  let expression1 = (d >= 1 && d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
      expression2 = (d >= 1 && d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11)
      expression3 = (bissexto2 a == True) && (m == 2 && d >= 1 && d <= 29)
      expression4 = (bissexto2 a == False) && (m == 2 && d >= 1 && d <= 28)
  in
  expression1 || expression2 || expression3 || expression4

-- B
bissextos2 :: [Int] -> [Int]
bissextos2 xs =
  let expression = [ x | x <- xs, bissexto2 x ]
  in
  expression

-- C
precede2 :: Data -> Data -> Bool
precede2 (d1, m1, a1) (d2, m2, a2) = 
  let expression1 = not (valida2 (d1, m1, a1)) || not (valida2 (d2, m2, a2))
      expression2 = a1 < a2
      expression3 = a1 == a2 && m1 < m2
      expression4 = a1 == a2 && m1 == m2 && d1 < d2
  in
  not expression1 || expression2 || expression3 || expression4
    

verificaEmprestimo2 :: Emprestimo -> Data -> Bool
verificaEmprestimo2 (livro, aluno, (de, me, ae), (dd, md, ad), sit) (d, m, a) = 
  let expression1 = not (valida (d, m, a))
      expression2 = precede2 (d, m, a) (dd, md, ad)
  in
  not expression1 || expression2

atrasados2 :: Emprestimos -> Data -> Emprestimos
atrasados2 xs (d, m, a) = 
  let expression = [ x | x <- xs, verificaEmprestimo2 x (d, m, a) == False]
  in
  expression

-- D
passo2 :: (Integer, Integer) -> (Integer, Integer)
passo2 (x, y) = 
  let expression = (y, x + y)
  in
  expression

fibo22 :: Integer -> (Integer, Integer)
fibo22 0 = (0, 1)
fibo22 n = 
  let expression = passo2 (fibo22 (n - 1))
  in
  expression

-- E
fatorialIntervalo2 :: Integer -> Integer
fatorialIntervalo2 x = 
  let prodIntervalo menor maior
        | menor >= maior = menor
        | otherwise = maior * (prodIntervalo menor (maior - 1))
  in
  prodIntervalo 1 x


-- EXERICICIO 03
-- A
-- (\x. 2 * x + 1) 3
-- 2 * 3 - 1
-- 6 - 1
-- 5

-- B
-- (\xy. x - y) 5 7
-- 5 - 7
-- -2

-- C
-- (\yx. x - y) 5 7
-- 7 - 5
-- 2

-- D
-- (\xy. x - y) (\z. z / 2)
-- (\y. (z / 2) - y)

-- E
-- (\xy. x - y) ((\z. z / 2) 6) 1
-- (\xy. x - y) (6 / 2) 1
-- (\xy. x - y) 3 1
-- 3 - 1
-- 2

-- F
-- (\x. \y. - x y) 9 4
-- (\y. - 9 y) 4
-- (- 9 4)
-- 5

-- G
-- (\x.xx) (\y. y)
-- (\y.yy)

-- EXERCICIO 05
letraA = (\x -> \y -> y)((\z -> z)(\z -> z))(\w -> w) 5

letraB = ((\f -> (\x -> f(f x))) (\y -> y * y)) 3

letraC = ((\f -> (\x -> f(f x))) (\y -> (+) y y)) 5

letraD = ((\x -> (\y -> (+) x y) 5) ((\y -> (-) y 3) 7))

letraE = (((\f -> (\x -> f(f(f x)))) (\y -> (y * y))) 2)

letraF = (\x -> \y -> (+) x ((\x -> (-) x 3) y)) 5 6
