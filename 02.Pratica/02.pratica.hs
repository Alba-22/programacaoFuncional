-- Exercicio 1
dobro :: Float -> Float
dobro x = 2 * x

quadruplicar :: Float -> Float
quadruplicar x = 2 * (dobro x)

hipotenusa :: Float -> Float -> Float
hipotenusa co ca = sqrt((co * co) + (ca * ca))

distancia :: Float -> Float -> Float -> Float -> Float
distancia ax ay bx by = hipotenusa ((bx) - (ax)) ((by) - (ay))

-- Exercicio 3
conversao :: Float -> (Float, Float, Float)
conversao real = (real, real * 3.96, real * 4.45)

-- Exercicio 4
bissexto :: Int -> Bool
bissexto ano
    | (mod ano 400 == 0) = True
    | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
    | otherwise = False

-- Exercicio 5
type Data = (Int, Int, Int)
bissexto2 :: Data -> Bool
bissexto2 (d, m, a) = bissexto a

-- Exercicio 6
valida :: Data -> Bool
valida (d, m, a)
  | (d >= 1 && d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
  | (d >= 1 && d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11) = True
  | (bissexto a == True) && (m == 2 && d >= 1 && d <= 29) = True
  | (bissexto a == False) && (m == 2 && d >= 1 && d <= 28) = True
  | otherwise = False

-- Exercicio 7
precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2)
  | not (valida (d1, m1, a1)) || not (valida (d2, m2, a2)) = False
  | a1 < a2 = True
  | a1 == a2 && m1 < m2 = True
  | a1 == a2 && m1 == m2 && d1 < d2 = True
  | otherwise = False

-- Exercicio 8
type Livro = (String, String, String, String, Data)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

-- Exercicio 9
verificaEmprestimo :: Emprestimo -> Data -> Bool
verificaEmprestimo (livro, aluno, (de, me, ae), (dd, md, ad), sit) (d, m, a)
  | not (valida (d, m, a)) = False
  | precede (d, m, a) (dd, md, ad) = True
  | otherwise = False

