-- EXERCICIO 01
paridade :: [Int] -> [Bool]
paridade x = map (even) x

-- EXERCICIO 02
prefixos :: [String] -> [String]
prefixos x = map (take 3) x

-- EXERCICIO 03
saudacao :: [String] -> [String]
saudacao x = map ("Oi " ++ ) x

-- EXERCICIO 04
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f xs = [x | x <- xs, f x]

-- EXERCICIO 05
pares :: [Int] -> [Int]
pares x = filter (even) x

-- EXERCICIO 06
solucoes :: [Int] -> [Int]
solucoes x = filter (\x -> (\m -> \n -> (<) m n) ((\z -> 5 * z + 6) x) ((\y -> y * y) x)) x

-- EXERCICIO 07
maior :: [Int] -> Int
maior x = foldr1 (max) x

-- EXERCICIO 08
menor_min10 :: [Int] -> Int
menor_min10 x = foldr (min) 10 x

-- EXERCICIO 09
junta_silabasplural :: [String] -> String
junta_silabasplural x = foldr (++) "s" x

-- EXERCICIO 10
menores10 :: [Int] -> ([Int], Int)
menores10 xs = 
  let lista = [x | x <- xs, x < 10]
  in (lista, length lista)

-- EXERCICIO 11

busca_aux :: Int -> [Int] -> Int -> (Bool, Int)
busca_aux _ [] c = (False, c - 1)
busca_aux n (x:xs) c
  | n /= x = busca_aux n xs (c + 1)
  | otherwise = (True, c)

busca :: Int -> [Int] -> (Bool, Int)
busca n (x:xs) = busca_aux n (x:xs) 1
