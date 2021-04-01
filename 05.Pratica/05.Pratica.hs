-- EXERCICIO 01:
conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x:xs) = 1 + conta_ch xs

conta :: [a] -> Int
conta [] = 0
conta (x:xs) = 1 + conta xs

maior :: [Int] -> Int
maior [x] = x
maior (x:y:resto)
  | x > y = maior (x: resto)
  | otherwise = maior (y: resto)

primeiros :: Int -> [a] -> [a]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x : primeiros (n - 1) xs

pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (x:xs) = if (a == x) then True else pertence a xs

uniaoR :: Eq a => [a] -> [a] -> [a]
uniaoR [] l = l
uniaoR (x:xs) l
  | pertence x l = uniaoR xs l
  | otherwise = x : uniaoR xs l

-- EXERCICIO 02:
npares :: [Int] -> Int
npares [] = 0
npares (x:xs)
  | mod x 2 == 0 = 1 + npares xs
  | otherwise = npares xs

-- EXERCICIO 03:
produtorio :: [Float] -> Float
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

-- EXERCICIO 04:
comprime :: [[a]] -> [a]
comprime [[]] = []
comprime ([]:y) = comprime y
comprime ((x:xs):y) = x:comprime(xs:y)

-- EXERCICIO 05:
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- EXERCICIO 06:

uniaoRec2 :: Eq a => [a] -> [a] -> [a]
uniaoRec2 as bs = as ++ [ b | b <- bs, not (pertence b as) ]