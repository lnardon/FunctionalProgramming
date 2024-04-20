-- 1
bin2Dec :: [Int] -> Int
bin2Dec [] = 0
bin2Dec (x:xs) = x * (2 ^ (length xs)) + bin2Dec xs

-- 2
dec2Bin :: Int -> Int -> [Int]
dec2Bin dec bits
    | bits < length binRep = error "Número de bits insuficiente"
    | otherwise = replicate (bits - length binRep) 0 ++ binRep
    where
        binRep = decParaBinHelper dec

-- Aux Func
dec2BinHelper :: Int -> [Int]
dec2BinHelper 0 = [0]
dec2BinHelper 1 = [1]
dec2BinHelper n = dec2BinHelper (n `div` 2) ++ [n `mod` 2]


-- 3



-- 4



-- 5