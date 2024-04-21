xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

intToBool :: Int -> Bool
intToBool 0 = False
intToBool 1 = True

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

xorOfBins :: [Int] -> [Int] -> [Int]
xorOfBins [] [] = []
xorOfBins (x:xs) (y:ys) = boolToInt (xor (intToBool x) (intToBool y)) : xorOfBins xs ys

arrayOfOne :: Int -> [Int]
arrayOfOne n = [1 | x <- [1..n]]

bin :: [Int] -> [Int]
bin xs = xorOfBins xs (arrayOfOne (length xs))

arrayLastOne :: Int -> [Int]
arrayLastOne n = [if x == n then 1 else 0 | x <- [1..n]]

addOfBins :: [Int] -> [Int] -> Int -> [Int]
addOfBins [] [] 0 = []
addOfBins [] [] 1 = []
addOfBins (x:xs) (y:ys) z | (x + y + z == 3) = z : addOfBins xs ys 1
                          | (x + y + z == 2) = 0 : addOfBins xs ys 1
                          | x == y = 0 + z : addOfBins xs ys 0
                          | otherwise = 1 : addOfBins xs ys 0

binCompl2Dec :: [Int] -> [Int]
binCompl2Dec xs = reverse (addOfBins (reverse (bin xs)) (reverse (arrayLastOne (length xs))) 0)

-- dec2bincompl :: [Int] -> int -> [Int]
---- preciso validar se o número é negativo ou não
-- dec2bincompl xs y = if (head xs) == 1 then {funcao negativo} else {funcao positivo}
