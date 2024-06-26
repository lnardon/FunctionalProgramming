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
        binRep = dec2BinHelper dec

-- 3
bincompl2dec :: [Int] -> Int
bincompl2dec [] = -1
bincompl2dec xs = if (head xs) == 0 then bin2Dec xs else -bin2Dec (bincompl2Negative xs)

-- 4
dec2bincompl :: Int -> Int -> [Int]
dec2bincompl dec bits = if dec < 0 then bincompl2Negative (dec2Bin (abs dec) bits) else dec2Bin (abs dec) bits 

-- 5
-- TODO: Review
somarbin :: [Int] -> [Int] -> Int -> [Int]
somarbin xs ys z = if and [((length xs) == z), ((length ys) == z)] then addOfBins (reverse xs) (reverse ys) 0 else error "Número de bits insuficiente"


-- Aux Func
dec2BinHelper :: Int -> [Int]
dec2BinHelper 0 = [0]
dec2BinHelper 1 = [1]
dec2BinHelper n = dec2BinHelper (n `div` 2) ++ [n `mod` 2]

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

invertbin :: [Int] -> [Int]
invertbin xs = xorOfBins xs (arrayOfOne (length xs))

oneInBin :: Int -> [Int]
oneInBin n = [if x == n then 1 else 0 | x <- [1..n]]

addOfBins :: [Int] -> [Int] -> Int -> [Int]
addOfBins [] [] 0 = []
addOfBins [] [] 1 = []
addOfBins (x:xs) (y:ys) z | (x + y + z == 3) = z : addOfBins xs ys 1
                          | (x + y + z == 2) = 0 : addOfBins xs ys 1
                          | x == y = 0 + z : addOfBins xs ys 0
                          | otherwise = 1 : addOfBins xs ys 0

bincompl2Negative :: [Int] -> [Int]
bincompl2Negative xs = reverse (addOfBins (reverse (invertbin xs)) (reverse (oneInBin (length xs))) 0)
