-- Dor Hanegby
-- 204009633

-- 1.
-- a.

replaceElement :: (Int, a) -> [a] -> [a]
replaceElement (index, elem) list = map (\(i, x) -> if i == index then elem else x) (zip [0.. length list] list )

-- b.

replaceElements :: [(Int, a)] -> [a] -> [a]
replaceElements [] xs = xs
replaceElements pairs xs = replaceElements (tail pairs) (replaceElement (head pairs) xs)

-- 2.
-- a.
type Key = String
type Pair a = (Key, a)
type List a = [(Key, a)]

addItem :: Pair a -> List a -> List a
addItem pair list = pair : list

-- b.

subsetByKey :: Key -> List a -> [a]
subsetByKey key list = map (\(key, item) -> item) (filter (\(str, num) -> str == key) list)

-- c.

subsetByKeys :: [Key] -> List a -> [a]
subsetByKeys [] _ = []
subsetByKeys keys list = subsetByKey (head keys) list ++ subsetByKeys (tail keys) list

-- d.

getKeys :: List a -> [Key]
getKeys list = uniq (map (\(key, item) -> key) list)

-- Using Eq to make sure a are the same type

uniq :: Eq a => [a] -> [a]
uniq xs = uniq' xs []

uniq' :: Eq a => [a] -> [a] -> [a]
uniq' [] acc = acc
uniq' (x:xs) acc = if elem x acc then uniq' xs acc else uniq' xs (acc ++ [x])

-- e.

groupByKeys :: List a -> [(Key, [a])]
groupByKeys list = map (\x -> (x, subsetByKey x list)) (getKeys list)

-- 3. 

type Matrix a = [[a]]

-- a.
createMatrix :: Int -> Int -> [a] -> Matrix a
createMatrix _ _ [] = []
createMatrix rows columns list = [take columns list] ++ createMatrix rows columns (drop columns list)

-- b.

getElementInCell :: Int -> Int -> Matrix a -> a
getElementInCell row column matrix = (matrix!!row)!!column

-- c.

appendH :: Matrix a -> Matrix a -> Matrix a
appendH [] [] = []
appendH (x:xs) (y:ys) = [x ++ y] ++ appendH xs ys

-- d.
appendV :: Matrix a -> Matrix a -> Matrix a
appendV m1 m2 = m1 ++ m2

-- e.
addMatrices :: Matrix Int -> Matrix Int -> Matrix Int
addMatrices [] [] = []
addMatrices (x:xs) (y:ys) = (zipWith (+) x y ) : addMatrices xs ys







