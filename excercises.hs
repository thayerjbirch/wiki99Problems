--https://wiki.haskell.org/99_questions/1_to_10

--Problem 1
myTail :: [a] -> a
myTail [] = error "Can't find the tail of an empty list"
myTail [x] = x
myTail (_:xs) = myTail xs

--Problem 2
mySecondLast :: [a] -> a
mySecondLast [] = error "Can't find the second last of an empty list"
mySecondLast [x] = error "Not enough elements in the list"
mySecondLast [x,y] = x
mySecondLast (_:xs) = mySecondLast xs

--Problem 3
elementAt ::[a] -> Int -> a
elementAt (a:_) 1 = a
elementAt (_:xs) b = elementAt xs (b -1)
elementAt _ _ = error "Index out of bounds"

--Problem 4
myLength :: [a] -> Int
myLength xs = sum[ 1 | _ <- xs]

--Problem 5
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

--Problem 6
isPalindrone :: (Eq a) => [a] -> Bool
isPalindrone [] = True
isPalindrone a = a == myReverse a

--Problem 7
myFlatten :: [[a]] -> [a]
myFlatten [] = []
myFlatten (x:xs) = x ++ myFlatten xs

--Problem 8
myCompress :: (Eq a) => [a] -> [a]
myCompress (x:xs@(y:_))
    | x == y = myCompress (xs)
    | otherwise = x : myCompress (xs)
myCompress x = x

altCompress :: (Eq a) => [a] -> [a]
altCompress [] = []
altCompress (x:xs) = x : altCompress(dropWhile(==x) xs)

--Problem 9
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = ([x] ++ takeWhile (==x) xs) : myPack(dropWhile(==x)xs)

--Problem 10
myEncode :: (Eq a) => [a] -> [(Int,a)]
myEncode [] = []
myEncode a = [(myLength n, head n) | n <- myPack a]

--Problem 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)
modEncode :: (Eq a) => [a] -> [ListItem a]
modEncode x = map helper $ myEncode x
    where helper (1,n) = Single n
        helper (y,n) = Multiple y n
       
--Problem 12
myDecode :: [ListItem a] -> [a]
myDecode y = concatMap decoder y
    where
        decoder (Single x)    = [x]
        decoder (Multiple n x) = replicate n x
	   
--Problem 13
