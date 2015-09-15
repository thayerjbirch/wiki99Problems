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
directEncode :: (Eq a) => [a] -> [(Int,a)]
directEncode [] = []
directEncode x = directEncodeHelper [] x

directEncodeHelper :: (Eq a) => [(Int,a)] -> [a] -> [(Int,a)]
directEncodeHelper result [] = result
directEncodeHelper result [x] = result++[(1,x)]
directEncodeHelper result (x:xs) = if x == (head xs)
                                   then directEncodeHelper (result++[((length $ takeWhile(x==)xs) + 1,x)]) (dropWhile(x==)xs)
                                   else directEncodeHelper (result++[(1,x)]) xs
                                   
--Problem 14
--Duplicate the elements in a list
dupli :: [a] -> [a]
dupli listIn = foldr(\x acc -> x:x:acc) [] listIn 

--Problem 15
--Triplicate the elements in a list
tripli :: [a] -> [a]
tripli listIn = foldr(\x acc -> x:x:x:acc) [] listIn

--Problem 16
--Drop ever Nth element in a list
dropNth :: Int -> [a] -> [a]
dropNth n targetList = dropNthHelper targetList 0 (n - 1)

dropNthHelper :: [a] -> Int -> Int -> [a]
dropNthHelper [x] iter n    = if(iter == n)
                              then []
                              else [x]
dropNthHelper (x:xs) iter n = if(iter == n)
                              then dropNthHelper xs 0 n
                              else x : dropNthHelper xs (iter + 1) n
dropNthHelper _ iter n      = []

--Problem 17
--Split a list into two parts, the length of the first of which is given
mySplit :: Int -> [a] -> ([a],[a])
mySplit n xs = (map fst lowerList, map fst upperList)
    where myList = xs `zip` [1..]
          lowerList = filter(\x -> (snd x) <= n) myList
          upperList = filter(\x -> (snd x) > n) myList
          
--Problem 18
--Extract a slice from a list
mySlice :: Int -> Int -> [a] -> [a]
mySlice i j xs = helper 0 i j xs []
    where helper count i j xs result
            | count < i = helper (count + 1) i j (tail xs) result
            | count >= i && count < j = helper (count + 1) i j (tail xs) (head xs : result)
            | otherwise = []