-- These are (hopefully) 99 haskell problems


-- 1.

myLast :: [a] -> a
myLast [] = error "LAST ELEMENT OF AN EMPTY LIST! WHAT!?"
myLast [a] = a
myLast (_:xs) = myLast xs
-- 2.
myLastButOne :: [a] -> a
myLastButOne [] = error "YOU CRAZY!"
myLastButOne [x] = error "YOU EVEN CRAZIER!"
myLastButOne [x,_] = x
myLastButOne (_:xs) = myLastButOne xs
-- 3.
elementAt :: (Integral n) => n -> [a] -> a
elementAt _ [] = error "WHAT!? INDEX OUT OF RANGE!"
elementAt 1 xs = head xs
elementAt n (x:xs) = elementAt (n-1) xs
-- 4.
myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
-- 5.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
-- 6.
myInit :: [a] -> [a]
myInit [] = error "WHAT?! YOU CRAZY! EMPTY HAS NO INIT!"
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == myLast xs) && isPalindrome (myInit xs)
-- 7.
data NestedList a = Elem a | List [NestedList a]
flatten :: (NestedList a) -> [a]
flatten (Elem a) = [a]
flatten (List x) = concatMap flatten x  -- concat map applies function to elements of list and concats the results
-- here are some alternatives
--flatten (List []) = []
--flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
-- and another alternative
--flatten ( List xs ) = case xs of [] -> []
--                                 x:ys -> (flatten x) ++ (flatten (List ys))

-- this just illustrates how to declare types in a function
--sillyLen :: (Floating a, Integral b) => [a] -> b
--sillyLen [] = 0
--sillyLen (x:xs) = 1 + sillyLen xs

-- 8.
deDupe :: (Eq a) => [a] -> [a]
deDupe [] = []
deDupe [x] = [x]
deDupe (x:y:xs) = if (x == y) then deDupe (y:xs) else x:(deDupe (y:xs))
-- 9.
groupDupe :: (Eq a) => [a] -> [[a]]
groupDupe [] = [[]]
groupDupe [x] = [[x]]
groupDupe (x:y:xs) = if (x == y) then (x:(head rest)):(tail rest) else [x]:(groupDupe (y:xs)) where rest = groupDupe (y:xs)
-- 10.
countDupe :: (Eq a) => [a] -> [(Int, a)]
countDupe [] = error "NOTHING TO COUNT! YOU CRAZY!"
countDupe xs = concatMap f (groupDupe xs)
    where   f :: [a] -> [(Int, a)]
            f [] = error "CAN'T COUNT EMPTY!"
            f xs = [(length xs, head xs)]
--countDupe xs = f (groupDupe xs)
--    where f :: [[b]] -> [(Int, b)] 
--          f [] = []
--          f (x:xs) = (length x, head x):(f xs)

-- 11.
data EncoLen a = Single a | Multiple Int a deriving Show
encodeDupe :: (Eq a) => [a] -> [EncoLen a]
encodeDupe [] = error "CAN'T ENCODE NOTHING! YOU CRAZY!"
-- here is list comprehension solution
encodeDupe xs = [ y | x <- (groupDupe xs), let y = if (length x == 1) then Single (head x) else Multiple (length x) (head x) ]
-- here is an alternative
--encodeDupe [] = error "NOTHING TO COUNT! YOU CRAZY!"
--encodeDupe xs = map f (groupDupe xs)
--   where   f :: [a] -> EncoLen a
--            f [] = error "CAN'T COUNT EMPTY!"
--            f xs = if (length xs == 1) then Single (head xs) else Multiple (length xs) (head xs)

-- 12.
dencodeDupe :: [EncoLen a] -> [a]
dencodeDupe [] = []
dencodeDupe (x:xs) = (helper x) ++ dencodeDupe xs
    where   helper :: EncoLen a -> [a]
            helper (Single x) = [x]
            helper (Multiple n x) = take n (repeat x)

-- 31.
--
-- This function doesn't work for negative numbers
isPrime :: Int -> Bool
isPrime x 
    | x <= 1 = False
    | otherwise = not (0 `elem` (map (mod x) [2 .. x-1]))

-- 32.
myGCD :: Int -> Int -> Int
myGCD x y
    | y < 0 = error "myGCD not implemented for negative numbers"
    | x < y = myGCD y x
myGCD x 0 = x
myGCD x y = myGCD y (mod x y)

-- 33.
myCoprime :: Int -> Int -> Bool
myCoprime x y = if (myGCD x y == 1) then True else False

-- 34.
totient :: Int -> Int
totient x 
    | x <= 0 = error "totient not defined for numbers less or equal zero"
totient 1 = 1
totient x = length (filter (myCoprime x) [1 .. x-1])

-- 35.
primeFactors :: Int -> [Int]
primeFactors x
    | x <= 0 = error "not implemented for non positive numbers or 1."
primeFactors 1 = []
primeFactors x = y:primeFactors (quot x y)
    where   y = head ( filter (isFactor x) [2 .. x])
            isFactor :: Int -> Int -> Bool
            isFactor x y = (mod x y == 0)

-- 36.
primeFactorMultiplicities :: Int -> [(Int, Int)]
primeFactorMultiplicities x = map swapTupe ( countDupe (primeFactors x))
    where   swapTupe :: (Int, Int) -> (Int, Int)
            swapTupe (x,y) = (y,x)

-- 37.
totientTwoPointZero :: Int -> Int
totientTwoPointZero x | x <= 0 = error "not implement for non positive numbers."
totientTwoPointZero 1 = 1
totientTwoPointZero x = foldl (*) 1 [ (p-1)*(p^(m-1)) | (p,m) <- factors]
    where   factors = primeFactorMultiplicities x

-- 38.
-- examine the complexities of the two totient functions
--
--
-- totientTwoPointZero x:
--      calls primeFactorMultiplicities x
--          calls primeFactors x  which has complexity (sum of factors of x)
--      thus primeFactorMultiplicites has complexity (sum of factors of x) + ( number of factors of x ) because it traverses that list once
-- tus totientTwoPointZeor has complexity (sum of factors of x) + (number of factors of x )
--
--
-- totient x:
--      has complexity at least x, becaues it must traverse [1..x-1] and check each for coprimeness.
--      myCoprime x y : this has complexity myGCD x y
--          myGCD x y  has complexity bounded by log x


-- 39.
listPrimes :: Int -> Int -> [Int]
listPrimes x y
    | ((x < 0) || (y < 0)) =  error "NEGATIVE NUMBERS WILL NOT BE TOLERATED!"
    | (x > y) = listPrimes y x
listPrimes x y = filter (isPrime) [x..y-1]

-- 40.
goldBach :: Int -> (Int, Int)
goldBach x
    | x <= 2 = error "Goldbach doesn't make sense for numbers less than 3, YOU CRAZY!"
    | (mod x 2 == 1) = error "Goldbach only works on even numbers!"
goldBach x = head [ (p, x-p) | p <- listPrimes 2 x, isPrime (x-p) ]

-- 41.
goldBachList :: Int -> Int -> String
goldBachList x y
    | ((x < 0) || (y < 0)) =  error "NEGATIVE NUMBERS WILL NOT BE TOLERATED!"
--goldBachList x y = 
