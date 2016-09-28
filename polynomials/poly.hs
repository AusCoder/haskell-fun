

-- for testing purposes
-- let x = Poly [1,2,1]
-- let y = Poly [2,1,1]
-- let z = Poly [-2,1]

data Poly = Poly [Int] deriving (Show)

polyMult :: Poly -> Poly -> Poly
polyMult (Poly _) (Poly[]) = error "cant multiply with nothing!"
polyMult (Poly []) (Poly _) = error "cant multiply with nothing!"
polyMult (Poly xs) (Poly ys) = Poly $ diagReduce $ map (\y -> map (*y) xs) ys
    where   diagReduce :: [[Int]] -> [Int]
            diagReduce [] = error "can't diagReduce empty list!"
            diagReduce xs
                | length xs == 1 = (xs !! 0)
            diagReduce (x:y:xs) = diagReduce $ (stager x y) : (map (0:) xs)
                where   stager :: [Int] -> [Int] -> [Int]
                        stager [] _ = error "cant stager nothing"
                        stager _ [] = error "cant stager nothing"
                        stager x y = (head x) : ((zipWith (+) (tail x) y) ++ [last y])

polyDiff :: Poly -> Poly -> Poly
polyDiff (Poly _) (Poly[]) = error "cant diff with nothing!"
polyDiff (Poly []) (Poly _) = error "cant diff with nothing!"
polyDiff (Poly xs) (Poly ys) =
    if (length xs) < (length ys)
        then Poly $ zipWith (-) (leftPad xs $ length ys) ys
    else Poly $ zipWith (-) xs (leftPad ys $ length xs)

polySum :: Poly -> Poly -> Poly
polySum (Poly _) (Poly[]) = error "cant sum with nothing!"
polySum (Poly []) (Poly _) = error "cant sum with nothing!"
polySum (Poly xs) (Poly ys) =
    if (length xs) < (length ys)
        then Poly $ zipWith (+) (leftPad xs $ length ys) ys
    else Poly $ zipWith (+) xs (leftPad ys $ length xs)

leftPad :: [Int] -> Int -> [Int]
leftPad xs n = if ((length xs) >= n) then xs else leftPad (0:xs) n



-- polySum :: Poly -> Poly -> Poly
-- polySum (Poly _) (Poly[]) = error "cant sum with nothing!"
-- polySum (Poly []) (Poly _) = error "cant sum with nothing!"
-- polySum (Poly xs) (Poly ys) = Poly $ zipWith (+) xs ys

deriv :: Poly -> Poly
deriv (Poly [x]) = Poly [0]
deriv (Poly xs) = Poly $ zipWith (*) (init xs) [(length xs) - 1, (length xs) - 2 .. 0]


-- types are a bit messed up here, need to change Poly to take [Num] or [Rational] or something
eval :: Float -> Poly -> Float
eval _ (Poly [x]) = fromIntegral x
eval c (Poly xs) = sum $ zipWith (*) (map fromIntegral xs) [c**(fromIntegral n) | n <- [deg, deg-1 .. 0]]
    where
        deg = (length xs) - 1

newtonsMeth :: Float -> Poly -> Float
newtonsMeth c (Poly xs)
    | abs (eval c (Poly xs)) < 0.000001 = c
newtonsMeth c (Poly xs) = newtonsMeth nextPoint (Poly xs)
    where
        nextPoint = c - (eval c (Poly xs))/(eval c (deriv (Poly xs)))

verboseNewtonsMeth :: Float -> Poly -> IO ()
verboseNewtonsMeth c (Poly xs)
    | abs (eval c (Poly xs)) < 0.000001 = do
        putStr "found a suitable root: "
        putStrLn $ show c
verboseNewtonsMeth c (Poly xs) = do
    putStr "trying point: "
    putStrLn $ show c
    verboseNewtonsMeth nextPoint (Poly xs)
    where
        nextPoint = c - (eval c (Poly xs))/(eval c (deriv (Poly xs)))

myFilter :: (a->Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if (p x) then x:acc else acc) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> (f x):acc) []

intersection :: Poly -> Poly -> Float
intersection p1 p2 = newtonsMeth initPoint $ polyDiff p1 p2
    where initPoint = 1.0

verboseIntersection :: Poly -> Poly -> IO ()
verboseIntersection p1 p2 = verboseNewtonsMeth initPoint $ polyDiff p1 p2
    where initPoint = 1.0
