countMiddleIsS :: String -> Integer
countMiddleIsS ([]) = 0
countMiddleIsS (x:[]) = 0
countMiddleIsS (' ':'i':'s':x:xs) = if (elem x [' ','.',',','?']) then 1 + countMiddleIsS (x:xs) else countMiddleIsS (x:xs)
countMiddleIsS (_:xs) = countMiddleIsS xs

beginIs :: String -> Integer
beginIs ('i':'s':x:xs) = if (elem x [' ','.',',','?']) then 1 else 0
beginIs xs = 0

endIs :: String -> Integer
endIs [] = 0
endIs [_] = 0
endIs [_,_] = 0
endIs " is" = 1
endIs (_:xs) = endIs xs

countIsS :: String -> Integer
countIsS [] = 0
countIsS [x] = 0
countIsS "is" = 1
countIsS [_,_] = 0
countIsS xs = beginIs xs + countMiddleIsS xs + endIs xs
