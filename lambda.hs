convert :: String -> String
convert [] = []
convert (x:xs)
    | x == '.'  = " -> " ++ next
    | x == 'λ'  = "\\" ++ next
    | otherwise = x : next
        where next = convert xs

ltolambda :: String -> String
ltolambda [] = []
ltolambda (x:xs)
    | x == 'λ'  = "lambda " ++ next
    | otherwise = x : next
        where next = ltolambda xs
