module Case1
where

case1 :: Int -> Int -> Int
case1 a b = a + b

case2 :: Int -> Int
case2 a = a + 1

case4 :: Int -> Int
case4 a = a - 1

case3 :: (Int -> Int) -> Int -> Int
case3 a b = a b

case5 :: Int -> Int
case5 a = a

case6 :: Int -> Int
case6 a = if a > 0 then a else (case5 a)