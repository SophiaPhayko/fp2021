-- Лабораторна робота №2
-- студентки групи КН-32 підгрупи 1
-- Пхайко Софія
-- Варіант №16

-- Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з застосуванням вбудованих функцiй.

-- Завдання 1. Знайти останнiй елемент списку.

lastnum1a :: [a] -> [a]
lastnum1a [] = []
lastnum1a [x1] = [x1]
lastnum1a [x1,x2] = [x2]
lastnum1a (x1:xs) = lastnum1a xs

-- Результат тестування:
-- Prelude> lastnum1a [1,3,5]
-- >5

lastnum1b :: [a] -> [a]
lastnum1b [] = []
lastnum1b [x1] = [x1]
lastnum1b [x1,x2] = [x2]
lastnum1b (xs) = lastnum1a (tail xs)

-- Результат тестування:
-- Prelude> lastnum1b [1,3,2]
-- >2

-- Завдання 2. Видалити зі списку елементи з і-го по k-й включно, напр. при і=2 та k=4: "asdfghj" -> "aghj".
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop' (n-1) xs

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs)  =  x : take' (n-1) xs

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take' (from - 1) xs ++ drop' to xs

-- Результат тестування:
-- Prelude>  slice 1 2 [1,2,3,5,4]
-- [3,5,4]

--б)
v1 :: ([a], Int, Int) -> ([a], Int, Int)
v1 (xs, a, b) = if a < b then (let(ys,zs) = splitAt a xs in ys ++ tail zs,a,b)
else (xs,a,b)

-- Результат тестування:
-- Prelude>  v1 ([1,2,3,4,5], 1,2)
-- ([1,3,4,5],1,2)

-- Висновок: під час даної лабораторної робооти я дізналась про рекурсивні функції та працювала з кортежами та списками.
