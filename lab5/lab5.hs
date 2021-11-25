-- Лабораторна робота №5
-- студентки групи КН-32 підгрупи 1
-- Пхайко Софія
-- Варіант №16

--Завдання 1.Переписати список справа наліво.
reverses :: [a] -> [a]
reverses = reverse

vyvod :: IO()
vyvod = do
-- a) введення з клавіатури
    putStrLn "Input:"
    str<-getLine
    putStrLn(reverse str)
--б) введення даних з файлу
    str<-readFile "input.txt"
    putStrLn(reverse str)
--в) виведення результатів на екран
    putStrLn "Input:"
    str<-getLine
    writeFile "output.txt" (reverse str)
--г) виведення результатів у файл
    str<-readFile "input.txt"
    writeFile "output.txt" (reverse str)

-- Висновок: Ознайомились з модульною органiзацiєю програм та засобами введення-виведення.