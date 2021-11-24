-- Лабораторна робота №4
-- студентки групи КН-32 підгрупи 1
-- Пхайко Софія
-- Варіант №16

-- Мета: Ознайомитись з системою типів та класів типів. Набути досвіду визначення
-- нових типів та класів типів і їх використання.

--Завдання
-- 4.1. Фігури на площині: Визначити функцію для отримання прямокутника, який містить
-- усі фігури із задоного списку.
-- 4.2. Переміщення фігури на вказаний вектор

data Font = Consolas | LucidaConsole | SourceCodePro deriving (Eq, Show)

data Figure = 
    Circle Int Int Int
  | Rect Int Int Int Int
  | Triangle Int Int Int Int Int Int
  | Text Int Int Font String
  deriving (Eq, Show)

getRectangles :: [Figure] -> [Figure]
getRectangles [] = []
getRectangles ((Rect x1 y1 x2 y2) : xs) = Rect x1 y1 x2 y2 : getRectangles xs
getRectangles ((Circle {}) : xs) = getRectangles xs
getRectangles ((Triangle {}) : xs) = getRectangles xs
getRectangles ((Text {}) : xs) = getRectangles xs

getCircles :: [Figure] -> [Figure]
getCircles [] = []
getCircles ((Circle x y r) : xs) = Circle x y r : getCircles xs
getCircles ((Rect {}) : xs) = getCircles xs
getCircles ((Triangle {}) : xs) = getCircles xs
getCircles ((Text {}) : xs) = getCircles xs

getTriangles :: [Figure] -> [Figure]
getTriangles [] = []
getTriangles ((Triangle x1 y1 x2 y2 x3 y3) : xs) = Triangle x1 y1 x2 y2 x3 y3 : getTriangles xs
getTriangles ((Rect {}) : xs) = getTriangles xs
getTriangles ((Circle {}) : xs) = getTriangles xs
getTriangles ((Text {}) : xs) = getTriangles xs

getLabels :: [Figure] -> [Figure]
getLabels [] = []
getLabels ((Text x y f s) : xs) = Text x y f s : getLabels xs
getLabels ((Rect {}) : xs) = getLabels xs
getLabels ((Triangle {}) : xs) = getLabels xs
getLabels ((Circle {}) : xs) = getLabels xs

getFigures :: String -> [Figure] -> [Figure]
getFigures str array
  | str == "Rectangle" = getRectangles array
  | str == "Circle" = getCircles array
  | str == "Triangle" = getTriangles array
  | str == "Text" = getLabels array
  | otherwise = []

--Тестування
--getFigures "Label" [(Circle 1 2 3),(Rect 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Label 0.0 0.0 Consolas "hello"]
--getFigures "Rectangle" [(Circle 1 2 3),(Rect 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Rect 1.0 1.0 4.0 4.0,Rect 1.0 1.0 4.0 5.0]
--getFigures "Triangle" [(Circle 1 2 3),(Rect 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Triangle 1.0 1.0 4.0 4.0 9.0 9.0]
--getFigures "Circle" [(Circle 1 2 3),(Rect 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Circle 1.0 2.0 3.0]

--Додаткове завдання
--перемiщення фiгури на вказаний вектор.
move :: Figure -> Int -> Int -> Figure
move (Rect x1 y1 x2 y2) v1 v2 = Rect (x1 + v1) (y1 + v2) (x2 + v1) (y2 + v2)
move (Circle x y r) v1 v2 = Circle (x + v1) (y + v2) r
move (Text x y f s) v1 v2 = Text (x + v1) (y + v2) f s
move (Triangle x1 y1 x2 y2 x3 y3) v1 v2 = Triangle (x1 + v1) (y1 + v2) (x2 + v1) (y2 + v2) (x3 + v1) (y3 + v2)

--Тестування
--move (Triangle 1 1 4 4 9 9) 2 2
--Triangle 3 3 6 6 11 11
--move (Circle 1 2 3) 3 1
--Circle 4 3 3
--move (Rectangle 1 1 4 4) 5 6
--Rectangle 6 7 9 10
--move (Label 2 4 Consolas "hello") (-1) (-2)
--Label 1 2 Consolas "hello"

--Висновок: під час даної лабораторної робооти я дізналась про нові типи та 
-- класи типів, нові зазнання використала на практиці.
