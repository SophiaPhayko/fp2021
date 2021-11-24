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

--Додаткове завдання
--перемiщення фiгури на вказаний вектор.
move :: Figure -> Int -> Int -> Figure
move (Rect x1 y1 x2 y2) v1 v2 = Rect (x1 + v1) (y1 + v2) (x2 + v1) (y2 + v2)
move (Circle x y r) v1 v2 = Circle (x + v1) (y + v2) r
move (Text x y f s) v1 v2 = Text (x + v1) (y + v2) f s
move (Triangle x1 y1 x2 y2 x3 y3) v1 v2 = Triangle (x1 + v1) (y1 + v2) (x2 + v1) (y2 + v2) (x3 + v1) (y3 + v2)

--Висновок: під час даної лабораторної робооти я дізналась про нові типи та 
-- класи типів, нові зазнання використала на практиці.
