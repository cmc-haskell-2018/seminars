module Seminar_03 where

import Data.List (foldl')

-- * Часть 1

-- | Наименование товара.
type Title = String

-- | Стоимость в рублях.
type RUB = Double

-- | Количество товара.
type Amount = Double

-- | Позиция в корзине.
type Item = (Title, RUB)

-- Задание 1.1
-- Реализуйте функцию подсчёта суммы простой корзины.

-- | Простая корзина (в интернет-магазине).
type SimpleCart = [Item]

-- | Пример простой корзины.
sampleSimpleCart :: SimpleCart
sampleSimpleCart =
  [ ("apples",  23.0)
  , ("oranges", 27.0)
  ]

simpleCartTotal1 :: SimpleCart -> RUB
simpleCartTotal1 [] = 0
simpleCartTotal1 (item : items)
  = snd item + simpleCartTotal1 items

simpleCartTotal2 :: SimpleCart -> RUB
simpleCartTotal2 = foldl' f 0
  where
    f :: RUB -> Item -> RUB
    f total (_, cost) = total + cost

simpleCartTotal3 :: SimpleCart -> RUB
simpleCartTotal3 = sum . map snd

-- Задание 1.2
-- Реализуйте функцию подсчёта суммы корзины
-- c учётом количества товара на каждой позиции.

-- | Корзина (в интернет-магазине).
type Cart = [(Amount, Item)]

-- | Пример корзины.
sampleCart :: Cart
sampleCart =
  [ (1.5, ("apples",  23.0))
  , (2.0, ("oranges", 27.0))
  ]

amountCost :: (Amount, Item) -> RUB
amountCost (amount, (_, cost)) = amount * cost

cartTotal1 :: Cart -> RUB
cartTotal1 [] = 0
cartTotal1 (item : items)
  = amountCost item + cartTotal1 items

cartTotal2 :: Cart -> RUB
cartTotal2 = foldl' f 0
  where
    f :: RUB -> (Amount, Item) -> RUB
    f total item = total + amountCost item

cartTotal3 :: Cart -> RUB
cartTotal3 = sum . map amountCost

-- Задание 1.3
-- Реализуйте функцию отображения содержимого корзины.

-- |
-- >>> pprintCart sampleCart
-- 1.5 x apples (23.0 RUB) = 34.5 RUB
-- 2.0 x oranges (27.0 RUB) = 54.0 RUB
-- -----------------------------------
-- Total = 88.5 RUB

pprintCart :: Cart -> String
pprintCart cart
  = unlines items
  ++ line ++ "\n"
  ++ pprintCartTotal cart
  where
    items = map pprintItem cart
    n = maximum (map length items)
    line = replicate n '-'

pprintCartTotal :: Cart -> String
pprintCartTotal cart = "Total = "
  ++ show (cartTotal3 cart) ++ " RUB"

pprintItem :: (Amount, Item) -> String
pprintItem row@(amount, (name, cost))
  = show amount ++ " x " ++ name
  ++ " (" ++ show cost ++ " RUB) = "
  ++ show (amountCost row) ++ " RUB"








-- * Часть 2

-- | Имя студента.
type StudentName = String

-- | Название задачи.
type ProblemName = String

-- | Количество попыток.
type Attempts = Int

-- Задача 2.1
-- Реализуйте функцию подсчёта сложности задачи.
-- Сложность задачи определяется как среднее количество попыток решить задачу.

-- | Статистика решения задачи студентами.
type ProblemStats
  = [(StudentName, Maybe Attempts)]

-- | Пример статистики решения задачи.
sampleProblemStats :: ProblemStats
sampleProblemStats =
  [ ("Иванов",   Just 3)
  , ("Петров",   Just 6)
  , ("Сидоров",  Nothing)
  , ("Васильев", Just 7)
  ]

-- | Пример статистики решения задачи.
sampleProblemStats2 :: ProblemStats
sampleProblemStats2 =
  [ ("Иванов",   Nothing)
  , ("Петров",   Just 1)
  , ("Сидоров",  Just 1)
  , ("Васильев", Just 2)
  ]

type Difficulty = Double

problemDifficulty1
  :: ProblemStats -> Maybe Difficulty
problemDifficulty1 [] = Nothing
problemDifficulty1 stats
  = Just (totalAttempts stats / totalStudents)
  where
    f Nothing         = 0
    f (Just attempts) = fromIntegral attempts

    totalAttempts = sum . map f . map snd

    totalStudents = fromIntegral (length stats)



-- |
-- >>> problemDifficulty sampleProblemStats
-- 4.0

-- Задача 2.2
-- Реализуйте функцию подсчёта сложности курса.
-- Сложность курса определяется как средняя сложность задачи.

-- | Статистика решений по ряду задач курса.
type CourseStats = [(ProblemName, ProblemStats)]

sampleCourseStats :: CourseStats
sampleCourseStats =
  [ ("Задача 1", sampleProblemStats)
  , ("Задача 2", sampleProblemStats2)
  ]

-- |
-- >>> courseDifficulty sampleCourseStats
-- 2.5

-- Задача 2.3
-- Реализуйте функцию, определяющую статистику решений задач для выбранного студента.

-- | Статистика решений задач для одного студента.
type StudentStats = [(ProblemName, Maybe Attempts)]

-- |
-- >>> studentStats "Петров" sampleCourseStats
-- [("Задача 1", Just 6), ("Задача 2", Just 1)]

-- * Часть 3

-- Задача 3.1
-- Реализуйте функцию нахождения нуля функции
-- (точки, в которой значение функции равно нулю),
-- используя метод Ньютона.

-- |
-- >>> zero (\x -> x^2 - 2*x)
-- 2.0

approximations
  :: (Double -> Double) -- ^ функция
  -> (Double -> Double) -- ^ производная
  -> Double    -- ^ начальное приближение
  -> [Double]  -- ^ последовательность приближений
approximations f f' x0
  = x0 : approximations f f' x1
  where
    x1 = x0 - f x0 / f' x0

