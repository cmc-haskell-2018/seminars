module Seminar_03 where

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

-- Задание 1.3
-- Реализуйте функцию отображения содержимого корзины.

-- |
-- >>> pprintCart sampleCart
-- 1.5 x apples (23.0 RUB) = 34.5 RUB
-- 2.0 x oranges (27.0 RUB) = 54.0 RUB
-- -----------------------------------
-- Total = 88.5 RUB

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
type ProblemStats = [(StudentName, Maybe Attempts)]

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
