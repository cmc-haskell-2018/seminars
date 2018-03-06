module Seminar_04 where

import Graphics.Gloss.Interface.Pure.Game
import Data.List (nub)

-- * Часть 1

-- ** Задание 1.1

-- | S a — бесконечная последовательность
-- значений типа a.
--
-- первое S — конструктор типа
-- второе S — конструктор значений
-- третье S — конструктор типа
data S a = S a (S a)
  deriving (Show)

-- | Бесконечная последовательность 42.
s :: S Int
s = S 42 s

-- | Бесконечная последовательность натуральных чисел.
naturals :: S Integer
naturals = allNaturalsFrom 1
  where
    -- | Бесконечная последовательность чисел, начиная с заданного.
    allNaturalsFrom :: Integer -> S Integer
    allNaturalsFrom n
      = S n (allNaturalsFrom (n + 1))

-- | Выделить первые N значений из последовательности.
--
-- Результат — список, а не S a,
-- потому что S a не может быть конечным.
takeS :: Int -> S a -> [a]
takeS n (S x xs)
  | n <= 0    = []
  | otherwise = x : takeS (n - 1) xs

-- ** Дополнительное задание
-- Реализуйте функцию runningAverage,
-- вычисляющую локальное среднее значение
-- для каждого элемента последовательности.
--
-- Если a_i — i-ый элемент исходной последовательности,
-- то новая последовательность будет состоять из элементов
--
-- b_i = (a_(i-n) + ... + a_(i+n)) / (2*n + 1)
--
-- runningAverage :: (Num a, Fractional a) => Int -> S a -> S a

-- ** Задание 1.2

data X a b = X a (X b a) | N
  deriving (Show)

-- X N1 N2
--
-- X :: aX -> X bX aX -> X aX bX
-- N1 :: X a1 b1
-- N2 :: X a2 b2
--
-- aX = X a1 b1
-- X bX aX = X a2 b2
-- 
-- bX = a2
-- aX = b2
-- aX = X a1 b1
--
-- X N1 N2 :: X (X a1 b1) a2

-- [ ] ошибка
-- [ ] X a b
-- [ ] X (X a b) b
-- [x] X (X a b) c


-- ** Дополнительное задание
--
-- Объявите тип данных, соответствующий дереву игры,
-- где на каждом уровне

-- * Часть 2
-- Судоку — основные типы и функции

-- | Ячейка в головоломке Судоку.
data Cell
  = E      -- ^ Пустая ячейка (empty).
  | C Int  -- ^ Ячейка с заполненной цифрой.
  deriving (Show, Eq)

-- | Тройка значений одного типа.
data T a = T a a a
  deriving (Show)

-- | Строка — это 3x3=9 значений.
type Row a = T (T a)

-- | Столбец — это 3x3=9 значений.
type Column a = T (T a)

-- | Квадрат — это 3 строки по 3 значения.
type Square a = T (T a)

-- | Судоку — это квадрат квадратов ячеек.
type Sudoku = Square (Square Cell)

-- | Является ли ячейка пустой?
nonEmpty :: Cell -> Bool
nonEmpty E = False
nonEmpty _ = True

-- | Пример корректного малого квадрата.
correctSquare :: Square Cell
correctSquare = T
  (T (C 1) (C 2) (C 3))
  (T (C 4) E     E)
  (T E     E     E)

-- | Пример некорректного малого квадрата (есть конфликт).
incorrectSquare :: Square Cell
incorrectSquare = T
  (T (C 1) (C 2) (C 3))
  (T (C 4) E     E)
  (T E     (C 2) E)

-- | Пустой малый квадрат.
emptySquare :: Square Cell
emptySquare = T e e e
  where
    e = T E E E

-- | Пример частично заполненной головоломки Судоку.
sampleSudoku :: Sudoku
sampleSudoku = T
  (T c e c)
  (T e c e)
  (T e e c)
  where
    c = correctSquare
    e = emptySquare

-- | Проверка Судоку на отсутствие конфликтов.
noConflicts :: Sudoku -> Bool
noConflicts sudoku = all checkTT tts
  where
    tts = rows sudoku
      ++ columns sudoku
      ++ squares sudoku

-- | Привести тройку к списку.
tElems :: T a -> [a]
tElems (T a b c) = [a, b, c]

-- | Привести девятку (тройку троек) к списку.
ttElems :: T (T a) -> [a]
ttElems tt = concat (map tElems (tElems tt))

-- | Проверить девятку ячеек на отсутствие конфликтов.
-- Конфликты отсутствуют, если в заполненных ячейках нет
-- повторяющихся значений.
checkTT :: T (T Cell) -> Bool
checkTT tt = nub xs == xs
  where
    xs = filter nonEmpty (ttElems tt)

-- | Выбрать все строки Судоку.
--
-- Чтобы выбрать все строки мы
-- - выбираем все строки из малых квадратов (tElems sudoku)
-- - транспонируем каждую строку из малых квадратов,
--   чтобы получить тройку строк (transposeT)
-- - переводим тройки в список и конкатенируем списки
rows :: Sudoku -> [Row Cell]
rows sudoku
  = concat (map (tElems . transposeT) (tElems sudoku))

-- | Транспонировать тройку троек.
--
-- Если думать о девятке как о квадрате,
-- то transposeT транспонирует квадрат.
--
-- Если транспонировать строку квадратов,
-- мы получим тройку строк.
--
-- >>> transposeT (T (T 1 2 3) (T 4 5 6) (T 7 8 9))
-- T (T 1 4 7) (T 2 5 8) (T 3 6 9)
transposeT :: T (T a) -> T (T a)
transposeT (T (T r11 r12 r13) (T r21 r22 r23) (T r31 r32 r33))
  = T (T r11 r21 r31) (T r12 r22 r32) (T r13 r23 r33)

-- | Выбрать все столбцы Судоку.
--
-- Чтобы выбрать все столбцы мы
-- - транспонируем квадрат малых квадратов
-- - транспонируем каждый малый квадрат
-- - из получившего транспонированного Судоку
--   достаём список строк
columns :: Sudoku -> [Column Cell]
columns sudoku
  = rows (mapSmallSquares transposeT (transposeT sudoku))

-- | Применить функцию к каждому малому квадрату Судоку.
mapSmallSquares
  :: (Square Cell -> Square Cell)
  -> Sudoku
  -> Sudoku
mapSmallSquares f sudoku = mapT (mapT f) sudoku

-- | Применить функцию к каждому значению в тройке.
mapT :: (a -> b) -> T a -> T b
mapT f (T a b c) = T (f a) (f b) (f c) 

-- | Выбрать все малые квадраты Судоку.
squares :: Sudoku -> [Square Cell]
squares = ttElems

-- ** Дополнительное задание
--
-- Реализуйте функции для отображения Судоку и его частей.
--
-- >>> ppSudoku sampleSudoku
-- 1 2 3 | . . . | 1 2 3
-- 4 . . | . . . | 4 . .
-- . . . | . . . | . . .
-- ------+-------+------
-- . . . | 1 2 3 | . . .
-- . . . | 4 . . | . . .
-- . . . | . . . | . . .
-- ------+-------+------
-- . . . | . . . | 1 2 3
-- . . . | . . . | 4 . .
-- . . . | . . . | . . .
--
-- >>> ppSquare correctSquare
-- 1 2 3
-- 4 . .
-- . . .
--
-- >>> ppRow correctSquare
-- 1 2 3 | 4 . . | . . .
--
-- >>> ppColumn correctSquare
-- 1
-- 2
-- 3
-- -
-- 4
-- .
-- .
-- -
-- .
-- .
-- .
