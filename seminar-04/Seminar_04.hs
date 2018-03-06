module Seminar_04 where

import Graphics.Gloss.Interface.Pure.Game
import Data.List (nub)

-- * Часть 1

-- первое S — конструктор типа
-- второе S — конструктор значений
-- третье S — конструктор типа
data S a = S a (S a)
  deriving (Show)

s :: S Int
s = S 42 s

naturals :: S Integer
naturals = allNaturalsFrom 1
  where
    allNaturalsFrom :: Integer -> S Integer
    allNaturalsFrom n
      = S n (allNaturalsFrom (n + 1))

takeS :: Int -> S a -> [a]
takeS n (S x xs)
  | n <= 0    = []
  | otherwise = x : takeS (n - 1) xs



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
















-- * Часть 2
-- Судоку — основные типы и функции

data Cell
  = E
  | C Int
  deriving (Show, Eq)

data T a = T a a a
  deriving (Show)

type Row a = T (T a)

type Column a = T (T a)

type Square a = T (T a)

type Sudoku = Square (Square Cell)

nonEmpty :: Cell -> Bool
nonEmpty E = False
nonEmpty _ = True

correctSquare :: Square Cell
correctSquare = T
  (T (C 1) (C 2) (C 3))
  (T (C 4) E     E)
  (T E     E     E)

incorrectSquare :: Square Cell
incorrectSquare = T
  (T (C 1) (C 2) (C 3))
  (T (C 4) E     E)
  (T E     (C 2) E)

noConflicts :: Sudoku -> Bool
noConflicts sudoku = all checkTT tts
  where
    tts = rows sudoku
      ++ columns sudoku
      ++ squares sudoku

ttElems :: T (T a) -> [a]
ttElems (T (T a b c) (T d e f) (T g h i))
  = [a, b, c, d, e, f, g, h, i]

checkTT :: T (T Cell) -> Bool
checkTT tt = nub xs == xs
  where
    xs = filter nonEmpty (ttElems tt)

rows :: Sudoku -> [Row Cell]
rows _ = []

columns :: Sudoku -> [Column Cell]
columns _ = []

squares :: Sudoku -> [Square Cell]
squares _ = []



















-- * Часть 3

-- Игра «Змейка» — основные типы и функции

initGame = ()
renderGame _ = blank
handleGame _ = id
updateGame _ = id

