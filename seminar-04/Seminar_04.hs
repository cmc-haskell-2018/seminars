module Seminar_04 where

import Graphics.Gloss.Interface.Pure.Game

-- * Часть 1

data S a = S a (S a)

data X a b = X a (X b a) | N

-- * Часть 2
-- Судоку — основные типы и функции

-- data Sudoku = ...

-- * Часть 3

-- Игра «Змейка» — основные типы и функции

initGame = ()
renderGame _ = blank
handleGame _ = id
updateGame _ = id

