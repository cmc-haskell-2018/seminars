module Main where

import Graphics.Gloss.Interface.Pure.Game
import Seminar_04

-- Запуск игры «Змейка».
main :: IO ()
main
  = play display bgColor fps
      initGame renderGame handleGame updateGame
  where
    display = InWindow "Змейка" (400, 400) (100, 100)
    bgColor = black
    fps     = 30
