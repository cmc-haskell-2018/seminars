{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module Part_01 where

-- * Задание 1.1
--
-- Опишите все возможные реализации функции f1.
-- f1 :: (a, a) -> a

-- * Задание 1.2
--
-- Опишите все возможные реализации функции f2.
-- f2 :: [a] -> String
--
-- Какие реализации удовлетворяют дополнительному условию?
-- f2 [1] == "[1]"

-- * Задание 1.3
--
-- Опишите все возможные реализации функции f3.
-- f3 :: (a -> b) -> [a] -> b

-- * Задание 1.4
--
-- Опишите все возможные реализации функции f4.
-- f4 :: (a -> b -> b) -> b -> [a] -> b
--
-- Какие реализации удовлетворяют дополнительному условию?
-- f4 (+) 0 [1, 2, 3] = 7

-- * Задание 1.5
--
-- Опишите все возможные реализации функции f5.
-- f5 :: (Bool -> a) -> a

-- * Задание 1.6
--
-- Опишите все возможные реализации функции f6.
-- f6 :: (a -> Bool) -> Bool

-- * Задание 2.1
--
-- Реализуйте функцию g1:
-- g1 :: (a -> a) -> a

-- * Задание 2.2
--
-- Реализуйте функцию g2,
-- используя каждый элемент списка ровно один раз:
-- g2 :: (a -> Bool) -> [a] -> Bool

-- * Задание 3.1
--
-- Реализуйте функцию:
-- startsWith :: Int -> Int -> Bool
--
-- >>> startsWith 1 176329
-- True
-- >>> startsWith 9 726
-- False

-- этот вариант мы реализовали на семинаре
-- 
-- Дополнительное задание:
-- попробуйте реализовать функцию без использования show
startsWith :: Int -> Int -> Bool
startsWith n m = and (zipWith (==) ns ms)
  where
    ns = show n
    ms = show m

-- * Задание 3.2
--
-- Опишите все возможные реализации функции secret.
-- secret :: (Int -> a) -> a

-- * Задание 3.3
--
-- Опишите все возможные реализации функции secret2.
-- secret2 :: (Int -> Bool) -> Bool

-- * Задание 3.4
--
-- Сколько возможных реализаций у функции makeSecret?
-- makeSecret :: Int -> (Int -> a) -> a
--
-- Используйте параметрический полиморфизм,
-- чтобы обобщить тип фукнции makeSecret и
-- оставить ровно одну реализацию.

-- * Задание 3.5
--
-- Реализуйте функцию guessNumber:
-- guessNumber :: ((Int -> Bool) -> Bool) -> Int
--
-- >>> guessNumber (makeSecret 3)
-- 3

-- этот вариант мы реализовали на семинаре,
-- но он очень медленный (перебирает все числа)
-- 
-- Дополнительное задание:
-- попробуйте улучшить скорость работы этой функции
-- 
-- Дополнительное задание:
-- какая лучшая алгоритмическая сложность возможна
-- функции guessNumber?
guessNumber :: ((Int -> Bool) -> Bool) -> Int
guessNumber f =
  case candidates of
    [] -> 0
    (x:_) -> x
  where
    candidates = filter check [minBound..maxBound]
    check n = f (== n)
