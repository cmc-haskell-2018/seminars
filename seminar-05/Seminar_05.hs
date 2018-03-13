module Seminar_05 where

import Data.List (intercalate)

-- * Часть 1

-- ** Задание 1.1
-- Определите вид (kind) конструкторов типов.
-- Какие конструкторы типов подходят для реализации Functor
-- (если учитывать только вид (kind))?
-- Выпишите реализацию Functor, где это возможно.
-- Объясните невозможность реализации, где это невозможно.

data A a = A a
data B a = B a (B a)
data C a b = C a (C b a) | CN
data D a = D
data E = E

-- ** Задание 1.2
-- Определите вид (kind) конструкторов типов.
-- Какие конструкторы типов подходят для реализации Functor
-- (если учитывать только вид (kind))?
-- Выпишите реализацию Functor, где это возможно.
-- Объясните невозможность реализации, где это невозможно.

data F a b = F (a -> b)
data G a b = G1 (a -> b) | G2 (b -> a)
data H a b = H ((b -> a) -> a)

-- ** Задание 1.3
-- Определите вид (kind) конструкторов типов.
-- Какие конструкторы типов подходят для реализации Functor
-- (если учитывать только вид (kind))?
-- Выпишите реализацию Functor, где это возможно.
-- Объясните невозможность реализации, где это невозможно.

data I f a = I (f a)
data J f g a = J (f (g a))
data K a f b = K a
data L f = L f (L f)
data M f a = MP a | MF (f (M f a))
data N r f a = N ((a -> f r) -> f r)
data O t m a = O (t m a)

-- * Часть 2

-- ** Задание 2.1
-- Выпишите реализацию Functor для AssocList.
-- Проверьте выполнение закона fmap id == id.

data AssocList k v = AssocList [(k, v)]
  deriving (Eq)

-- ** Задание 2.2
-- Выпишите реализацию Functor для RoseTree.
-- Проверьте выполнение закона fmap id == id.

data RoseTree a = RoseTree a [RoseTree a]
  deriving (Eq)

-- ** Задание 2.3
-- Выпишите реализацию Functor для Grid.
-- Проверьте выполнение закона fmap id == id.

data Grid a = Grid ((Int, Int) -> a)

-- * Часть 3

-- ** Задание 3.1
-- Опишите структуру деревьев с метками на рёбрах.
-- Выпишите реализацию Functor для этой структуры
-- (fmap применяет функцию к меткам на рёбрах).
--
-- data ETree e = ???

-- ** Задание 3.2
-- Опишите тип поля в двумерном пространстве.
-- Выпишите реализацию Functor для этой структуры
-- (fmap применяет функцию к каждому значению поля
-- в пространстве).
--
-- data Field a = ???

-- * Часть 4

-- ** Задание 4.1
-- Выпишите реализацию Functor для Tape.

data Tape a = Tape [a] a [a]
  deriving (Show)

data Cell = Clean | Dirty
  deriving (Show)

sampleTape :: Tape Cell
sampleTape = Tape
  [Clean, Dirty, Dirty, Clean]
  Dirty
  [Clean, Dirty, Clean, Dirty]

neighbourhood :: Int -> Tape a -> [a]
neighbourhood n (Tape ls x rs)
  = reverse (take n ls) ++ [x] ++ take n rs

cellChar :: Cell -> Char
cellChar Clean = '.'
cellChar Dirty = 'X'

-- ** Задание 4.2
-- Выпишите реализацию ppTape
--
-- >>> ppTape cellChar sampleTape
-- .XX.X.X.X
--
-- ppTape :: (a -> Char) -> Tape a -> String
-- ppTape = ???
