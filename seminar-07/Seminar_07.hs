module Seminar_07 where

import Data.List (intersperse, intercalate)
import System.IO (hSetBuffering, stdin, BufferMode(..))

-- class Foldable f where
--   foldMap :: Monoid m => (a -> m) -> f a -> m

-- * Часть 1: сортировка слиянием

-- | Список, отсортированный по возрастанию.
newtype SortedList a = SortedList [a]
  deriving (Eq, Show)

fromSortedList :: SortedList a -> [a]
fromSortedList (SortedList xs) = xs

-- ** Задание 1.1
-- Реализуйте моноид для SortedList.
--
-- instance Monoid ...

-- ** Задание 1.2
-- Реализуйте функцию сортировки, используя SortedList.
-- 
-- sortViaSortedList :: Ord a => [a] -> [a]

-- ** Задание 1.3
-- Оцените алгоритмическую сложность sortViaSortedList.

-- ** Задание 1.4
-- Определите тип двоичного дерева.
--
-- data BinTree ...

-- ** Задание 1.5
-- Определите Foldable для BinTree
--
-- instance Foldable ...

-- ** Задание 1.6
-- Реализуйте функцию сортировки списка, используя BinTree
--
-- sortViaBinTree :: Ord a => [a] -> [a]

-- ** Задание 1.7
-- Оцените алгоритмическую сложность sortViaBinTree

-- ** Задание 1.8
-- Определите функцию первых k минимальных элементов списка.
--
-- minValues :: Ord a=> Int -> [a] -> [a]

-- ** Задание 1.9
-- Определите алгоритмическую сложность minValues.






-- * Часть 2

-- | Метка игрока.
data Mark
  = X -- ^ Крестик.
  | O -- ^ Нолик.
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell = Maybe Mark

-- | Доска-поле для игры крестики-нолики.
newtype Board = Board [[Cell]]
  deriving (Eq, Show)

-- | Пустая доска 3x3.
emptyBoard3x3 :: Board
emptyBoard3x3 = Board (replicate 3 (replicate 3 Nothing))

-- | Поставить метку на заданную клетку доски, если
--
-- * клетка с указанными координатами существует
-- * клетка не занята
putMark :: (Int, Int) -> Mark -> Board -> Board
putMark (i, j) mark (Board xss) = Board yss
  where
    yss = modifyAt j (modifyAt i f) xss

    -- если клетка пуста — ставим метку
    f Nothing = Just mark
    -- если клетка занята — оставляем как есть
    f cell    = cell

-- | Изменить i-ый элемент списка
-- при помощи заданной функции.
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt 0 f (x:xs) = f x : xs
modifyAt i f (x:xs) = x : modifyAt (i - 1) f xs

-- | Отобразить клетку игрового поля.
ppCell :: Cell -> String
ppCell Nothing = " "
ppCell (Just mark) = show mark

-- | Отобразить игровое поле.
ppBoard :: Board -> String
ppBoard (Board xss) = unlines $ intersperse hr rows
  where
    ppRow = intercalate "|" . map ppCell
    rows = map ppRow xss

    -- hr будет исползован только если
    -- в rows есть хотя бы одна строка
    -- поэтому здесь можно использовать head
    hr = map f (head rows)
      where
        f '|' = '+'
        f _   = '-'

switchPlayer :: Mark -> Mark
switchPlayer X = O
switchPlayer O = X

-- | Запустить игру с заданными параметрами.
runWith
  :: Board                -- ^ Состояние игрового поля.
  -> Mark                 -- ^ Чей ход?
  -> (Mark -> Board -> IO Board)  -- ^ Функция хода крестиков.
  -> (Mark -> Board -> IO Board)  -- ^ Функция хода ноликов.
  -> IO ()
runWith board mark xmove omove = do
  putStrLn (ppBoard board)
  newBoard <- case mark of
    X -> xmove X board
    O -> omove O board
  if newBoard == board
    then runWith board mark xmove omove
    else runWith newBoard (switchPlayer mark) xmove omove

-- | Запустить игру на поле 3x3.
run3x3 :: IO ()
run3x3 = do
  hSetBuffering stdin LineBuffering
  runWith emptyBoard3x3 X playerMove aiMove
  where
    playerMove mark board = do
      putStr (show mark ++ " move: ")
      coords <- readLn
      return (putMark coords mark board)

    -- пока что вместо бота используем второго игрока
    aiMove = playerMove

-- ** Задание 2.1
-- Выпишите тип функции ИИ.

-- ** Задание 2.2
--
-- Реализуйте функцию искуственного интеллекта для игры
-- крестики-нолики, где ИИ выбирает первую доступную
-- ячейку для своего хода.

-- ** Задание 2.3
--
-- Реализуйте функции ИИ с эвристической оценкой
-- игрового поля, и просмотром всех возможных игр
-- на заданное количество ходов вперёд.
