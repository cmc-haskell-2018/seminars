{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-} -- включаем ворнинги
module Seminar_01 where

-- здесь происходит импорт некоторых функций,
-- необходимых для реализации чат-бота
import Control.Monad (unless)
import System.IO (hSetBuffering, stdin, BufferMode(..))

-- | Расчёт факториала.
--
-- >>> factorial 7
-- 5040
-- >>> factorial 25
-- 15511210043330985984000000
--
-- Расчёт факториала на отрицательных числах выдаёт 0:
--
-- >>> factorial (-1)
-- 0
factorial :: Integer -> Integer
factorial 0 = 1
factorial n
  | n > 0     = n * factorial (n - 1)
  | otherwise = 0

-- | Выделить инициалы из полного имени.
--
-- >>> initials "John Doe"
-- "J.D."
-- >>> initials "Nick"
-- "N."
-- >>> initials ""
-- ""
initials :: String -> String
initials ""    = ""
initials (c:s) = c : '.' : initials (removeFirstWord s)

-- | Убрать первое слово и пробельный символ.
--
-- >>> removeFirstWord "John Doe"
-- "Doe"
removeFirstWord :: String -> String
removeFirstWord "" = ""
removeFirstWord (c:s)
  | c == ' '  = s
  | otherwise = removeFirstWord s

-- * Чат-бот для управления задачами

-- | Команда (ввод пользователя).
type Command = String

-- | Задача (просто текст).
type Task = String

-- | Нумерация задач.
--
-- >>> numerate ["hello", "world"]
-- [(1,"hello"),(2,"world")]
numerate :: [Task] -> [(Int, Task)]
numerate = f 1
  where
    f _ [] = []
    f n (task:tasks)
      = (n, task) : f (n + 1) tasks

-- | Обработка ввода пользователя.
process
  :: [Task]   -- ^ Список текущих дел.
  -> Command  -- ^ Ввод пользователя.
  -> (String, [Task])
process tasks cmd = case words cmd of
  -- отобразить список задач
  ["/show"]
    -> ("Ваши задачи:\n" ++ unlines (map show (numerate tasks)), tasks)
  -- завершить выполнение последней задачи
  ["/complete"] -> case tasks of
    [] -> ("Нет задач!", tasks)
    (task:rest) -> ("Выполнено: " ++ task, rest)
  -- добавить задачу
  _ -> ("Записано!", cmd : tasks)

-- | Запуск бота с заданным списком задач.
taskBot :: [Task] -> IO ()
taskBot savedTasks = do
  hSetBuffering stdin LineBuffering
  go savedTasks
  where
    go tasks = do
      putStr "<Вы> "
      s <- getLine
      unless (s == "exit") $ do
        let (output, newTasks) = process tasks s
        putStr "<Бот> "
        putStrLn output
        go newTasks
