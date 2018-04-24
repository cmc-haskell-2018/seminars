{-# LANGUAGE DeriveFunctor #-}
module Seminar_11 where

import Control.Monad (unless, forever, ap)
import qualified Data.Time
import System.IO (hSetBuffering, stdin, BufferMode(..))

-- stack install pretty-show
-- import Text.Show.Pretty (pPrint)

-- | Имя человека.
type Name = String

-- | Запись о человеке в базе данных.
data Person = Person
  { personName      :: Name         -- ^ Имя человека.
  , personBirthDate :: Day          -- ^ Дата рождения.
  , personMother    :: Maybe Name   -- ^ Имя матери.
  , personFather    :: Maybe Name   -- ^ Имя отца.
  } deriving (Show)

-- * Часть 1

{-

data Query a = ...

instance Functor      Query
instance Applicative  Query
instance Monad        Query

runQuery :: Query a -> FamilyDB -> Either String a

lookupPersonByName :: Name -> Query Person

fail :: String -> Query a

-}

-- ** Задание 1.1
--
-- Реализуйте функции motherOf и fatherOf
-- 
-- motherOf :: Person -> Query Person
-- fatherOf :: Person -> Query Person

-- ** Задание 1.2
--
-- Реализуйте функцию grandfatherOf,
-- находящую отца отца.
--
-- grandfatherOf :: Person -> Query Person

-- ** Задание 1.3
--
-- Реализуйте функцию parents,
-- возвращающую обоих родителей.
--
-- parentsOf :: Person -> ...

-- ** Задание 1.4
--
-- Реализуйте функции для
-- определения дедушек и бабушек.
--
-- grandmothersOf :: Person -> ...
-- grandfathersOf :: Person -> ...
-- grandparentsOf :: Person -> ...

-- * Часть 2

data Id a = Id a
  deriving (Show, Functor)

-- ** Задание 2.1
--
-- Реализуйте Applicative и Monad для Id
--
-- instance Applicative Id where ...
-- instance Monad Id where ...

-- ** Задание 2.2
--
-- Реализуйте fib при помощи Id.
--
-- fib :: Integer -> Id Integer

-- ** Задание 2.3
--
-- Реализуйте Monad для Maybe
--
-- instance Monad Maybe where ...

-- ** Задание 2.4
--
-- Реализуйте Monad для []
--
-- instance Monad [] where ...

-- ** Задание 2.5
--
-- Реализуйте Monad для ((->) e)
--
-- instance Monad ((->) e) where ...




-- вспомогательные определения

-- | База данных содержит записи о всех известных людях.
type FamilyDB = [(Name, Person)]

-- | DB query returning result of type @a@.
newtype Query a = Query
  { runQuery :: FamilyDB -> Either String a }
  deriving (Functor)

instance Applicative Query where
  pure x = Query (pure (pure x))
  Query qf <*> Query qx = Query ((<*>) <$> qf <*> qx)

instance Monad Query where
  return = pure
  Query qx >>= f = Query $ \db -> do
    x <- qx db
    runQuery (f x) db

  fail msg = Query (pure (Left msg))

-- | Get 'Person' by his or her 'Name'.
lookupPersonByName :: Name -> Query Person
lookupPersonByName name = Query $ \db ->
  case lookup name db of
    Nothing -> Left $
      "unknown person: " ++ name
    Just person -> pure person

-- | Создать базу из списка отдельных записей о людях.
mkFamilyDB :: [Person] -> FamilyDB
mkFamilyDB people = map mkEntry people
  where
    mkEntry person = (personName person, person)

-- | Пример базы из нескольких записей.
sampleFamilyDB :: [(Name, Person)]
sampleFamilyDB = mkFamilyDB
  [ alex, oleg, ivan, anna, eugene ]

alex :: Person
alex = Person "Alex" (fromGregorian 1998 12 02)
  (Just "Anna")
  (Just "Oleg")

oleg :: Person
oleg = Person "Oleg" (fromGregorian 1975 07 23)
  (Just "Marina")
  (Just "Ivan")

ivan :: Person
ivan = Person "Ivan" (fromGregorian 1955 02 28)
  (Just "Evdokiya")
  (Just "Ruslan")

anna :: Person
anna = Person "Anna" (fromGregorian 1975 07 23)
  (Just "Vasilisa")
  (Just "Eugene")

eugene :: Person
eugene = Person "Eugene" (fromGregorian 1975 07 23)
  (Just "Maria")
  (Just "Lev")

-- | Тип-обёртка с более понятной реализацией 'Show'.
newtype Day = Day Data.Time.Day
  deriving (Eq)

fromGregorian :: Integer -> Int -> Int -> Day
fromGregorian year month day
  = Day (Data.Time.fromGregorian year month day)

-- |
-- >>> fromGregorian 2018 04 25
-- fromGregorian 2018 4 25
instance Show Day where
  show (Day date)
    = "fromGregorian"
    ++ " " ++ show year
    ++ " " ++ show month
    ++ " " ++ show day
    where
      (year, month, day) = Data.Time.toGregorian date
