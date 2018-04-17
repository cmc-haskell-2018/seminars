{-# LANGUAGE DeriveFunctor #-}
module Seminar_10 where

import Prelude hiding (Monad(..))
import Data.Time

-- | Имя человека.
type Name = String

-- | Номер телефона в международном формате.
newtype Phone = Phone Integer
  deriving (Eq, Show)

-- | Запись о человеке в базе данных.
data Person = Person
  { personName      :: Name         -- ^ Имя человека.
  , personBirthDate :: Day          -- ^ Дата рождения.
  , personMother    :: Maybe Name   -- ^ Имя матери.
  , personFather    :: Maybe Name   -- ^ Имя отца.
  } deriving (Show)

-- вспомогательные определения

sampleFamilyDB :: [(Name, Person)]
sampleFamilyDB = map mkEntry people
  where
    mkEntry person = (personName person, person)

    people =
      [ Person "Alex" (fromGregorian 1998 12 02)
          (Just "Anna") (Just "Oleg")
      , Person "Oleg" (fromGregorian 1975 07 23)
          (Just "Marina") (Just "Fyodor")
      , Person "Fyodor" (fromGregorian 1955 02 28)
          (Just "Evdokiya") (Just "Ruslan")
      , Person "Anna" (fromGregorian 1975 07 23)
          (Just "Vasilisa") (Just "Eugene")
      ]
