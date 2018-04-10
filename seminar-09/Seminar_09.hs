{-# LANGUAGE DeriveFunctor #-}
module Seminar_09 where

import Data.Functor.Identity

-- class Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

type Gen = Int

mkRandInt :: Gen -> (Int, Gen)
mkRandInt old = (new, new)
  where
    new = (a * old + c) `mod` m
    a = 1103515245
    c = 12345
    m = 2^31

newtype Rand a = Rand { runRand :: Gen -> (a, Gen) }
  deriving (Functor)

instance Applicative Rand where
  pure x = Rand (\g -> (x, g))
  Rand rf <*> Rand rx = Rand $ \g ->
    let (f, gf) = rf g
        (x, gx) = rx gf
    in (f x, gx)

rand :: Rand Int
rand = Rand mkRandInt

-- * Часть 1

newtype State s a = State { runState :: s -> (a, s) }
  deriving (Functor)

instance Applicative (State g) where
  pure x = State (\g -> (x, g))
  State rf <*> State rx = State $ \g ->
    let (f, gf) = rf g
        (x, gx) = rx gf
    in (f x, gx)

-- ** Задание 1.1
--
-- Реализуйте функции get и put.
--
-- get :: ?
--
-- put :: ?

-- ** Задание 1.2
--
-- Реализуйте функцию modify.
--
-- modify :: ?

-- ** Задание 1.3
--
-- Реализуйте подсчёт суммы элементов списка,
-- используя аппликативный функтор State.

-- * Часть 2

class Monoidal f where
  unit  :: f ()
  (<.>) :: (f a, f b) -> f (a, b)

-- ** Задание 2.1
--
-- Реализуйте Monoidal для известных вам
-- аппликативных функторов.

-- instance Monoidal Identity
-- instance Monoidal Maybe
-- instance Monoidal (Either e)
-- instance Monoidal []
-- instance Monoidal ((->) e)
-- instance Monoidal Rand

-- ** Задание 2.2
--
-- Реализуйте Applicative для Wrapped f,
-- используя Monoidal.

newtype Wrapped f a = Wrapped (f a)

-- instance Monoidal f => Applicative (Wrapped f) where

-- ** Задание 2.3
--
-- Реализуйте Monoidal для Wrapped f,
-- используя Applicative.

-- instance Applicative f => Monoidal (Wrapped f) where

-- ** Задание 2.4
--
-- Сформулируйте законы аппликативного функтора в терминах
-- unit и <.>.
