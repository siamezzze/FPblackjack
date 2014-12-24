module Deck 
  ( Card
  , possiblePoints
  , Deck
  , DeckState
  , cards
  , gen
  , mkDeck
  , draw
  , shuffle
  , takeRandomCard
  , takeCardAt ) where

import System.Random
import Control.Monad.State
import Data.List

data Card = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving (Enum, Show, Eq)

--Из-за того, что туз оценивается по разному
possibleValues :: Card -> [Int]
possibleValues card
    | card == Ace = [1, 11]
    | card `elem` [Jack,Queen,King] = [10]
    | otherwise = [fromEnum card]


possiblePoints :: [Card] -> [Int]
possiblePoints hand = nub $ map sum $ mapM possibleValues hand

-- Колода - карты + генератор
data Deck = Deck
  { cards :: [Card]
	, number :: Int
  , gen :: StdGen }
  deriving (Show)

-- создать колоду 
-- TODO: больше одной - сделано
--mkDeck :: StdGen -> Deck
mkDeck g n = 
  Deck { cards = concat ( replicate n ( [ card | card <- [Ace ..], _ <- [1..4] :: [Int] ]) )
			 , number = n
       , gen = g }

type DeckState a = State Deck a

-- взять карту
draw :: DeckState Card
draw = takeCardAt 0

-- перемешать
shuffle :: DeckState ()
shuffle = do
  curr <- get
  shuffled <- replicateM (52*(number curr)) takeRandomCard
  put curr { cards = shuffled }

-- Это для перемешивания - достать случайную карту и удалить ее
takeRandomCard :: DeckState Card
takeRandomCard = do
  curr <- get
  let n = length $ cards curr
      (i, gen') = randomR (0, n) $ gen curr
  card <- takeCardAt i
  put curr { gen = gen' }
  return card

-- Еще одна вспомогалельная - достать карту с заданным индексом и удалить ее
takeCardAt :: Int -> DeckState Card
takeCardAt i = do
  curr <- get
  let (cards', cards'') = splitAt (i + 1) $ cards curr
      card              = last cards'
      newCards          = init cards' ++ cards''
  put curr { cards = newCards }
  return card
