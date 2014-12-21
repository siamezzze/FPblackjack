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

-- |The 'Card' data type represents the possible playing card ranks.
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
-- TODO: больше одной
--mkDeck :: StdGen -> Deck
mkDeck g n = 
  Deck { cards = concat ( replicate n ( [ card | card <- [Ace ..], _ <- [1..4] :: [Int] ]) )
			 , number = n
       , gen = g }

type DeckState a = State Deck a

-- взять карту
draw :: DeckState Card
draw = takeCardAt 0

-- |'shuffle' takes a 52-card deck and randomly shuffles its elements.
shuffle :: DeckState ()
shuffle = do
  curr <- get
  shuffled <- replicateM (52*(number curr)) takeRandomCard
  put curr { cards = shuffled }

-- |'takeRandomCard' will pick one random card from the deck and remove it.
-- It is a helper-function used by 'shuffle'.
takeRandomCard :: DeckState Card
takeRandomCard = do
  curr <- get
  let n = length $ cards curr
      (i, gen') = randomR (0, n) $ gen curr
  card <- takeCardAt i
  put curr { gen = gen' }
  return card

-- |'takeCardAt' will pick the card at the given index and remove it from the
-- deck.
takeCardAt :: Int -> DeckState Card
takeCardAt i = do
  curr <- get
  let (cards', cards'') = splitAt (i + 1) $ cards curr
      card              = last cards'
      newCards          = init cards' ++ cards''
  put curr { cards = newCards }
  return card
