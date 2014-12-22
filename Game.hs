module Game where

import Deck
import System.Random
import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State

type Hand = [Card]
data Action = Hit | Stay deriving (Eq, Read)
data PlayerState = Plays | Stays | Boosted | Blackjack deriving (Eq, Read, Show)

data Game = Game
  { shoe :: Deck
  , pids :: [String]
  , playerHands :: Map.Map String Hand
  , playerStates :: Map.Map String PlayerState
  , playerNames :: Map.Map String String
  , dealerHand :: [Card] 
  , dealerState :: PlayerState} deriving (Show)

nDecks = 4

makeGame :: Map.Map String String -> StdGen -> Game
makeGame names gen = 
  let ps        = Map.keys names
      s         = execState shuffle $ mkDeck gen nDecks
      emptygame = Game
        { shoe         = s
        , pids         = ps
        , playerHands  = Map.empty
        , playerStates = Map.fromList (map (\pid -> (pid, Plays)) ps)
        , playerNames  = names
        , dealerHand   = []
        , dealerState  = Plays }
  in initializeHands emptygame

drawCard :: Game -> String -> (Hand, Game)
drawCard game "dealer" =
  let (card, deck') = runState draw $ shoe game
      hand          = dealerHand game
      hand'         = card : hand
      game'         = game {shoe        = deck',
                            dealerHand  = hand'}
  in (hand', game')
drawCard game pid =
  let (card, deck') = runState draw $ shoe game
      hands         = playerHands game
      hand          = Map.findWithDefault [] pid hands
      hand'         = card : hand
      hands'        = Map.insert pid hand' hands
      game'         = game {shoe        = deck',
                            playerHands = hands'}
  in (hand', game')
  
drawCards :: Int -> Game -> String -> Game
drawCards n game player = (iterate (\g -> snd $ drawCard g player) game) !! n 

initializeHands :: Game -> Game
initializeHands game =
  let ps = pids game
  in  foldl (drawCards 2) game (ps ++ ["dealer"])

setState :: String -> PlayerState -> Game -> Game
setState "dealer" ps game = game {dealerState = ps}
setState pid ps game = game {playerStates = Map.insert pid ps $ playerStates game}

bust :: Hand -> Bool
bust = and . map ((<) 21) . possiblePoints --проверить

twentyOne :: Hand -> Bool
twentyOne = any ((==) 21) . possiblePoints

eState :: Hand -> PlayerState --не учитывает Stays, по логике вообще не должен вызываться в этом случае
eState hand = if bust hand then Boosted else if twentyOne hand then Blackjack else Plays

hit :: String -> Game -> Game
hit pid game = 
  let (hand, game') = drawCard game pid
      state         = eState hand
  in setState pid state game'

getState :: Game -> String -> PlayerState
getState game pid = (playerStates game) Map.! pid

plays :: Game -> String -> Bool 
plays game pid = (getState game pid) == Plays

--For testing only
main :: IO ()
main = do
  stdGen <- newStdGen
  let game = makeGame (Map.fromList [("player1", "Ada"), ("player2", "Sally")]) stdGen
  putStrLn $ show game
