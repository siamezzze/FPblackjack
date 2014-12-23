module Game where

import Deck
import System.Random
import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Maybe
import Data.Binary

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
        , playerStates = Map.empty
        , playerNames  = names
        , dealerHand   = []
        , dealerState  = Plays }
  in initializeStates $ initializeHands emptygame

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

initializeStates :: Game -> Game
initializeStates game = 
  let ps     = pids game
  in  game {playerStates = Map.fromList $ map (\pid -> (pid, eState $ getHand game pid)) ps} --TODO: инициализировать руку дилеера

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

best :: Hand -> Int
best = maximum . filter ((>=) 21) . possiblePoints

score :: Hand -> Int
score h 
  | bust h    = 0
  | otherwise = best h

eState :: Hand -> PlayerState --не учитывает Stays, по логике вообще не должен вызываться в этом случае
eState hand = if bust hand then Boosted else if twentyOne hand then Blackjack else Plays

hit :: String -> Game -> Game
hit pid game = 
  let (hand, game') = drawCard game pid
      state         = eState hand
  in setState pid state game'

stay :: String -> Game -> Game
stay pid = setState pid Stays

getState :: Game -> String -> PlayerState
getState game pid = (playerStates game) Map.! pid

getName :: Game -> String -> String
getName game pid = (playerNames game) Map.! pid

getHand :: Game -> String -> Hand
getHand game pid = (playerHands game) Map.! pid

plays :: Game -> String -> Bool 
plays game pid = (getState game pid) == Plays

anybodyPlays :: Game -> Bool
anybodyPlays game = any (plays game) (pids game)

obfuscate :: Hand -> String
obfuscate h = show $ "?" : (map (show) (tail h))

players :: Game -> [String]
players game = filter (\pid -> plays game pid) (pids game)

winners :: Game -> [String]
winners game = filter (\pid -> (score $ getHand game pid) > (score $ dealerHand game)) (pids game)

pushers :: Game -> [String]
pushers game = if (21 == score (dealerHand game)) then filter (\pid -> (score $ getHand game pid) == 21) (pids game) else []

showGame :: Game -> String
showGame game =
  let playersInfo = map (\pid -> (getName game pid) ++ " : " ++ show (getHand game pid) ++ " " ++ show (score $ getHand game pid) ++ " - " ++ show (getState game pid)) (pids game)
      dealerInfo  = "Dealer: " ++ (obfuscate $ dealerHand game)
  in (unlines playersInfo) ++ "\n" ++ dealerInfo

parseAction :: String -> Maybe Action
parseAction "Hit" = Just Hit
parseAction "h" = Just Hit
parseAction "Stay" = Just Stay
parseAction "s" = Just Stay
parseAction _ = Nothing

askForAction' :: IO Action
askForAction' = do
  putStrLn "Enter your action (\"Hit\", \"h\" - Hit, \"Stay\", \"s\" - Stay."
  input <- getLine
  let pa = parseAction input
  if isJust pa then return (fromJust pa) else askForAction' 

askForAction :: Game -> String -> IO Action
askForAction game pid = do
  putStrLn $ showGame game
  putStrLn $ "Your hand: " ++ show (getHand game pid) ++ " " ++ show (score $ getHand game pid)
  askForAction'
  
applyAction :: Game -> String -> Action -> Game
applyAction game pid Hit  = hit pid game
applyAction game pid Stay = stay pid game

playerInfo :: Game -> String -> String
playerInfo game pid = "Your hand: " ++ show (getHand game pid) ++ " " ++ show (score $ getHand game pid) ++ " - " ++ show (getState game pid)

interactWithPlayer :: Game -> String -> IO Game
interactWithPlayer game pid = do
  let st = getState game pid
  if (st /= Plays) then return game else do
    act <- askForAction game pid
    let game' = case act of Hit  -> hit pid game
                            Stay -> stay pid game
    putStrLn $ "Your hand: " ++ show (getHand game' pid) ++ " " ++ show (score $ getHand game' pid) ++ " - " ++ show (getState game' pid)
    return game'

actionToInt :: Action -> Int
actionToInt Hit = 1
actionToInt Stay = 0

actionFromInt :: Int -> Action
actionFromInt 1 = Hit
actionFromInt 0 = Stay

--For testing only
--main :: IO ()
--main = do
--  stdGen <- newStdGen
--  let game = makeGame (Map.fromList [("player1", "Ada"), ("player2", "Sally")]) stdGen
--  putStrLn $ showGame game
--  let game' = stay "player1" game
--  putStrLn $ showGame game'
--  game'' <- interactWithPlayer game' "player2"
--  putStrLn $ showGame game''
