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
--Количество дек
nDecks = 4

--Создать игру
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

--Взять карту
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
  
--Взять несколько карт
drawCards :: Int -> Game -> String -> Game
drawCards n game player = (iterate (\g -> snd $ drawCard g player) game) !! n 

--Инициализация состояний для начала игры (вообще говоря, по умолчанию считаем, что все играют, т.е. Plays, но возможна ситуация блэкджека в первых 2х картах
initializeStates :: Game -> Game
initializeStates game = 
  let ps     = pids game
  in  game {playerStates = Map.fromList $ map (\pid -> (pid, eState $ getHand game pid)) ps,
            dealerState  = eState $ dealerHand game} --TODO: инициализировать руку дилеера

--Сдать карты
initializeHands :: Game -> Game
initializeHands game =
  let ps = pids game
  in  foldl (drawCards 2) game (ps ++ ["dealer"])

--Установить состояние (Играет, Прекратил (Stay), Перебор (Boosted), Блэкджек)
setState :: String -> PlayerState -> Game -> Game
setState "dealer" ps game = game {dealerState = ps}
setState pid ps game = game {playerStates = Map.insert pid ps $ playerStates game}

--Проверка на перебор
bust :: Hand -> Bool
bust = and . map ((<) 21) . possiblePoints --проверить

--Проверка на блэкджек
twentyOne :: Hand -> Bool
twentyOne = any ((==) 21) . possiblePoints

--Поиск лучшего из возможных счетов по данной руке
best :: Hand -> Int
best = maximum . filter ((>=) 21) . possiblePoints

--Подсчет очков - так же, но если перебор - 0
score :: Hand -> Int
score h 
  | bust h    = 0
  | otherwise = best h

--По данной руке определить состояние (вызывается после Hit)
eState :: Hand -> PlayerState --не учитывает Stays, по логике вообще не должен вызываться в этом случае
eState hand = if bust hand then Boosted else if twentyOne hand then Blackjack else Plays

--Действие - добор карты
hit :: String -> Game -> Game
hit pid game = 
  let (hand, game') = drawCard game pid
      state         = eState hand
  in setState pid state game'

--Действие - прекратить играть
stay :: String -> Game -> Game
stay pid = setState pid Stays

--Немного геттеров: состояние
getState :: Game -> String -> PlayerState
getState game "dealer" = dealerState game
getState game pid = (playerStates game) Map.! pid

--Имя
getName :: Game -> String -> String
getName game pid = (playerNames game) Map.! pid

--Рука
getHand :: Game -> String -> Hand
getHand game pid = (playerHands game) Map.! pid

--Играет ли еще игрок?
plays :: Game -> String -> Bool 
plays game pid = (getState game pid) == Plays

--Играет ли хоть кто-нибудь?
--(кажется, не понадобилось, сейчас проверка на верхнем уровне)
anybodyPlays :: Game -> Bool
anybodyPlays game = any (plays game) (pids game)

--Пока игроки играют, одна из 2х стартовых карт дилера закрыта
obfuscate :: Hand -> String
obfuscate h = show $ "?" : (map (show) (tail h))

--Отобрать всех, кто еще играет
--(тоже, кажется, не понадобилось)
players :: Game -> [String]
players game = filter (\pid -> plays game pid) (pids game)

--Отображение игры для игрока
showGame :: Game -> String
showGame game =
  let playersInfo = map (\pid -> (getName game pid) ++ " : " ++ show (getHand game pid) ++ " " ++ show (score $ getHand game pid) ++ " - " ++ show (getState game pid)) (pids game)
      dealerInfo  = "Dealer: " ++ (obfuscate $ dealerHand game)
  in "-------\n" ++ (unlines playersInfo) ++ "\n" ++ dealerInfo

--Распарсить действие по строке
parseAction :: String -> Maybe Action
parseAction "Hit" = Just Hit
parseAction "h" = Just Hit
parseAction "Stay" = Just Stay
parseAction "s" = Just Stay
parseAction _ = Nothing

--Запросить действие
askForAction' :: IO Action
askForAction' = do
  putStrLn "Enter your action (\"Hit\", \"h\" - Hit, \"Stay\", \"s\" - Stay."
  input <- getLine
  let pa = parseAction input
  if isJust pa then return (fromJust pa) else askForAction' 

--Вывести состояние игры и запросить действие
--(кажется, не понадобилось)
askForAction :: Game -> String -> IO Action
askForAction game pid = do
  putStrLn $ showGame game
  putStrLn $ "Your hand: " ++ show (getHand game pid) ++ " " ++ show (score $ getHand game pid)
  askForAction'
  
--Применить выбранное действие
applyAction :: Game -> String -> Action -> Game
applyAction game pid Hit  = hit pid game
applyAction game pid Stay = stay pid game

--Информация о руке и состоянии игрока
playerInfo :: Game -> String -> String
playerInfo game pid = "Your hand: " ++ show (getHand game pid) ++ " " ++ show (score $ getHand game pid) ++ " - " ++ show (getState game pid)

--Когда раунды игроков закончились, играет дилер и тут уже его карты открываются
dealerFullInfo :: Game -> String
dealerFullInfo game = "Dealer: " ++ show (dealerHand game) ++ " " ++ show (score $ dealerHand game) ++ " " ++ show (dealerState game)

--Отобрать всех, кто выиграл 1 к 1, то есть набрал больше дилера
winners :: Game -> [String]
winners game = filter (\pid -> ((score $ getHand game pid) > (score $ dealerHand game)) && ((score $ getHand game pid) < 21)) (pids game)

--Отобрать всех, кто выиграл 3 к 2, то есть получил блэкджек (только если дилер его не получил)
blackjackers :: Game -> [String]
blackjackers game = if (dealerState game /= Blackjack) then filter (\pid -> (score $ getHand game pid) == 21) (pids game) else []

--Отобрать тех, кто сыграл вничью (в этом случае они остаются при своих ставках)
pushers :: Game -> [String]
pushers game = filter (\pid -> ((score $ getHand game pid) == (score $ dealerHand game)) && ((score $ getHand game pid) > 0 )) (pids game)

--Подвести итоги игры
--TODO: Я не уверена, что все варианты обрабатываются правильно, нужно проверить.
endGameResults :: Game -> String
endGameResults game = unlines scores ++ "------\n" ++ dInfo ++ bjInfo ++ winsInfo ++ pushInfo ++ addInfo  
  where scores = map (\pid -> (getName game pid) ++ " : " ++ show (score $ getHand game pid)) (pids game)
        dealerScore = show (score $ dealerHand game)
        dInfo = if (dealerState game == Blackjack) then "The dealer has blackjack!\n" else if (dealerState game == Boosted) then "The dealer boosted!\n" else ""
        wins = map (getName game) $ winners game
        pushes = map (getName game) $ pushers game
        bj = map (getName game) $ blackjackers game
        bjInfo = if (bj /= []) then "The following players won 3 to 2 :\n" ++ unlines bj else ""
        winsInfo = if (wins /= []) then "The following players won 1 to 1 :\n" ++ unlines wins else ""
        pushInfo = if (pushes /= []) then "The following players pushed :\n" ++ unlines pushes else ""
        addInfo = if ((wins /= []) || (bj /= [])) then "Congratulations!" else "This time, casino wins."
       
--Только для тестирования не-сетевой версии
interactWithPlayer :: Game -> String -> IO Game
interactWithPlayer game pid = do
  let st = getState game pid
  if (st /= Plays) then return game else do
    act <- askForAction game pid
    let game' = case act of Hit  -> hit pid game
                            Stay -> stay pid game
    putStrLn $ "Your hand: " ++ show (getHand game' pid) ++ " " ++ show (score $ getHand game' pid) ++ " - " ++ show (getState game' pid)
    return game'

--Немного костыль, но иты передавать легче
actionToInt :: Action -> Int
actionToInt Hit = 1
actionToInt Stay = 0

actionFromInt :: Int -> Action
actionFromInt 1 = Hit
actionFromInt 0 = Stay

--Для тестирования
--main :: IO ()
--main = do
--  stdGen <- newStdGen
--  let game = makeGame (Map.fromList [("player1", "Ada"), ("player2", "Sally")]) stdGen
--  putStrLn $ showGame game
--  let game' = stay "player1" game
--  putStrLn $ showGame game'
--  game'' <- interactWithPlayer game' "player2"
--  putStrLn $ showGame game''
