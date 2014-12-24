{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, UndecidableInstances #-}
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Concurrent
import Text.Printf
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Monad
import System.Environment
import qualified Data.Map.Strict as Map
import Deck
import Game
import System.Random
--Кажется, все эти import'ы действительно нужны...

--типы сообщений
data Msg = Ping ProcessId | Pong ProcessId | Name ProcessId String | Greetings ProcessId String | Play ProcessId String | Act ProcessId Int | Result ProcessId String | Results ProcessId String | Ask ProcessId String
  deriving (Typeable, Generic)
instance Binary Msg

class (Binary a, Typeable a) => Serializable a
instance (Binary a, Typeable a) => Serializable a


type IDCards = Map.Map ProcessId String
type Results = Map.Map ProcessId Int

hasNames :: Map.Map ProcessId String -> Bool
hasNames ids = all ( /= "") $ Map.elems ids

hadPlayed :: Results -> Bool
hadPlayed ids = all ( > 0) $ Map.elems ids

playerActions :: Process () --
playerActions = do
    m <- expect
    --liftIO (getLine) --подумать
    case m of
        Ping from -> do
            say $ printf "ping received from %s" (show from) --Если пингуют, надо представиться
            mypid <- getSelfPid
            liftIO . putStrLn $ "What's your name?"
            myname <- liftIO (getLine)
            send from (Name mypid myname)
            playerActions
        Greetings from name -> do
            liftIO . putStrLn $ "Greetings, " ++ name ++ "!"
            playerActions
        Play from game -> do --Здесь нужно выдать игроку информацию о состоянии игры
            liftIO . putStrLn $ game
            playerActions
        Ask from st -> do --Спросить о его действиях
            mypid <- getSelfPid
            liftIO . putStrLn $ st
            myaction <- liftIO (askForAction')
            send from (Act mypid $ actionToInt myaction)
            playerActions
        Result from res -> do--Предварительный результат, т.е. рука, состояние, количество набранных очков
            liftIO . putStrLn $ res
            playerActions
        Results from res -> do 
            liftIO . putStrLn $ res
            --Полная информация о набранных игроками и дилером очках
        _ -> playerActions
        

remotable ['playerActions]


master :: [NodeId] -> Process ()
master peers = do
    ps <- forM peers $ \nid -> do
        say $ printf "spawning on %s" (show nid)
        spawn nid $(mkStaticClosure 'playerActions)
    mypid <- getSelfPid
    forM_ ps $ \pid -> do
        say $ printf "pinging %s" (show pid)
        send pid (Ping mypid)
    stdGen <- liftIO (newStdGen)
    waitForNames stdGen $ Map.fromList (map (\pid -> (pid , "")) ps)
      
--Знакомство
waitForNames :: StdGen -> IDCards -> Process ()
waitForNames g ids = 
  if hasNames ids then do --Все поздоровались
    say $ show ids
    let pids = Map.keys ids
    mypid <- getSelfPid
    
    --Создать новый экземпляр игры
    stdGen <- liftIO (newStdGen)
    let names = Map.mapKeys (show) ids
    let game = makeGame names stdGen
    let usersplaying = filter (\pid -> plays game (show pid)) pids
    --say $ show game --Показать всю игру вместе с колодой:)
    --playRound pids usersplaying game 
    playRound pids [] game 
    --waitForResults ps
    return () 
  else do
    m <- expect
    case m of
      Name p name -> do --Здороваемся
        mypid <- getSelfPid
        say $ "Name " ++ name ++ " recieved"
        send p (Greetings mypid name)
        waitForNames g (Map.insert p name ids) --Запомнили имя, знакомимся дальше
      _ -> say "Unexpected message" >> terminate

--Игра
broadcastInfo :: [ProcessId] -> Game -> Process ()
broadcastInfo pids game = do --рассказать всем о состоянии игры
  mypid <- getSelfPid
  forM_ pids $ \pid -> do
    say $ printf "sending game %s" (show pid)
    --Отправляем состояние игры
    send pid (Play mypid (showGame game)) 

--Запросить действия у игроков
askForActions :: [ProcessId] -> Game -> Process ()
askForActions pids game = do
  mypid <- getSelfPid
  let usersplaying = filter (\pid -> plays game (show pid)) pids --отбираем тех, кто играет в новом раунде
  forM_ usersplaying $ \pid -> do
    say $ printf "Asking for actions %s" (show pid)--Спрашиваем их о действиях
    send pid (Ask mypid (playerInfo game (show pid))) 

--Раунд игры
playRound :: [ProcessId] -> [ProcessId] -> Game -> Process ()
playRound allusers [] game = do --раунд окончен
  broadcastInfo allusers game --Отправляем _всем_ текущее состояние игры
  askForActions allusers game
  let usersplaying = filter (\pid -> plays game (show pid)) allusers --отбираем тех, кто играет в новом раунде
  if (anybodyPlays game) then playRound allusers usersplaying game --если такие еще есть - запускаем новый раунд
  else playDealer allusers game --если таких нет - пора играть дилеру
playRound allusers usersplaying game = do
  mypid <- getSelfPid
  m <- expect --ждем сообщения
  case m of
    Act from action -> --если сообщение о действии
      if (from `elem` usersplaying) then do --от того, кто действовать может
          let game' = applyAction game (show from) (actionFromInt action) --применим это действие
          send from (Result mypid (playerInfo game' (show from))) --и отправим ему результат
          playRound allusers (filter (/= from) usersplaying) game' --продолжим раунд, учтя, что он в этом раунде уже не играет
      else
          playRound allusers usersplaying game --от того, кто действовать не может - продолжаем раунд (игнорируем) 
    _ -> playRound allusers usersplaying game --вообще непонятное сообщение - продолжаем раунд (игнорируем)
    
--Ход дилера
playDealer :: [ProcessId] -> Game -> Process () --ход Дилера
playDealer pids game = do
  mypid <- getSelfPid
  forM_ pids $ \pid -> do
    send pid (Result mypid (dealerFullInfo game)) 
  if (dealerState game /= Plays) then
    sendResults pids game
  else do
    let game' = if (17 > (score $ dealerHand game)) then hit "dealer" game else stay "dealer" game --По правилам он обязан брать, пока не наберет 17 и остановиться, как только наберет
    playDealer pids game'

--Результаты игры
--TODO: Сейчас выводятся только общие резульаты, неплохо было бы еще поздравлять игрока с победой лично :)
sendResults :: [ProcessId] -> Game -> Process ()
sendResults pids game = do
  mypid <- getSelfPid
  forM_ pids $ \pid -> do
    send pid (Results mypid (endGameResults game)) 




note = "Incorrect arguments.\n./Main player\n./Main player <port number>\n./Main player <host> <port number>\n./Main dealer\n./Main dealer <port number>\n./Main dealer <host> <port number>\n--default host = 127.0.0.1\n--default port = 4242"
waitMes = "Wait for dealer to start the game."

defaultHost = "127.0.0.1"
defaultPort = "4242"

main = do
    args <- getArgs
    let rtable = Main.__remoteTable initRemoteTable
    case args of
        [ "dealer" ] -> do
         backend <- initializeBackend defaultHost defaultPort rtable
         startMaster backend master
        [ "dealer" , host, port] -> do
         backend <- initializeBackend host port rtable
         startMaster backend master
        [ "dealer", port ] -> do
         backend <- initializeBackend defaultHost port rtable
         startMaster backend master
        [ "player" ] -> do
         backend <- initializeBackend defaultHost defaultPort rtable;
				 putStrLn waitMes
         startSlave backend
        [ "player", port ] -> do
         backend <- initializeBackend defaultHost port rtable;
				 putStrLn waitMes
         startSlave backend
        [ "player", host, port ] -> do
         backend <- initializeBackend host port rtable;
				 putStrLn waitMes
         startSlave backend
        _ -> do putStrLn note
