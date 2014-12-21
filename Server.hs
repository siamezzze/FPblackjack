{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
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
--Предлагаю по возможности все игровое взаимодействие сделать в модуле Game (нынешний Main) и здесь импортировать его
--import Game

--типы сообщений
data Msg = Ping ProcessId | Pong ProcessId | Name ProcessId String | Greetings ProcessId String | Result ProcessId Int | Act ProcessId Int | Play ProcessId
  deriving (Typeable, Generic)
instance Binary Msg

class (Binary a, Typeable a) => Serializable a
instance (Binary a, Typeable a) => Serializable a

data Action = Hit | Stay deriving (Eq, Read)

type IDCards = Map.Map ProcessId String
type Results = Map.Map ProcessId Int

hasNames :: Map.Map ProcessId String -> Bool
hasNames ids = all ( /= "") $ Map.elems ids

hadPlayed :: Results -> Bool
hadPlayed ids = all ( > 0) $ Map.elems ids

playerActions :: Process ()
playerActions = do
    liftIO . putStrLn $ "1"
    m <- expect
    liftIO . putStrLn $ "2"
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
        Play from -> do
            liftIO . putStrLn $ "We're starting the game! (without you)"
            --Здесь нужно выдать игроку информацию о состоянии игры и спросить о его действиях
        --Result from res -> do
            --Предварительный результат, т.е. количество набранных очков
        --Results from res -> do 
            --Полная информация о набранных игроками и дилером очках
        

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
    waitForNames $ Map.fromList (map (\pid -> (pid , "")) ps)
    
--Для образца
waitForPongs :: [ProcessId] -> Process ()
waitForPongs [ ] = return ()
waitForPongs ps = do
    m <- expect
    case m of
      Pong p -> waitForPongs (filter (/= p) ps)
      Name p name -> do 
        mypid <- getSelfPid
        say $ "Name" ++ name ++ "recieved"
        send p (Greetings mypid name)
      _ -> say "MASTER received ping" >> terminate

--Знакомство
waitForNames :: IDCards -> Process ()
waitForNames ids = 
  if hasNames ids then do --Все поздоровались
    say $ show ids
    let ps = Map.keys ids
    mypid <- getSelfPid
    --Создать новый экземпляр игры
    forM_ ps $ \pid -> do
        say $ printf "triggering game %s" (show pid)
        --Здесь отправлять нужно полностью состояние игры либо руки дилера и игрока
        send pid (Play mypid) 
    --waitForResults ps
    return () 
  else do
    m <- expect
    case m of
      Name p name -> do --Здороваемся
        mypid <- getSelfPid
        say $ "Name " ++ name ++ " recieved"
        send p (Greetings mypid name)
        waitForNames (Map.insert p name ids) --Запомнили имя, знакомимся дальше
      _ -> say "Unexpected message" >> terminate

--собственно, основная серверная часть - ожидание действий игроков и реакция на них
waitForResults :: Results -> Process ()
waitForResults res = 
  if hadPlayed res then do --Все уже на(до-)игрались
    say $ show res
    --здесь дилер тянет карты
    --проверяем результаты, выдаем их игрокам
    return () 
  else do
    m <- expect
    case m of
      Act playerAction name -> do 
        mypid <- getSelfPid
        undefined
        --обработать действие. Если Hit, выдать карту (либо обновленное состояние игры) и проверить буст
        --Stay или уже проиграл - запустить waitForResults с занесенными туда результатами
      _ -> say "Unexpected message" >> terminate

defaultHost = "127.0.0.1"
defaultPort = "4242"

main = do
    args <- getArgs
    let rtable = Main.__remoteTable initRemoteTable
    case args of
        [ "master" ] -> do
         backend <- initializeBackend defaultHost defaultPort rtable
         startMaster backend master
        [ "master", port ] -> do
         backend <- initializeBackend defaultHost port rtable
         startMaster backend master
        [ "slave" ] -> do
         backend <- initializeBackend defaultHost defaultPort rtable
         startSlave backend
        [ "slave", port ] -> do
         backend <- initializeBackend defaultHost port rtable
         startSlave backend
        [ "slave", host, port ] -> do
         backend <- initializeBackend host port rtable
         startSlave backend
