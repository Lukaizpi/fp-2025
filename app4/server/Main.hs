module Main where

import qualified Lib1
import qualified Lib3
import qualified Lib4

import Network.Socket
  ( withSocketsDo
  , AddrInfo(..), AddrInfoFlag(AI_PASSIVE), defaultHints
  , getAddrInfo, socket, addrFamily, addrSocketType, addrProtocol
  , setSocketOption, SocketOption(ReuseAddr)
  , bind, listen, accept, Socket
  , socketToHandle
  )
import Network.Wai (Application, Request, Response, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types.Header (hContentType)
import System.IO (hSetBuffering, BufferMode(..), hGetLine, hPutStrLn, hClose, hFlush, IOMode(ReadWriteMode), stdout)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO, readTVarIO, atomically, writeTVar, readTVar, TVar)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Exception (SomeException, catch)
import Data.Char (isSpace)
import Data.List (intercalate)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting server..."
  hFlush stdout
  chan <- newChan
  _ <- forkIO (Lib3.storageOpLoop chan)
  stVar <- newTVarIO Lib3.emptyState
  _ <- Lib3.load chan stVar
  _ <- forkIO $ let go = do
                      _ <- Lib3.save chan stVar
                      threadDelay (5 * 1000000)
                      go
                  in go
  let port = 4000
  putStrLn $ "Listening on port " ++ show port
  hFlush stdout
  -- Run warp HTTP server with a single POST /cmd endpoint
  let app = makeApp chan stVar
  run port app

makeApp :: Chan Lib3.StorageOp -> TVar Lib3.State -> Application
makeApp chan stVar req respond = do
  -- Only handle POST /cmd, otherwise return 200 empty
  body <- strictRequestBody req
  let input = trim (Data.ByteString.Lazy.Char8.unpack body)
  putStrLn $ "Received HTTP body: " ++ input
  hFlush stdout
  respStr <- case Lib4.runParser Lib4.parseCommand input of
    Left err -> do
      let msg = "ERROR: " ++ err
      putStrLn $ "Parse error, sending: " ++ msg
      hFlush stdout
      return msg
    Right (cmd, rest) ->
      if trim rest /= ""
        then do
          let msg = "ERROR: trailing input: " ++ rest
          putStrLn $ "Trailing input, sending: " ++ msg
          hFlush stdout
          return msg
        else do
          out <- processCommand chan stVar cmd
          putStrLn $ "Sending response: " ++ take 100 out ++ if length out > 100 then "..." else ""
          hFlush stdout
          return out
  respond $ responseLBS status200 [(hContentType, BS8.pack "text/plain; charset=utf-8")] (Data.ByteString.Lazy.Char8.pack respStr)

processCommand :: Chan Lib3.StorageOp -> TVar Lib3.State -> Lib1.Command -> IO String
processCommand chan stVar cmd = case cmd of
  Lib1.ShowActivePlayers -> do
    st <- readTVarIO stVar
    let players = Lib3.getActivePlayers st
    return $ intercalate "\n" ("Active players":players)
  Lib1.ShowStats player -> do
    st <- readTVarIO stVar
    let stats = foldl (Lib3.updateStats player) (Lib3.PlayerStats False 0 0 0 0 0 0 0 0) (Lib3.commands st)
        statsWithActive = stats { Lib3.psActive = player `elem` Lib3.getActivePlayers st }
    return $ unlines [ "Stats for player: " ++ player
                     , "Active: " ++ show (Lib3.psActive statsWithActive)
                     , "Passes: " ++ show (Lib3.psPasses statsWithActive)
                     , "Shots: " ++ show (Lib3.psShots statsWithActive)
                     , "Goals: " ++ show (Lib3.psGoals statsWithActive)
                     , "Fouls: " ++ show (Lib3.psFouls statsWithActive)
                     , "Dribbles: " ++ show (Lib3.psDribbles statsWithActive)
                     , "Steals: " ++ show (Lib3.psSteals statsWithActive)
                     , "Yellow cards: " ++ show (Lib3.psYellow statsWithActive)
                     , "Red cards: " ++ show (Lib3.psRed statsWithActive)
                     ]
  Lib1.Enter p -> do
    r <- atomically $ do
      st <- readTVar stVar
      let active = Lib3.getActivePlayers st
      if p `elem` active
        then return (Left $ "Player " ++ p ++ " is already on the field")
        else do
          let newSt = Lib3.updateStatePure st (Lib1.Enter p)
          writeTVar stVar newSt
          return (Right ())
    case r of
      Left err -> return $ "ERROR: " ++ err
      Right _ -> do
        _ <- Lib3.save chan stVar
        return "OK"
  Lib1.Substitution out inn -> do
    r <- atomically $ do
      st <- readTVar stVar
      let active = Lib3.getActivePlayers st
      if out `notElem` active
        then return (Left $ "Cannot substitute: player " ++ out ++ " is not active")
        else if inn `elem` active
          then return (Left $ "Cannot substitute: " ++ inn ++ " is already playing")
          else do
            let newSt = Lib3.updateStatePure st (Lib1.Substitution out inn)
            writeTVar stVar newSt
            return (Right ())
    case r of
      Left err -> return $ "ERROR: " ++ err
      Right _ -> do
        _ <- Lib3.save chan stVar
        return "OK"
  Lib1.Play _ _ -> do
    atomically $ do
      st <- readTVar stVar
      let newSt = Lib3.updateStatePure st cmd
      writeTVar stVar newSt
    _ <- Lib3.save chan stVar
    return "OK"
  Lib1.Dump _ -> return "OK"