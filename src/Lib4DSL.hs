{-# LANGUAGE DeriveFunctor #-}
module Lib4DSL
  ( ClientF(..)
  , Client(..)
  , enter
  , substitution
  , play
  , showStats
  , showActivePlayers
  , dumpExamples
  , interpretLocal
  , interpretHTTP
  ) where

import qualified Lib1
import qualified Lib3
import qualified Lib2
import Data.List (intercalate)
import Control.Monad (ap)
import Control.Monad.Trans.State.Strict (State, get, put)
import Network.HTTP.Client (newManager, defaultManagerSettings, parseRequest, httpLbs, requestBody, requestHeaders, RequestBody(RequestBodyLBS), responseBody)
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8
import Control.Monad.IO.Class (liftIO)

-- | DSL functor: one constructor per Lib1.Command
data ClientF next
  = EnterF String next
  | SubstitutionF String String next
  | PlayF String [Lib1.Action] next
  | ShowStatsF String (String -> next)
  | ShowActivePlayersF (String -> next)
  | DumpExamplesF (String -> next)
  deriving Functor

-- Minimal Free monad implementation
data Client a = Pure a | Free (ClientF (Client a))

instance Functor Client where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free op) = Free (fmap (fmap f) op)

instance Applicative Client where
  pure = Pure
  (<*>) = ap

instance Monad Client where
  return = Pure
  Pure a >>= k = k a
  Free op >>= k = Free (fmap (>>= k) op)

-- Smart constructors
enter :: String -> Client String
enter p = Free (EnterF p (Pure "OK"))

substitution :: String -> String -> Client String
substitution out inn = Free (SubstitutionF out inn (Pure "OK"))

play :: String -> [Lib1.Action] -> Client String
play player actions = Free (PlayF player actions (Pure "OK"))

showStats :: String -> Client String
showStats p = Free (ShowStatsF p Pure)

showActivePlayers :: Client String
showActivePlayers = Free (ShowActivePlayersF Pure)

dumpExamples :: Client String
dumpExamples = Free (DumpExamplesF Pure)

-- | Interpret the DSL locally using Lib3.State-like structure.
-- Returns the string that would be returned by the server.
interpretLocal :: Client a -> State Lib3.State a
interpretLocal (Pure a) = return a
interpretLocal (Free op) = case op of
  EnterF p next -> do
    st <- get
    let active = Lib3.getActivePlayers st
    if p `elem` active
      then interpretLocal next -- return "OK" is embedded; we ignore duplicate semantics here
      else do
        let newSt = Lib3.updateStatePure st (Lib1.Enter p)
        put newSt
        interpretLocal next

  SubstitutionF out inn next -> do
    st <- get
    let active = Lib3.getActivePlayers st
    if out `notElem` active || inn `elem` active
      then interpretLocal next
      else do
        let newSt = Lib3.updateStatePure st (Lib1.Substitution out inn)
        put newSt
        interpretLocal next

  PlayF player actions next -> do
    st <- get
    let newSt = Lib3.updateStatePure st (Lib1.Play player actions)
    put newSt
    interpretLocal next

  ShowStatsF player k -> do
    st <- get
    let stats = foldl (Lib3.updateStats player) (Lib3.PlayerStats False 0 0 0 0 0 0 0 0) (Lib3.commands st)
        statsWithActive = stats { Lib3.psActive = player `elem` Lib3.getActivePlayers st }
        out = unlines [ "Stats for player: " ++ player
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
    interpretLocal (k out)

  ShowActivePlayersF k -> do
    st <- get
    let players = Lib3.getActivePlayers st
        out = intercalate "\n" ("Active players":players)
    interpretLocal (k out)

  DumpExamplesF k -> do
    -- Use examples from Lib1
    interpretLocal (k (intercalate "\n" (map Lib2.toCliCommand Lib1.examples)))

-- | Interpret the DSL by calling the remote server over HTTP.
-- The server expects a plain-text body containing a ToCliCommand string and replies with plain text.
interpretHTTP :: Client a -> IO a
interpretHTTP c = do
  manager <- newManager tlsManagerSettings
  go manager c
  where
    go _ (Pure a) = return a
    go mgr (Free op) = case op of
      EnterF p next -> doSend mgr (Lib2.toCliCommand (Lib1.Enter p)) >> go mgr next
      SubstitutionF out inn next -> doSend mgr (Lib2.toCliCommand (Lib1.Substitution out inn)) >> go mgr next
      PlayF player acts next -> doSend mgr (Lib2.toCliCommand (Lib1.Play player acts)) >> go mgr next
      ShowStatsF player k -> do
        res <- doSend mgr (Lib2.toCliCommand (Lib1.ShowStats player))
        go mgr (k res)
      ShowActivePlayersF k -> do
        res <- doSend mgr (Lib2.toCliCommand Lib1.ShowActivePlayers)
        go mgr (k res)
      DumpExamplesF k -> do
        res <- doSend mgr (intercalate "\n" (map Lib2.toCliCommand Lib1.examples))
        go mgr (k res)

    doSend mgr txt = do
      initReq <- parseRequest "http://127.0.0.1:4000/cmd"
      let req = initReq { requestBody = RequestBodyLBS (Data.ByteString.Lazy.Char8.pack txt)
                        , requestHeaders = [(hContentType, BS8.pack "text/plain; charset=utf-8")]
                        }
      resp <- httpLbs req mgr
      return (Data.ByteString.Lazy.Char8.unpack (responseBody resp))
