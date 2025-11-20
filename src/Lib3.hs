{-# OPTIONS_GHC -Wno-orphans #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1
import Control.Concurrent.STM (TVar, readTVarIO, atomically, readTVar, writeTVar)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Applicative (Alternative(..), (<$>), (<*>), many, some)
import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import Control.Monad (foldM)
import System.Directory (doesFileExist)
import System.IO (writeFile, readFile)
import Control.Exception (try, SomeException)

-- Parser newtype ------------------------------------------------------------

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

-- Functor
instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Left e -> Left e
        Right (a, rest) -> Right (f a, rest)

-- Applicative
instance Applicative Parser where
    pure x = Parser $ \s -> Right (x, s)
    (Parser pf) <*> (Parser pa) = Parser $ \s ->
        case pf s of
            Left e -> Left e
            Right (f, rest) -> case pa rest of
                Left e2 -> Left e2
                Right (a, rest2) -> Right (f a, rest2)

-- Alternative
instance Alternative Parser where
    empty = Parser $ \_ -> Left "empty"
    (Parser p1) <|> (Parser p2) = Parser $ \s ->
        case p1 s of
            Right r -> Right r
            Left _ -> p2 s

-- Basic parser primitives (similar to your Lib2) ---------------------------

type ErrorMsg = String

parseChar :: Char -> Parser Char
parseChar c = Parser $ \s ->
    case s of
        []     -> Left "Unexpected end of input"
        (x:xs) -> if x == c
                     then Right (x, xs)
                     else Left $ "Expected '" ++ [c] ++ "' but found '" ++ [x] ++ "'"

-- Case-insensitive string parser (works like parseCIString)
parseCIString :: String -> Parser String
parseCIString pat = Parser $ \input -> go pat input ""
  where
    go [] remaining matched = Right (reverse matched, remaining)
    go _ [] _ = Left $ "Unexpected end of input while matching '" ++ pat ++ "'"
    go (p:ps) (i:is) matched
      | toLower p == toLower i = go ps is (i:matched)
      | otherwise = Left $ "Expected '" ++ [p] ++ "' but found '" ++ [i] ++ "'"

-- parse whitespace (one or more spaces)
parseWhitespace :: Parser String
parseWhitespace = some (parseChar ' ')

-- quoted string: "...."
parseStringValue :: Parser String
parseStringValue =
    (\_ s _ -> s) <$> parseChar '"' <*> parseUntilQuote <*> parseChar '"'

parseUntilQuote :: Parser String
parseUntilQuote = Parser $ \s -> go s ""
  where
    go [] _ = Left "Expected closing quote"
    go ('"':xs) acc = Right (reverse acc, '"':xs)
    go (x:xs) acc = go xs (x:acc)

symbol :: String -> Parser String
symbol = parseCIString

-- comma separator that optionally consumes a following space
commaSep :: Parser String
commaSep = (\_ m -> fst m) <$> symbol "," <*> optionalWhitespace
  where
    optionalWhitespace = Parser $ \inp -> case runParser (some (parseChar ' ')) inp of
        Right (ws, rest) -> Right ((ws, rest), rest)  -- used only to consume optional spaces
        Left _ -> Right (("", inp), inp)

-- between helper
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = (\_ x _ -> x) <$> open <*> p <*> close

-- sepBy for lists
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

-- optional combinator
optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- Helper: consume trailing spaces (zero or more)
parseSpaces :: Parser String
parseSpaces = Parser $ \s -> Right (span isSpace s)

-- -------------------- DSL-specific parsers (ported from Lib2) --------------

-- The Lib1 types used:
-- Lib1.Action, Lib1.Command, Lib1.Card, Lib1.Dribbleresult

parseCard :: Parser Lib1.Card
parseCard = (const Lib1.Yellow <$> parseCIString "yellow")
        <|> (const Lib1.Red <$> parseCIString "red")

parseDribbleResult :: Parser Lib1.Dribbleresult
parseDribbleResult = (const Lib1.Success <$> parseCIString "success")
                 <|> (const Lib1.Fail <$> parseCIString "fail")

-- Actions

parsePass :: Parser Lib1.Action
parsePass = (\_ _ p _ _ _ q -> Lib1.Pass p q)
            <$> parseCIString "pass" <*> parseWhitespace
            <*> parseStringValue
            <*> parseWhitespace <*> parseCIString "to" <*> parseWhitespace
            <*> parseStringValue

parseDribble :: Parser Lib1.Action
parseDribble = (\_ _ p _ _ _ r -> Lib1.Dribble p r)
               <$> parseCIString "dribble" <*> parseWhitespace
               <*> parseStringValue
               <*> parseWhitespace <*> parseCIString "has" <*> parseWhitespace
               <*> parseDribbleResult

parseFoul :: Parser Lib1.Action
parseFoul = (\_ _ p _ _ _ q -> Lib1.Foul p q)
            <$> parseCIString "foul" <*> parseWhitespace
            <*> parseStringValue
            <*> parseWhitespace <*> parseCIString "on" <*> parseWhitespace
            <*> parseStringValue

parseHardFoul :: Parser Lib1.Action
parseHardFoul = (\_ (_ws1, p1) _ (_ws2, onkw, _ws3, p2) (_ws4, c) -> Lib1.HardFoul p1 p2 c)
            <$> parseCIString "hardfoul"
            <*> ((,) <$> parseWhitespace <*> parseStringValue)
            <*> (pure ()) -- placeholder for grouping
            <*> ((,,,) <$> parseWhitespace <*> parseCIString "on" <*> parseWhitespace <*> parseStringValue)
            <*> ((,) <$> parseWhitespace <*> parseCard)

parseShot :: Parser Lib1.Action
parseShot = (\_ _ p -> Lib1.Shot p) <$> parseCIString "shot" <*> parseWhitespace <*> parseStringValue

parseGoal :: Parser Lib1.Action
parseGoal = (\_ _ p _ _ _ a -> Lib1.Goal p a)
            <$> parseCIString "goal" <*> parseWhitespace
            <*> parseStringValue
            <*> parseWhitespace <*> parseCIString "assisted-by" <*> parseWhitespace
            <*> parseStringValue

parseSteal :: Parser Lib1.Action
parseSteal = (\_ _ p -> Lib1.Steal p) <$> parseCIString "steal" <*> parseWhitespace <*> parseStringValue

parseComposite :: Parser Lib1.Action
parseComposite = (\_ _ acts -> Lib1.CompositeAction acts)
                 <$> parseCIString "composite" <*> parseWhitespace
                 <*> between (symbol "[") (sepBy parseAction (symbol "," *> optional parseSpaces)) (symbol "]")
  where -- sepBy here expects comma (we used symbol "," then optional spaces)
    parseAction = parseComposite
              <|> parseGoal
              <|> parseHardFoul
              <|> parseDribble
              <|> parsePass
              <|> parseFoul
              <|> parseShot
              <|> parseSteal

-- We declare parseAction top-level to reuse in composite; using the same ordering as Lib2:
parseAction :: Parser Lib1.Action
parseAction = parseComposite
          <|> parseGoal
          <|> parseHardFoul
          <|> parseDribble
          <|> parsePass
          <|> parseFoul
          <|> parseShot
          <|> parseSteal

-- Commands

parsePlay :: Parser Lib1.Command
parsePlay = (\_ _ (player, _, acts) -> Lib1.Play player acts)
            <$> parseCIString "play" <*> parseWhitespace
            <*> ((,,) <$> parseStringValue <*> parseWhitespace <*> between (symbol "[") (sepBy parseAction (symbol "," *> optional parseSpaces)) (symbol "]"))

parseEnter :: Parser Lib1.Command
parseEnter = (\_ _ p -> Lib1.Enter p) <$> parseCIString "enter" <*> parseWhitespace <*> parseStringValue

parseSubstitution :: Parser Lib1.Command
parseSubstitution = (\_ (_ws, a) (_g, _ws2, b) -> Lib1.Substitution a b)
                    <$> parseCIString "substitution"
                    <*> ((,) <$> parseWhitespace <*> parseStringValue)
                    <*> ((,,) <$> ((,) <$> parseWhitespace <*> parseCIString "for") <*> parseWhitespace <*> parseStringValue)

parseShowStats :: Parser Lib1.Command
parseShowStats = (\_ _ p -> Lib1.ShowStats p) <$> parseCIString "showstats" <*> parseWhitespace <*> parseStringValue

parseShowActivePlayers :: Parser Lib1.Command
parseShowActivePlayers = const Lib1.ShowActivePlayers <$> parseCIString "showactiveplayers"

parseDump :: Parser Lib1.Command
parseDump = const (Lib1.Dump Lib1.Examples) <$> (parseCIString "dump" *> parseWhitespace *> parseCIString "examples")

-- CRITICAL: Order matters (more specific first)
parseCommand :: Parser Lib1.Command
parseCommand =
       parseShowActivePlayers
  <|>  parseShowStats
  <|>  parseSubstitution
  <|>  parseDump
  <|>  parsePlay
  <|>  parseEnter

-- -------------------- State and persistence --------------------------------

-- Represent the State as a list of authoritative CLI commands (the simplest approach).
-- We keep commands that change the state: Enter, Substitution, Play.
-- For the purpose of this lab this is acceptable and easy to map to/from CLI.
newtype State = State { commands :: [Lib1.Command] }

emptyState :: State
emptyState = State []

-- Pure domain update: given a command and previous state, compute new state.
-- We only store commands that are "mutating" for simplicity.
isMutating :: Lib1.Command -> Bool
isMutating (Lib1.Enter _)         = True
isMutating (Lib1.Substitution _ _) = True
isMutating (Lib1.Play _ _)        = True
isMutating _                      = False

updateStatePure :: State -> Lib1.Command -> State
updateStatePure (State cmds) cmd =
    if isMutating cmd
      then State (cmds ++ [cmd])
      else State cmds

getActivePlayers :: State -> [String]
getActivePlayers (State cmds) = foldl update [] cmds
  where
    update acc (Lib1.Enter p) = acc ++ [p]
    update acc (Lib1.Substitution out inn) = (filter (/= out) acc) ++ [inn]
    update acc _ = acc

isPlayer :: State -> String -> Bool
isPlayer st player = player `elem` getActivePlayers st


-- Ejecuta un comando actualizando el estado y mostrando información si es necesario
-- IMPLEMENTACIÓN CORREGIDA: usamos UNA llamada a atomically que devuelve un resultado puro
-- para imprimir/desplegar después de la transacción.
type ExecOutcome = Either String (Maybe (Either [String] (String, PlayerStats)))
-- Left String => error message to print
-- Right Nothing => no printing required
-- Right (Just (Left [String])) => print list of lines (e.g., active players)
-- Right (Just (Right (player, stats))) => print stats for player

execute :: TVar State -> Lib1.Command -> IO ()
execute stVar cmd = do
    outcome <- atomically $ do
        st <- readTVar stVar
        let active = getActivePlayers st
        case cmd of
          Lib1.Substitution out inn ->
            if out `notElem` active
              then return $ Left $ "Cannot substitute: player " ++ out ++ " is not active"
              else if inn `elem` active
                then return $ Left $ "Cannot substitute: " ++ inn ++ " is already playing"
                else do
                  let newSt = updateStatePure st cmd
                  writeTVar stVar newSt
                  return $ Right Nothing

          Lib1.Enter inn ->
            if inn `elem` active
              then return $ Left $ "Player " ++ inn ++ " is already on the field"
              else do
                let newSt = updateStatePure st cmd
                writeTVar stVar newSt
                return $ Right Nothing

          Lib1.Play _ _ ->
            let newSt = updateStatePure st cmd
            in do writeTVar stVar newSt
                  return $ Right Nothing

          Lib1.ShowActivePlayers ->
            let players = getActivePlayers st
            in return $ Right (Just (Left ("Active players":players)))

          Lib1.ShowStats player ->
            -- compute stats purely from state.commands
            let stats = foldl (updateStats player) (PlayerStats False 0 0 0 0 0 0 0 0) (commands st)
                statsWithActive = stats { psActive = player `elem` getActivePlayers st }
            in return $ Right (Just (Right (player, statsWithActive)))

          _ -> return $ Right Nothing

    case outcome of
      Left err -> putStrLn err
      Right Nothing -> return ()
      Right (Just (Left (hdr:xs))) -> do
          putStrLn hdr
          mapM_ putStrLn xs
      Right (Just (Left [])) -> return ()
      Right (Just (Right (player, stats))) -> printPlayerStats player stats


-- Función showStats que calcula todas las estadísticas de un jugador
-- (ahora showStats no es necesaria para execute; la dejamos para uso externo si se desea)
showStats :: TVar State -> String -> IO ()
showStats stVar player = do
    st <- readTVarIO stVar
    let activePlayersList = getActivePlayers st
        stats = foldl (updateStats player) (PlayerStats False 0 0 0 0 0 0 0 0) (commands st)
        statsWithActive = stats { psActive = player `elem` activePlayersList }
    printPlayerStats player statsWithActive

-- Tipo auxiliar para estadísticas de jugadores
data PlayerStats = PlayerStats
  { psActive    :: Bool
  , psPasses    :: Int
  , psShots     :: Int
  , psGoals     :: Int
  , psFouls     :: Int
  , psDribbles  :: Int
  , psSteals    :: Int
  , psYellow    :: Int
  , psRed       :: Int
  }

updateStats :: String -> PlayerStats -> Lib1.Command -> PlayerStats
updateStats player stats cmd = case cmd of
    Lib1.Play p actions | p == player ->
        foldl updateAction stats actions
    _ -> stats

updateAction :: PlayerStats -> Lib1.Action -> PlayerStats
updateAction stats action = case action of
    Lib1.Pass _ _ -> stats { psPasses = psPasses stats + 1 }
    Lib1.Shot _ -> stats { psShots = psShots stats + 1 }
    Lib1.Goal _ _ -> stats { psGoals = psGoals stats + 1 }
    Lib1.Foul _ _ -> stats { psFouls = psFouls stats + 1 }
    Lib1.Dribble _ _ -> stats { psDribbles = psDribbles stats + 1 }
    Lib1.Steal _ -> stats { psSteals = psSteals stats + 1 }
    Lib1.HardFoul _ _ c -> case c of
        Lib1.Yellow -> stats { psYellow = psYellow stats + 1 }
        Lib1.Red -> stats { psRed = psRed stats + 1 }
    _ -> stats

printPlayerStats :: String -> PlayerStats -> IO ()
printPlayerStats player stats = do
    putStrLn $ "Stats for player: " ++ player
    putStrLn $ "Active: " ++ show (psActive stats)
    putStrLn $ "Passes: " ++ show (psPasses stats)
    putStrLn $ "Shots: " ++ show (psShots stats)
    putStrLn $ "Goals: " ++ show (psGoals stats)
    putStrLn $ "Fouls: " ++ show (psFouls stats)
    putStrLn $ "Dribbles: " ++ show (psDribbles stats)
    putStrLn $ "Steals: " ++ show (psSteals stats)
    putStrLn $ "Yellow cards: " ++ show (psYellow stats)
    putStrLn $ "Red cards: " ++ show (psRed stats)


-- -------------------- Storage operations and loop --------------------------

-- StorageOp: Save <content> <replyChan> | Load <replyChan>
-- Now reply channels carry Either String results so we can report errors.
data StorageOp = Save String (Chan (Either String ())) | Load (Chan (Either String String))

-- This loop runs forever in a dedicated thread.
-- It reads ops from the channel and does file IO accordingly.
-- We catch IO exceptions and report them via the reply channel as Left <err>.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop = do
      op <- readChan chan
      case op of
        Save content reply -> do
          eres <- try (writeFile "fp2025_state.txt" content) :: IO (Either SomeException ())
          case eres of
            Left ex -> writeChan reply (Left $ "Save error: " ++ show ex)
            Right () -> writeChan reply (Right ())
          loop
        Load reply -> do
          eres <- try (doesFileExist "fp2025_state.txt") :: IO (Either SomeException Bool)
          case eres of
            Left ex -> writeChan reply (Left $ "Load error (exists?): " ++ show ex) >> loop
            Right exists ->
              if not exists
                then writeChan reply (Right "") >> loop
                else do
                  r <- try (readFile "fp2025_state.txt") :: IO (Either SomeException String)
                  case r of
                    Left ex -> writeChan reply (Left $ "Load error (read): " ++ show ex)
                    Right content -> writeChan reply (Right content)
                  loop

-- save: read TVar State, convert to CLI lines and ask storage loop to save them
-- RETURNS Either String () to report IO errors
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save chan stVar = do
    st <- readTVarIO stVar
    let linesToWrite = stateToCliLines st
        content = intercalate "\n" linesToWrite
    reply <- newChan
    writeChan chan (Save content reply)
    r <- readChan reply
    return r

-- load: ask storage loop for file contents, parse lines and apply them to an empty state,
-- then write TVar with reconstructed state.
-- RETURNS Either String ()
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load chan stVar = do
    reply <- newChan
    writeChan chan (Load reply)
    r <- readChan reply
    case r of
      Left err -> return $ Left err
      Right content ->
        if null (trim content)
          then atomically (writeTVar stVar emptyState) >> return (Right ())
          else do
            let ls = lines content
                parsed = map parseLineAsCommand ls  -- now pure
            case sequence parsed of
              Left err -> return $ Left err
              Right cmds -> do
                  let newState = foldl updateStatePure emptyState cmds
                  atomically $ writeTVar stVar newState
                  return $ Right ()

-- Helper: parse single line using parseCommand; require complete consumption (or allow trailing spaces)
-- PURE version: returns Either String Lib1.Command
parseLineAsCommand :: String -> Either String Lib1.Command
parseLineAsCommand line =
    case runParser parseCommand (trim line) of
      Left e -> Left $ "Parse error on line: " ++ line ++ " (" ++ e ++ ")"
      Right (cmd, rest) ->
        if all isSpace rest
           then Right cmd
           else Left $ "Trailing input after parse on line: " ++ line ++ " (left: " ++ show rest ++ ")"

-- Convert state to CLI lines (string list)
stateToCliLines :: State -> [String]
stateToCliLines (State cmds) = map commandToCli cmds

-- A local conversion of Lib1.Command -> CLI string
-- This mirrors the ToCliCommand instance from Lab2.
quoteIfNeeded :: String -> String
quoteIfNeeded s = '"' : s ++ "\""

joinActions :: [Lib1.Action] -> String
joinActions [] = ""
joinActions [x] = actionToStr x
joinActions (x:xs) = actionToStr x ++ ", " ++ joinActions xs

actionToStr :: Lib1.Action -> String
actionToStr (Lib1.Pass a b) = "pass " ++ quoteIfNeeded a ++ " to " ++ quoteIfNeeded b
actionToStr (Lib1.Dribble p r) = "dribble " ++ quoteIfNeeded p ++ " has " ++ (case r of Lib1.Success -> "success"; Lib1.Fail -> "fail")
actionToStr (Lib1.Foul a b) = "foul " ++ quoteIfNeeded a ++ " on " ++ quoteIfNeeded b
actionToStr (Lib1.HardFoul a b c) = "hardfoul " ++ quoteIfNeeded a ++ " on " ++ quoteIfNeeded b ++ " " ++ (case c of Lib1.Yellow -> "yellow"; Lib1.Red -> "red")
actionToStr (Lib1.Shot p) = "shot " ++ quoteIfNeeded p
actionToStr (Lib1.Goal a b) = "goal " ++ quoteIfNeeded a ++ " assisted-by " ++ quoteIfNeeded b
actionToStr (Lib1.Steal p) = "steal " ++ quoteIfNeeded p
actionToStr (Lib1.CompositeAction as) = "composite [" ++ joinActions as ++ "]"

commandToCli :: Lib1.Command -> String
commandToCli (Lib1.Play player actions) = "play " ++ quoteIfNeeded player ++ " [" ++ joinActions actions ++ "]"
commandToCli (Lib1.Enter p) = "enter " ++ quoteIfNeeded p
commandToCli (Lib1.Substitution a b) = "substitution " ++ quoteIfNeeded a ++ " for " ++ quoteIfNeeded b
commandToCli (Lib1.ShowStats p) = "showstats " ++ quoteIfNeeded p
commandToCli Lib1.ShowActivePlayers = "showactiveplayers"
commandToCli (Lib1.Dump Lib1.Examples) = "dump examples"

-- Utility: trim spaces
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
