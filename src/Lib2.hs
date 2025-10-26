module Lib2
  ( Command(..)
  , Lib2.examples
  , examplesRecursive
  , parseProgram
  , parseCommand
  , process
  , ToCliCommand(..)
  ) where

import Lib1
import Data.Char (isDigit, isSpace)
import Control.Applicative ((<|>))

------------------------------------------------------------
-- Parser básico
------------------------------------------------------------
newtype Parser a = Parser { runP :: String -> Maybe (a, String) }

orElse :: Parser a -> Parser a -> Parser a
orElse p q = Parser $ \s -> runP p s <|> runP q s

and2 :: Parser a -> Parser b -> Parser (a,b)
and2 pa pb = Parser $ \s -> do
  (a, s1) <- runP pa s
  (b, s2) <- runP pb s1
  return ((a,b), s2)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  (c:cs) | f c -> Just (c, cs)
  _            -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (== c)

manyP :: Parser a -> Parser [a]
manyP p = Parser $ \s -> case runP p s of
  Nothing     -> Just ([], s)
  Just (a,s1) -> case runP (manyP p) s1 of
    Just (as,s2) -> Just (a:as, s2)
    _            -> Just ([a], s1)

token :: Parser a -> Parser a
token p = Parser $ \s -> runP p (dropWhile isSpace s)

symbol :: String -> Parser String
symbol str = token $ Parser $ \s ->
  if take (length str) s == str
    then Just (str, drop (length str) s)
    else Nothing

semicolon, lbrace, rbrace :: Parser Char
semicolon = token (charP ';')
lbrace    = token (charP '{')
rbrace    = token (charP '}')

quotedStringP :: Parser String
quotedStringP = Parser $ \s ->
  let s' = dropWhile isSpace s
  in case s' of
    ('"':xs) -> let (content, rest) = span (/= '"') xs in
                case rest of
                  ('"':r) -> Just (content, r)
                  _       -> Nothing
    _ -> Nothing

------------------------------------------------------------
-- Ejemplos
------------------------------------------------------------
examples :: [Command]
examples =
  [ Enter "Messi"
  , Play "Messi"
      [ Pass "Messi" "Di María"
      , CompositeAction
          [ Dribble "Di María" Success
          , Shot "Di María"
          , Goal "Di María" "Messi"
          ]
      ]
  , ShowStats "Messi"
  , Dump Examples
  ]

examplesRecursive :: [Command]
examplesRecursive =
  [ Enter "Mbappé"
  , Play "Mbappé"
      [ Dribble "Mbappé" Success
      , Shot "Mbappé"
      ]
  , Dump Examples
  ]

------------------------------------------------------------
-- Parsers de acciones (para dentro de Play)
------------------------------------------------------------
parseAction :: Parser Action
parseAction =
  parsePass 
    `orElse` parseDribble 
    `orElse` parseShot 
    `orElse` parseGoal 
    `orElse` parseHardFoul 
    `orElse` parseFoul 
    `orElse` parseSteal 
    `orElse` parseCompositeAction

parseActionSeq :: Parser [Action]
parseActionSeq = Parser $ \s -> runP (manyP parseAction) s

parsePass :: Parser Action
parsePass = Parser $ \s -> do
  ((_, (p1, (_, p2))), rest) <- runP (and2 (symbol "pass") (and2 quotedStringP (and2 (symbol "to") quotedStringP))) s
  (_, after) <- runP semicolon rest
  return (Pass p1 p2, after)

parseDribble :: Parser Action
parseDribble = Parser $ \s -> do
  ((_, player), rest1) <- runP (and2 (symbol "dribble") quotedStringP) s
  (_, rest2) <- runP (symbol "has") rest1
  (result, rest3) <- runP parseDribbleResult rest2
  (_, after) <- runP semicolon rest3
  return (Dribble player result, after)

parseDribbleResult :: Parser Dribbleresult
parseDribbleResult = Parser $ \s ->
  case runP (symbol "success") s of
    Just (_, rest) -> Just (Success, rest)
    Nothing -> case runP (symbol "fail") s of
      Just (_, rest) -> Just (Fail, rest)
      Nothing -> Nothing

parseShot :: Parser Action
parseShot = Parser $ \s -> do
  ((_, p), rest) <- runP (and2 (symbol "shot") quotedStringP) s
  (_, after) <- runP semicolon rest
  return (Shot p, after)

parseGoal :: Parser Action
parseGoal = Parser $ \s -> do
  ((_, (s1, (_, s2))), rest) <- runP (and2 (symbol "goal") (and2 quotedStringP (and2 (symbol "assisted-by") quotedStringP))) s
  (_, after) <- runP semicolon rest
  return (Goal s1 s2, after)

parseFoul :: Parser Action
parseFoul = Parser $ \s -> do
  ((_, a), rest1) <- runP (and2 (symbol "foul") quotedStringP) s
  (_, rest2) <- runP (symbol "on") rest1
  (b, rest3) <- runP quotedStringP rest2
  (_, after) <- runP semicolon rest3
  return (Foul a b, after)

parseHardFoul :: Parser Action
parseHardFoul = Parser $ \s -> do
  ((_, a), rest1) <- runP (and2 (symbol "hardfoul") quotedStringP) s
  (_, rest2) <- runP (symbol "on") rest1
  (b, rest3) <- runP quotedStringP rest2
  (card, rest4) <- runP parseCard rest3
  (_, after) <- runP semicolon rest4
  return (HardFoul a b card, after)

parseCard :: Parser Card
parseCard = Parser $ \s ->
  case runP (symbol "yellow") s of
    Just (_, rest) -> Just (Yellow, rest)
    Nothing -> case runP (symbol "red") s of
      Just (_, rest) -> Just (Red, rest)
      Nothing -> Nothing

parseSteal :: Parser Action
parseSteal = Parser $ \s -> do
  ((_, p), rest) <- runP (and2 (symbol "steal") quotedStringP) s
  (_, after) <- runP semicolon rest
  return (Steal p, after)

parseCompositeAction :: Parser Action
parseCompositeAction = Parser $ \s -> do
  (_, rest1) <- runP (symbol "composite") s
  (_, rest2) <- runP lbrace rest1
  (acts, rest3) <- runP parseActionSeq rest2
  (_, rest4) <- runP rbrace rest3
  (_, after) <- runP semicolon rest4
  return (CompositeAction acts, after)

------------------------------------------------------------
-- Parsers de comandos principales
------------------------------------------------------------
parseEnter, parsePlay, parseSubstitution, parseShowStats, parseShowActive, parseDump :: Parser Command

parseEnter = Parser $ \s -> do
  ((_, name), rest) <- runP (and2 (symbol "enter") quotedStringP) s
  (_, after) <- runP semicolon rest
  return (Enter name, after)

parsePlay = Parser $ \s -> do
  ((_, player), rest1) <- runP (and2 (symbol "play") quotedStringP) s
  (_, rest2) <- runP (symbol "with") rest1
  (_, rest3) <- runP lbrace rest2
  (acts, rest4) <- runP parseActionSeq rest3
  (_, rest5) <- runP rbrace rest4
  (_, after) <- runP semicolon rest5
  return (Play player acts, after)

parseSubstitution = Parser $ \s -> do
  ((_, (a,(_,b))), rest) <- runP (and2 (symbol "substitution") (and2 quotedStringP (and2 (symbol "for") quotedStringP))) s
  (_, after) <- runP semicolon rest
  return (Substitution a b, after)

parseShowStats = Parser $ \s -> do
  ((_, p), rest) <- runP (and2 (symbol "show stats") quotedStringP) s
  (_, after) <- runP semicolon rest
  return (ShowStats p, after)

parseShowActive = Parser $ \s -> do
  (_, rest) <- runP (symbol "show active players") s
  (_, after) <- runP semicolon rest
  return (ShowActivePlayers, after)

parseDump = Parser $ \s -> do
  (_, rest1) <- runP (symbol "dump") s
  (_, rest2) <- runP (symbol "examples") rest1
  (_, after) <- runP semicolon rest2
  return (Dump Examples, after)

------------------------------------------------------------
-- Parser principal
------------------------------------------------------------
parseCommandSingle :: Parser Command
parseCommandSingle =
  parseEnter 
    `orElse` parsePlay 
    `orElse` parseSubstitution 
    `orElse` parseShowStats 
    `orElse` parseShowActive 
    `orElse` parseDump

parseCommandSeq :: Parser [Command]
parseCommandSeq = Parser $ \s -> runP (manyP parseCommandSingle) s

parseProgram :: String -> Maybe [Command]
parseProgram input =
  case runP parseCommandSeq (dropWhile isSpace input) of
    Just (cmds, rest) | all isSpace rest -> Just cmds
    _ -> Nothing

parseCommand :: String -> Either String (Command, String)
parseCommand input =
  case runP parseCommandSingle input of
    Nothing -> Left "unable to parse command"
    Just (cmd, rest) -> Right (cmd, rest)

------------------------------------------------------------
-- Procesador
------------------------------------------------------------
process :: Command -> [String]
process (Dump Examples) =
  ["-- dump examples --"] ++ map toCli Lib2.examples ++ ["-- end dump --"]
process cmd = ["Executed: " ++ toCli cmd]

------------------------------------------------------------
-- Conversión a cadena
------------------------------------------------------------
class ToCliCommand a where
  toCli :: a -> String

toCliCard :: Card -> String
toCliCard Yellow = "yellow"
toCliCard Red    = "red"
  
instance ToCliCommand Command where
  toCli (Enter n) = "enter \"" ++ n ++ "\";"
  toCli (Play n acts) = "play \"" ++ n ++ "\" with { " ++ unwords (map toCli acts) ++ " };"
  toCli (Substitution a b) = "substitution \"" ++ a ++ "\" for \"" ++ b ++ "\";"
  toCli (ShowStats n) = "show stats \"" ++ n ++ "\";"
  toCli ShowActivePlayers = "show active players;"
  toCli (Dump Examples) = "dump examples;"

instance ToCliCommand Action where
  toCli (Pass a b) = "pass \"" ++ a ++ "\" to \"" ++ b ++ "\";"
  toCli (Dribble p r) = "dribble \"" ++ p ++ "\" has " ++  show r ++ ";"
  toCli (Shot p) = "shot \"" ++ p ++ "\";"
  toCli (Goal a b) = "goal \"" ++ a ++ "\" assisted-by \"" ++ b ++ "\";"
  toCli (Foul a b) = "foul \"" ++ a ++ "\" on \"" ++ b ++ "\";"
  toCli (HardFoul a b card) ="hardfoul \"" ++ a ++ "\" on \"" ++ b ++ "\" " ++ toCliCard card ++ ";"
  toCli (Steal p) = "steal \"" ++ p ++ "\";"
  toCli (CompositeAction as) = "composite { " ++ unwords (map toCli as) ++ " };"