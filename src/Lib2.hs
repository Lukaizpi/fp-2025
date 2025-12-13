{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2
    ( parseCommand
    , ToCliCommand(..)
    , process
    ) where

import qualified Lib1
import Data.Char (isSpace, toLower)

-- ========== Exports ==========
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process cmd = ["Parsed as " ++ show cmd]

-- ========== Parser type and helpers ==========

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- combinators
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
    Right r -> Right r
    Left _ -> p2 input

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p input = case p input of
    Left err -> Left err
    Right (v, rest) -> Right (f v, rest)

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input = case p1 input of
    Left err -> Left err
    Right (v1, rest1) -> case p2 rest1 of
        Left err -> Left err
        Right (v2, rest2) -> Right ((v1, v2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 input = case and2 p1 p2 input of
    Left err -> Left err
    Right ((v1, v2), rest) -> case p3 rest of
        Left err -> Left err
        Right (v3, rest2) -> Right ((v1, v2, v3), rest2)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 input = case and3 p1 p2 p3 input of
    Left err -> Left err
    Right ((v1, v2, v3), rest) -> case p4 rest of
        Left err -> Left err
        Right (v4, rest2) -> Right ((v1, v2, v3, v4), rest2)

optional :: Parser a -> Parser (Maybe a)
optional p input = case p input of
    Right (v, rest) -> Right (Just v, rest)
    Left _ -> Right (Nothing, input)

many :: Parser a -> Parser [a]
many p = many' [] where
    many' acc input = case p input of
        Left _ -> Right (reverse acc, input)
        Right (v, rest) -> many' (v:acc) rest

many1 :: Parser a -> Parser [a]
many1 p input = case many p input of
    Left e -> Left e
    Right ([], _) -> Left "At least one required"
    Right r -> Right r

-- basic primitives
parseChar :: Char -> Parser Char
parseChar _ [] = Left "Unexpected end of input"
parseChar c (x:xs) | c == x = Right (c, xs)
                   | otherwise = Left $ "Expected '" ++ [c] ++ "' but found '" ++ [x] ++ "'"

-- Case-insensitive string parser
parseCIString :: String -> String -> Either String (String, String)
parseCIString pattern input = go pattern input ""
  where
    go [] remaining matched = Right (reverse matched, remaining)
    go _ [] _ = Left $ "Unexpected end of input while matching '" ++ pattern ++ "'"
    go (p:ps) (i:is) matched
      | toLower p == toLower i = go ps is (i:matched)
      | otherwise = Left $ "Expected '" ++ [p] ++ "' but found '" ++ [i] ++ "'"

parseWhitespace :: Parser String
parseWhitespace = many1 (parseChar ' ')

-- quoted string
parseStringValue :: Parser String
parseStringValue = pmap (\(_, s, _) -> s) $ 
    and3 (parseChar '"') parseUntilQuote (parseChar '"')

parseUntilQuote :: Parser String
parseUntilQuote [] = Left "Expected closing quote"
parseUntilQuote ('"':xs) = Right ("", '"':xs)
parseUntilQuote (x:xs) = case parseUntilQuote xs of
    Right (str, rest) -> Right (x:str, rest)
    Left err -> Left err

symbol :: String -> Parser String
symbol sym s = parseCIString sym s

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep s = case p s of
    Left _ -> Right ([], s)
    Right (x, rest) -> parseRest [x] rest
  where
    parseRest acc st = case sep st of
      Left _ -> Right (reverse acc, st)
      Right (_, st2) -> case p st2 of
        Left _ -> Left "Expected element after separator"
        Right (y, st3) -> parseRest (y:acc) st3

between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close s = case open s of
  Left e -> Left e
  Right (_, rest) ->
    let rest' = dropWhile (== ' ') rest in
    case p rest' of
      Left e2 -> Left e2
      Right (x, rest2) ->
        let rest2' = dropWhile (== ' ') rest2 in
        case close rest2' of
          Left e3 -> Left e3
          Right (_, rest3) -> Right (x, rest3)

-- ========== DSL-specific parsers ==========

parseCard :: Parser Lib1.Card
parseCard = pmap (const Lib1.Yellow) (parseCIString "yellow")
    `orElse` pmap (const Lib1.Red) (parseCIString "red")

parseDribbleResult :: Parser Lib1.Dribbleresult
parseDribbleResult = pmap (const Lib1.Success) (parseCIString "success")
    `orElse` pmap (const Lib1.Fail) (parseCIString "fail")

parseAction :: Parser Lib1.Action
parseAction = parseComposite
          `orElse` parseGoal
          `orElse` parseHardFoul
          `orElse` parseDribble
          `orElse` parsePass
          `orElse` parseFoul
          `orElse` parseShot
          `orElse` parseSteal

parsePass :: Parser Lib1.Action
parsePass = pmap build $ and4 (parseCIString "pass") parseWhitespace parseStringValue (and4 parseWhitespace (parseCIString "to") parseWhitespace parseStringValue)
  where build (_, _, p1, (_, _, _, p2)) = Lib1.Pass p1 p2

parseDribble :: Parser Lib1.Action
parseDribble = pmap build $ and4 (parseCIString "dribble") parseWhitespace parseStringValue (and4 parseWhitespace (parseCIString "has") parseWhitespace parseDribbleResult)
  where build (_, _, p, (_, _, _, r)) = Lib1.Dribble p r

parseFoul :: Parser Lib1.Action
parseFoul = pmap build $ and4 (parseCIString "foul") parseWhitespace parseStringValue (and4 parseWhitespace (parseCIString "on") parseWhitespace parseStringValue)
  where build (_, _, p1, (_, _, _, p2)) = Lib1.Foul p1 p2

parseHardFoul :: Parser Lib1.Action
parseHardFoul = pmap build $ and4 (parseCIString "hardfoul") (and2 parseWhitespace parseStringValue) (and4 parseWhitespace (parseCIString "on") parseWhitespace parseStringValue) (and2 parseWhitespace parseCard)
  where build (_, (_, p1), (_, _, _, p2), (_, card)) = Lib1.HardFoul p1 p2 card

parseShot :: Parser Lib1.Action
parseShot = pmap (\(_, _, p) -> Lib1.Shot p) $ and3 (parseCIString "shot") parseWhitespace parseStringValue

parseGoal :: Parser Lib1.Action
parseGoal = pmap build $ and4 (parseCIString "goal") parseWhitespace parseStringValue (and4 parseWhitespace (parseCIString "assisted-by") parseWhitespace parseStringValue)
  where build (_, _, s, (_, _, _, a)) = Lib1.Goal s a

parseSteal :: Parser Lib1.Action
parseSteal = pmap (\(_, _, p) -> Lib1.Steal p) $ and3 (parseCIString "steal") parseWhitespace parseStringValue

commaSep :: Parser String
commaSep = pmap fst $ and2 (symbol ",") (optional parseWhitespace)

parseComposite :: Parser Lib1.Action
parseComposite = pmap build $ and3 (parseCIString "composite") parseWhitespace (between (symbol "[") (sepBy parseAction commaSep) (symbol "]"))
  where build (_, _, acts) = Lib1.CompositeAction acts

-- ========== Command parsers ==========

parsePlay :: Parser Lib1.Command
parsePlay = pmap build $ and3 (parseCIString "play") parseWhitespace (and3 parseStringValue parseWhitespace (between (symbol "[") (sepBy parseAction commaSep) (symbol "]")))
  where build (_, _, (player, _, acts)) = Lib1.Play player acts

parseEnter :: Parser Lib1.Command
parseEnter = pmap (\(_, _, p) -> Lib1.Enter p) $ and3 (parseCIString "enter") parseWhitespace parseStringValue

parseSubstitution :: Parser Lib1.Command
parseSubstitution = pmap build $ and3 (parseCIString "substitution") (and2 parseWhitespace parseStringValue) (and3 (and2 parseWhitespace (parseCIString "for")) parseWhitespace parseStringValue)
  where build (_, (_, a), ((_, _), _, b)) = Lib1.Substitution a b

parseShowStats :: Parser Lib1.Command
parseShowStats = pmap (\(_, _, p) -> Lib1.ShowStats p) $ and3 (parseCIString "showstats") parseWhitespace parseStringValue

parseShowActivePlayers :: Parser Lib1.Command
parseShowActivePlayers = pmap (const Lib1.ShowActivePlayers) (parseCIString "showactiveplayers")

parseDump :: Parser Lib1.Command
parseDump = pmap build $ and3 (parseCIString "dump") parseWhitespace (parseCIString "examples")
  where build _ = Lib1.Dump Lib1.Examples

parseCommand :: String -> Either String (Lib1.Command, String)
parseCommand = parseShowActivePlayers 
           `orElse` parseShowStats     
           `orElse` parseSubstitution   
           `orElse` parseDump         
           `orElse` parsePlay         
           `orElse` parseEnter        

-- ========== ToCliCommand instances ==========

class ToCliCommand a where
    toCliCommand :: a -> String

quoteIfNeeded :: String -> String
quoteIfNeeded s = '"':s ++ "\""

instance ToCliCommand Lib1.Command where
    toCliCommand (Lib1.Play player actions) = "play " ++ quoteIfNeeded player ++ " [" ++ joinActions actions ++ "]"
    toCliCommand (Lib1.Enter p) = "enter " ++ quoteIfNeeded p
    toCliCommand (Lib1.Substitution a b) = "substitution " ++ quoteIfNeeded a ++ " for " ++ quoteIfNeeded b
    toCliCommand (Lib1.ShowStats p) = "showstats " ++ quoteIfNeeded p
    toCliCommand Lib1.ShowActivePlayers = "showactiveplayers"
    toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"

joinActions :: [Lib1.Action] -> String
joinActions acts = case acts of
    [] -> ""
    [x] -> actionToStr x
    (x:xs) -> actionToStr x ++ ", " ++ joinActions xs

actionToStr :: Lib1.Action -> String
actionToStr (Lib1.Pass a b) = "pass " ++ quoteIfNeeded a ++ " to " ++ quoteIfNeeded b
actionToStr (Lib1.Dribble p r) = "dribble " ++ quoteIfNeeded p ++ " has " ++ (case r of Lib1.Success -> "success"; Lib1.Fail -> "fail")
actionToStr (Lib1.Foul a b) = "foul " ++ quoteIfNeeded a ++ " on " ++ quoteIfNeeded b
actionToStr (Lib1.HardFoul a b c) = "hardfoul " ++ quoteIfNeeded a ++ " on " ++ quoteIfNeeded b ++ " " ++ (case c of Lib1.Yellow -> "yellow"; Lib1.Red -> "red")
actionToStr (Lib1.Shot p) = "shot " ++ quoteIfNeeded p
actionToStr (Lib1.Goal a b) = "goal " ++ quoteIfNeeded a ++ " assisted-by " ++ quoteIfNeeded b
actionToStr (Lib1.Steal p) = "steal " ++ quoteIfNeeded p
actionToStr (Lib1.CompositeAction as) = "composite [" ++ joinActions as ++ "]"

-- ========== Manual Eq instances ==========

instance Eq Lib1.Card where
    Lib1.Yellow == Lib1.Yellow = True
    Lib1.Red == Lib1.Red = True
    _ == _ = False

instance Eq Lib1.Dribbleresult where
    Lib1.Success == Lib1.Success = True
    Lib1.Fail == Lib1.Fail = True
    _ == _ = False

instance Eq Lib1.Dumpable where
    Lib1.Examples == Lib1.Examples = True

instance Eq Lib1.Action where
    (Lib1.Pass a b) == (Lib1.Pass c d) = a == c && b == d
    (Lib1.Dribble a r1) == (Lib1.Dribble b r2) = a == b && r1 == r2
    (Lib1.Foul a b) == (Lib1.Foul c d) = a == c && b == d
    (Lib1.HardFoul a b c) == (Lib1.HardFoul d e f) = a == d && b == e && c == f
    (Lib1.Shot a) == (Lib1.Shot b) = a == b
    (Lib1.Goal a b) == (Lib1.Goal c d) = a == c && b == d
    (Lib1.Steal a) == (Lib1.Steal b) = a == b
    (Lib1.CompositeAction xs) == (Lib1.CompositeAction ys) = xs == ys
    _ == _ = False

instance Eq Lib1.Command where
    (Lib1.Play p1 a1) == (Lib1.Play p2 a2) = p1 == p2 && a1 == a2
    (Lib1.Enter x) == (Lib1.Enter y) = x == y
    (Lib1.Substitution a b) == (Lib1.Substitution c d) = a == c && b == d
    (Lib1.ShowStats x) == (Lib1.ShowStats y) = x == y
    Lib1.ShowActivePlayers == Lib1.ShowActivePlayers = True
    (Lib1.Dump Lib1.Examples) == (Lib1.Dump Lib1.Examples) = True
    _ == _ = False