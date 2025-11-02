{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1
import Data.Char (isDigit, isAlpha, isSpace, toLower)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- Basic helpers
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- parser combinators
failP :: ErrorMsg -> Parser a
failP msg _ = Left msg

succeed :: a -> Parser a
succeed x s = Right (x, s)

orElse :: Parser a -> Parser a -> Parser a
orElse p q s = case p s of
  Right r -> Right r
  Left _ -> q s

and2 :: Parser a -> Parser b -> Parser (a,b)
and2 p q s = case p s of
  Left err -> Left err
  Right (a, rest) -> case q rest of
    Left err2 -> Left err2
    Right (b, rest2) -> Right ((a,b), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
and3 p q r s = case p s of
  Left e -> Left e
  Right (a, s1) -> case q s1 of
    Left e2 -> Left e2
    Right (b, s2) -> case r s2 of
      Left e3 -> Left e3
      Right (c, s3) -> Right ((a,b,c), s3)

-- primitive parsers
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred [] = Left "Unexpected end of input"
satisfy pred (c:cs) = if pred c then Right (c, cs) else Left $ "Unexpected char: " ++ [c]

char :: Char -> Parser Char
char c = satisfy (== c)

stringP :: String -> Parser String
stringP "" s = Right ("", s)
stringP (x:xs) s = case char x s of
  Left err -> Left err
  Right (_, rest) -> case stringP xs rest of
    Left err2 -> Left err2
    Right (_, rest2) -> Right (x:xs, rest2)

stringCI :: String -> Parser String
stringCI pat s = if map toLower pat `elemPrefixOf` map toLower s
  then Right (take (length pat) s, drop (length pat) s)
  else Left $ "Expected keyword " ++ pat
  where
    elemPrefixOf p xs = map toLower p == take (length p) (map toLower xs)

whitespace :: Parser String
whitespace s = let (sp, rest) = span isSpace s in Right (sp, rest)

-- identifiers and strings
quotedString :: Parser String
quotedString s = case dropWhile isSpace s of
  ('"':rest) -> parseInside rest
  _ -> Left "Expected quoted string"
  where
    parseInside inp = let (content, rest) = break (== '"') inp in
      case rest of
        ('"':r) -> Right (content, r)
        _ -> Left "Unterminated quoted string"

bareIdent :: Parser String
bareIdent s = let s' = dropWhile isSpace s in
  case s' of
    [] -> Left "Expected identifier"
    _ -> let (tok, rest) = span isIdentChar s' in
      if null tok then Left "Expected identifier" else Right (tok, rest)
  where isIdentChar c = not (isSpace c) && not (c `elem` "[],\"()")

stringVal :: Parser String
stringVal s = case quotedString s of
  Right r -> Right r
  Left _ -> bareIdent s

symbol :: String -> Parser String
symbol sym s = case stringP sym (dropWhile isSpace s) of
  Right _ -> Right (sym, dropWhile isSpace $ drop (length sym) (dropWhile isSpace s))
  Left _ -> Left $ "Expected symbol " ++ sym

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
  Right (_, rest) -> case p rest of
    Left e2 -> Left e2
    Right (x, rest2) -> case close rest2 of
      Left e3 -> Left e3
      Right (_, rest3) -> Right (x, rest3)

-- DSL parsers
parseCard :: Parser Lib1.Card
parseCard s = let s' = dropWhile isSpace s in
  (case stringCI "yellow" s' of
    Right _ -> Right (Lib1.Yellow, drop (length "yellow") s')
    Left _ -> case stringCI "red" s' of
      Right _ -> Right (Lib1.Red, drop (length "red") s')
      Left _ -> Left "Expected card (yellow|red)")

parseDribbleResult :: Parser Lib1.Dribbleresult
parseDribbleResult s = let s' = dropWhile isSpace s in
  (case stringCI "success" s' of
    Right _ -> Right (Lib1.Success, drop (length "success") s')
    Left _ -> case stringCI "fail" s' of
      Right _ -> Right (Lib1.Fail, drop (length "fail") s')
      Left _ -> Left "Expected dribble result (success|fail)")

parseAction :: Parser Lib1.Action
parseAction s =
  ( parsePass
    `orElse` parseDribble
    `orElse` parseFoul
    `orElse` parseHardFoul
    `orElse` parseShot
    `orElse` parseGoal
    `orElse` parseSteal
    `orElse` parseComposite
  ) s
  where
    parsePass st = case stringCI "pass" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (p1, r2) <- stringVal (dropWhile isSpace r1)
        (_, r3) <- stringCI "to" (dropWhile isSpace r2)
        (p2, r4) <- stringVal (dropWhile isSpace r3)
        Right (Lib1.Pass p1 p2, r4)

    parseDribble st = case stringCI "dribble" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (p, r2) <- stringVal (dropWhile isSpace r1)
        (_, r3) <- stringCI "has" (dropWhile isSpace r2)
        (res, r4) <- parseDribbleResult (dropWhile isSpace r3)
        Right (Lib1.Dribble p res, r4)

    parseFoul st = case stringCI "foul" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (p1, r2) <- stringVal (dropWhile isSpace r1)
        (_, r3) <- stringCI "on" (dropWhile isSpace r2)
        (p2, r4) <- stringVal (dropWhile isSpace r3)
        Right (Lib1.Foul p1 p2, r4)

    parseHardFoul st = case stringCI "hardfoul" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (p1, r2) <- stringVal (dropWhile isSpace r1)
        (_, r3) <- stringCI "on" (dropWhile isSpace r2)
        ((p2, c), r4) <- and2 stringVal parseCard (dropWhile isSpace r3)
        Right (Lib1.HardFoul p1 p2 c, r4)

    parseShot st = case stringCI "shot" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (p, r2) <- stringVal (dropWhile isSpace r1)
        Right (Lib1.Shot p, r2)

    parseGoal st = case stringCI "goal" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (scorer, r2) <- stringVal (dropWhile isSpace r1)
        (_, r3) <- stringCI "assisted-by" (dropWhile isSpace r2)
        (assist, r4) <- stringVal (dropWhile isSpace r3)
        Right (Lib1.Goal scorer assist, r4)

    parseSteal st = case stringCI "steal" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (p, r2) <- stringVal (dropWhile isSpace r1)
        Right (Lib1.Steal p, r2)

    parseComposite st = case stringCI "composite" (dropWhile isSpace st) of
      Left _ -> Left ""
      Right (_, r1) -> do
        (acts, r2) <- between (symbol "[") (sepBy parseAction (symbol ",")) (symbol "]") (dropWhile isSpace r1)
        Right (Lib1.CompositeAction acts, r2)

-- Commands
parsePlay :: Parser Lib1.Command
parsePlay s = case stringCI "play" (dropWhile isSpace s) of
  Left _ -> Left ""
  Right _ -> let after = drop (length "play") (dropWhile isSpace s) in
    case and2 stringVal (between (symbol "[") (sepBy parseAction (symbol ",")) (symbol "]")) after of
      Left _ -> Left "Malformed play command"
      Right ((player, actions), rest) -> Right (Lib1.Play player actions, rest)

parseEnter :: Parser Lib1.Command
parseEnter s = case stringCI "enter" (dropWhile isSpace s) of
  Left _ -> Left ""
  Right _ -> let after = drop (length "enter") (dropWhile isSpace s) in
    case stringVal after of
      Left _ -> Left "Malformed enter"
      Right (p, rest) -> Right (Lib1.Enter p, rest)

parseSubstitution :: Parser Lib1.Command
parseSubstitution s = case stringCI "substitution" (dropWhile isSpace s) of
  Left _ -> Left ""
  Right _ -> let after = drop (length "substitution") (dropWhile isSpace s) in
    case and2 stringVal stringVal after of
      Left _ -> Left "Malformed substitution"
      Right ((p1,p2), rest) -> Right (Lib1.Substitution p1 p2, rest)

parseShowStats :: Parser Lib1.Command
parseShowStats s = case stringCI "showstats" (dropWhile isSpace s) of
  Left _ -> Left ""
  Right _ -> let after = drop (length "showstats") (dropWhile isSpace s) in
    case stringVal after of
      Left _ -> Left "Malformed showstats"
      Right (p, rest) -> Right (Lib1.ShowStats p, rest)

parseShowActivePlayers :: Parser Lib1.Command
parseShowActivePlayers s = case stringCI "showactiveplayers" (dropWhile isSpace s) of
  Left _ -> Left ""
  Right _ -> Right (Lib1.ShowActivePlayers, drop (length "showactiveplayers") (dropWhile isSpace s))

parseDump :: Parser Lib1.Command
parseDump s = case stringCI "dump" (dropWhile isSpace s) of
  Left _ -> Left ""
  Right _ -> let after = drop (length "dump") (dropWhile isSpace s) in
    case stringCI "examples" after of
      Left _ -> Left "Malformed dump"
      Right _ -> Right (Lib1.Dump Lib1.Examples, drop (length "examples") after)

-- | Parses user's input.
parseCommand :: Parser Lib1.Command
parseCommand input = let s = dropWhile isSpace input in
  case (parsePlay `orElse` parseEnter `orElse` parseSubstitution `orElse` parseShowStats
        `orElse` parseShowActivePlayers `orElse` parseDump) s of
    Left _ -> Left "Unable to parse command"
    Right (cmd, rest) ->
      if null (trim rest) then Right (cmd, "") else Left $ "Trailing input: " ++ rest

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

quoteIfNeeded :: String -> String
quoteIfNeeded s = if any isSpace s || null s then '"':s ++ "\"" else s

instance ToCliCommand Lib1.Command where
  toCliCommand (Lib1.Play player actions) = "play " ++ quoteIfNeeded player ++ " [" ++ joinActions actions ++ "]"
  toCliCommand (Lib1.Enter p) = "enter " ++ quoteIfNeeded p
  toCliCommand (Lib1.Substitution a b) = "substitution " ++ quoteIfNeeded a ++ " " ++ quoteIfNeeded b
  toCliCommand (Lib1.ShowStats p) = "showstats " ++ quoteIfNeeded p
  toCliCommand Lib1.ShowActivePlayers = "showactiveplayers"
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"

joinActions :: [Lib1.Action] -> String
joinActions acts = concat $ intersperse ", " (map actionToStr acts)
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

actionToStr :: Lib1.Action -> String
actionToStr (Lib1.Pass a b) = "pass " ++ quoteIfNeeded a ++ " to " ++ quoteIfNeeded b
actionToStr (Lib1.Dribble p r) = "dribble " ++ quoteIfNeeded p ++ " has " ++ drStr r
  where drStr Lib1.Success = "success"; drStr Lib1.Fail = "fail"
actionToStr (Lib1.Foul a b) = "foul " ++ quoteIfNeeded a ++ " on " ++ quoteIfNeeded b
actionToStr (Lib1.HardFoul a b c) = "hardfoul " ++ quoteIfNeeded a ++ " on " ++ quoteIfNeeded b ++ " " ++ cardStr c
  where cardStr Lib1.Yellow = "yellow"; cardStr Lib1.Red = "red"
actionToStr (Lib1.Shot p) = "shot " ++ quoteIfNeeded p
actionToStr (Lib1.Goal a b) = "goal " ++ quoteIfNeeded a ++ " assisted-by " ++ quoteIfNeeded b
actionToStr (Lib1.Steal p) = "steal " ++ quoteIfNeeded p
actionToStr (Lib1.CompositeAction as) = "composite [" ++ joinActions as ++ "]"

-- Eq instances
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
