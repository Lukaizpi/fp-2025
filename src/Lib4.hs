{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib4 where

import qualified Lib1
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements)

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Except (ExceptT(..), throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace, toLower)
import Data.List (intercalate)

type ErrorMsg = String
type Input = String
type Parser = ExceptT ErrorMsg (State Input)

-- Helpers: run a Parser from a String (for tests/users)
runParser :: Parser a -> Input -> Either ErrorMsg (a, Input)
runParser p inp = 
  case runState (runExceptT p) inp of
    (Left err, _) -> Left err
    (Right val, rest) -> Right (val, rest)

-- Primitive parsers -----------------------------------------------------
parseChar :: Char -> Parser Char
parseChar c = do
  inp <- lift get
  case inp of
    [] -> throwE "Unexpected end of input"
    (x:xs) -> if x == c
                 then lift (put xs) >> return x
                 else throwE $ "Expected '" ++ [c] ++ "' but found '" ++ [x] ++ "'"

parseCIString :: String -> Parser String
parseCIString pat = go pat []
  where
    go [] acc = return (reverse acc)
    go (p:ps) acc = do
      inp <- lift get
      case inp of
        [] -> throwE $ "Unexpected end of input while matching '" ++ pat ++ "'"
        (i:is) -> if toLower p == toLower i
                    then lift (put is) >> go ps (i:acc)
                    else throwE $ "Expected '" ++ [p] ++ "' but found '" ++ [i] ++ "'"

parseWhitespace :: Parser String
parseWhitespace = someChars ' '

someChars :: Char -> Parser String
someChars c = do
  x <- parseChar c
  xs <- manyChars c
  return (x:xs)

manyChars :: Char -> Parser String
manyChars c = choice (someChars c) (return "")

choice :: Parser a -> Parser a -> Parser a
choice p1 p2 = ExceptT $ do
  s <- get
  let (r1, s1) = runState (runExceptT p1) s
  case r1 of
    Right a -> put s1 >> return (Right a)
    Left _  -> do
      let (r2, s2) = runState (runExceptT p2) s
      put s2
      return r2

optionalP :: Parser a -> Parser (Maybe a)
optionalP p = choice (Just <$> p) (return Nothing)

-- many for general parser (simple implementation)
manyP :: Parser a -> Parser [a]
manyP p = choice ((:) <$> p <*> manyP p) (return [])

-- convenience wrappers naming similar to Lib3
many :: Parser a -> Parser [a]
many = manyP

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

parseSpaces :: Parser String
parseSpaces = do
  inp <- lift get
  let (spaces, rest) = span isSpace inp
  lift (put rest)
  return spaces

parseStringValue :: Parser String
-- Accept either a quoted string "like this" or an unquoted identifier (no spaces, no brackets, no commas)
parseStringValue = choice parseQuoted parseUnquoted
  where
    parseQuoted = do
      _ <- parseChar '"'
      s <- parseUntilQuote
      _ <- parseChar '"'
      return s
    parseUnquoted = do
      inp <- lift get
      let (tok, rest) = span (\r -> not (isSpace r) && r /= '[' && r /= ']' && r /= ',' && r /= '"') inp
      case tok of
        [] -> throwE "Expected quoted string or identifier"
        _  -> lift (put rest) >> return tok

parseUntilQuote :: Parser String
parseUntilQuote = do
  inp <- lift get
  case inp of
    [] -> throwE "Expected closing quote"
    ('"':_) -> return ""
    (x:xs) -> lift (put xs) >> (x :) <$> parseUntilQuote

symbol :: String -> Parser String
symbol = parseCIString

between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = do
  _ <- open
  _ <- optionalP parseSpaces
  x <- p
  _ <- optionalP parseSpaces
  _ <- close
  return x

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = choice ((:) <$> p <*> many (sep >> p)) (return [])

-- -------------------- Parsers reusing Lib3 structure --------------------
parseCard :: Parser Lib1.Card
parseCard = choice (const Lib1.Yellow <$> parseCIString "yellow") (const Lib1.Red <$> parseCIString "red")

parseDribbleResult :: Parser Lib1.Dribbleresult
parseDribbleResult = choice (const Lib1.Success <$> parseCIString "success") (const Lib1.Fail <$> parseCIString "fail")

-- Actions
parsePass :: Parser Lib1.Action
parsePass = do
  _ <- parseCIString "pass"
  _ <- parseWhitespace
  p <- parseStringValue
  _ <- parseWhitespace
  _ <- parseCIString "to"
  _ <- parseWhitespace
  q <- parseStringValue
  return $ Lib1.Pass p q

parseDribble :: Parser Lib1.Action
parseDribble = do
  _ <- parseCIString "dribble"
  _ <- parseWhitespace
  p <- parseStringValue
  _ <- parseWhitespace
  _ <- parseCIString "has"
  _ <- parseWhitespace
  r <- parseDribbleResult
  return $ Lib1.Dribble p r

parseFoul :: Parser Lib1.Action
parseFoul = do
  _ <- parseCIString "foul"
  _ <- parseWhitespace
  p <- parseStringValue
  _ <- parseWhitespace
  _ <- parseCIString "on"
  _ <- parseWhitespace
  q <- parseStringValue
  return $ Lib1.Foul p q

parseHardFoul :: Parser Lib1.Action
parseHardFoul = do
  _ <- parseCIString "hardfoul"
  _ <- parseWhitespace
  p1 <- parseStringValue
  _ <- parseWhitespace
  _ <- parseCIString "on"
  _ <- parseWhitespace
  p2 <- parseStringValue
  _ <- parseWhitespace
  c <- parseCard
  return $ Lib1.HardFoul p1 p2 c

parseShot :: Parser Lib1.Action
parseShot = do
  _ <- parseCIString "shot"
  _ <- parseWhitespace
  p <- parseStringValue
  return $ Lib1.Shot p

parseGoal :: Parser Lib1.Action
parseGoal = do
  _ <- parseCIString "goal"
  _ <- parseWhitespace
  p <- parseStringValue
  _ <- parseWhitespace
  _ <- parseCIString "assisted-by"
  _ <- parseWhitespace
  a <- parseStringValue
  return $ Lib1.Goal p a

parseSteal :: Parser Lib1.Action
parseSteal = do
  _ <- parseCIString "steal"
  _ <- parseWhitespace
  p <- parseStringValue
  return $ Lib1.Steal p

parseComposite :: Parser Lib1.Action
parseComposite = do
  _ <- parseCIString "composite"
  _ <- parseWhitespace
  -- separator: optional spaces, optional comma, optional spaces
  let actionSep = do { _ <- optionalP parseSpaces; _ <- optionalP (symbol ","); _ <- optionalP parseSpaces; return () }
  acts <- between (symbol "[") (sepBy parseAction actionSep) (symbol "]")
  return $ Lib1.CompositeAction acts

parseAction :: Parser Lib1.Action
parseAction = choice parseComposite (choice parseGoal (choice parseHardFoul (choice parseDribble (choice parsePass (choice parseFoul (choice parseShot parseSteal))))))

-- Commands
parsePlay :: Parser Lib1.Command
parsePlay = do
  _ <- parseCIString "play"
  _ <- parseWhitespace
  player <- parseStringValue
  _ <- parseWhitespace
  let actionSep = do { _ <- optionalP parseSpaces; _ <- optionalP (symbol ","); _ <- optionalP parseSpaces; return () }
  actions <- between (symbol "[") (sepBy parseAction actionSep) (symbol "]")
  return $ Lib1.Play player actions

parseEnter :: Parser Lib1.Command
parseEnter = do
  _ <- parseCIString "enter"
  _ <- parseWhitespace
  p <- parseStringValue
  return $ Lib1.Enter p

parseSubstitution :: Parser Lib1.Command
parseSubstitution = do
  _ <- parseCIString "substitution"
  _ <- parseWhitespace
  a <- parseStringValue
  _ <- parseWhitespace
  _ <- parseCIString "for"
  _ <- parseWhitespace
  b <- parseStringValue
  return $ Lib1.Substitution a b

parseShowStats :: Parser Lib1.Command
parseShowStats = do
  _ <- parseCIString "showstats"
  _ <- parseWhitespace
  p <- parseStringValue
  return $ Lib1.ShowStats p

parseShowActivePlayers :: Parser Lib1.Command
parseShowActivePlayers = do
  _ <- parseCIString "showactiveplayers"
  return Lib1.ShowActivePlayers

parseDump :: Parser Lib1.Command
parseDump = do
  _ <- parseCIString "dump"
  _ <- parseWhitespace
  _ <- parseCIString "examples"
  return $ Lib1.Dump Lib1.Examples

-- Order matters
parseCommand :: Parser Lib1.Command
parseCommand = choice parseShowActivePlayers (choice parseShowStats (choice parseSubstitution (choice parseDump (choice parsePlay parseEnter))))

-- | This generates arbitrary (a.k.a random) commands for tests.
instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary = elements Lib1.examples