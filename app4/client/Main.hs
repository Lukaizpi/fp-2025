module Main where

import System.Environment (getArgs)
import qualified Lib4DSL as DSL
import qualified Lib3
import qualified Lib1
import qualified Lib4
import Control.Monad.Trans.State.Strict (runState)
import Data.Maybe (fromMaybe)
import System.IO (hPutStrLn, stderr)
import Data.Foldable (asum)

-- Sample DSL program: enter Neymar, play a pass, then show stats
sampleProgram :: DSL.Client String
sampleProgram = do
  _ <- DSL.enter "Neymar"
  _ <- DSL.play "Neymar" [Lib1.Pass "Neymar" "Ronaldo"]
  DSL.showStats "Neymar"

-- Convert a parsed Lib1.Command to a DSL Client program that returns a String
commandToClient :: Lib1.Command -> DSL.Client String
commandToClient cmd = case cmd of
  Lib1.Enter p -> DSL.enter p
  Lib1.Substitution out inn -> DSL.substitution out inn
  Lib1.Play player acts -> DSL.play player acts
  Lib1.ShowStats p -> DSL.showStats p
  Lib1.ShowActivePlayers -> DSL.showActivePlayers
  Lib1.Dump Lib1.Examples -> DSL.dumpExamples

runCommandsLocal :: [Lib1.Command] -> IO ()
runCommandsLocal cmds = do
  let go [] _ = return ()
      go (c:cs) st = do
        let client = commandToClient c
            (res, st') = runState (DSL.interpretLocal client) st
        putStrLn res
        go cs st'
  go cmds Lib3.emptyState

runCommandsHTTP :: [Lib1.Command] -> IO ()
runCommandsHTTP cmds = mapM_ (\rc -> do
  res <- DSL.interpretHTTP (commandToClient rc)
  putStrLn res) cmds

parseLines :: [String] -> IO [Lib1.Command]
parseLines ls = do
  let nonEmpty = filter (not . all (`elem` " \t\r\n")) ls
  fmap concat $ mapM parseOne nonEmpty
  where
    parseOne l = case Lib4.runParser Lib4.parseCommand l of
      Left err -> do
        hPutStrLn stderr $ "Parse error for line: " ++ show l ++ " -> " ++ err
        return []
      Right (cmd, rest) -> if all (`elem` " \t\r\n") rest
        then return [cmd]
        else do
          hPutStrLn stderr $ "Trailing input after parse for line: " ++ show l ++ " -> " ++ show rest
          return []

main :: IO ()
main = do
  args <- getArgs
  -- parse flags: --interpreter <local|http>, --script <file>, --cmd "<command>"
  let getFlag name = case dropWhile (/= name) args of
                        (_:val:_) -> Just val
                        _ -> Nothing
      interp = fromMaybe "local" (asum [getFlag "--interpreter", getFlag "-i"])
      mScript = case dropWhile (/= "--script") args of
                  (_:f:_) -> Just f
                  _ -> Nothing
      mCmd = case dropWhile (/= "--cmd") args of
               (_:c:_) -> Just c
               _ -> Nothing

  -- Build command list
  cmds <- case (mScript, mCmd) of
    (Just f, _) -> do
      content <- readFile f
      parseLines (lines content)
    (_, Just c) -> parseLines [c]
    _ -> return [Lib1.Enter "Neymar", Lib1.Play "Neymar" [Lib1.Pass "Neymar" "Ronaldo"], Lib1.ShowStats "Neymar"]

  putStrLn $ "Running with interpreter: " ++ interp
  case interp of
    "local" -> runCommandsLocal cmds
    "http" -> runCommandsHTTP cmds
    _ -> putStrLn "Unknown interpreter. Use --interpreter http|local or -i"
