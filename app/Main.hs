module Main where

import Control.Exception (catch)
import GHC.Exception (ErrorCall (..))
import GHC.IO.Handle (hFlush)
import Lib (Code (Code), Lang (..), evalCode, help)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (stdout)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> prompt helpMsg
    (a : as) ->
      let l = mapLang a
       in case l of
            Just l -> parseOptions l as
            Nothing -> prompt "Language not supported, exepecting one of { arith | utlc | stlc | sub | sf | dtlc }"

parseOptions :: Lang -> [String] -> IO ()
parseOptions l [] = repl l
parseOptions l (a : args) = case a of
  "-h" -> prompt (help l)
  "--help" -> prompt (help l)
  "-f" -> case args of
    [] -> error "No file specified"
    (x : xs) -> exec l x
  _ -> error "Unknown option"

repl :: Lang -> IO ()
repl l = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    "\\q" -> exitSuccess
    _ -> do
      catch (prompt $ evalCode (Code l input)) handler
      repl l

exec :: Lang -> FilePath -> IO ()
exec l f = do
  input <- readFile f
  prompt $ evalCode (Code l input)

mapLang :: String -> Maybe Lang
mapLang s = case s of
  "arith" -> Just Arith
  "utlc" -> Just UTLC
  "stlc" -> Just STLC
  "sub" -> Just Sub
  "sf" -> Just SF
  "dtlc" -> Just DTLC
  _ -> Nothing

helpMsg :: String
helpMsg =
  unlines
    [ "Usage: pl-demo-exe <lang> [options...]",
      "Options:",
      "  -h, --help        Display help message for the language",
      "  -f, --file <path> Specify the input file, will run repl if not sepcified"
    ]

handler :: ErrorCall -> IO ()
handler = prompt . show

prompt :: String -> IO ()
prompt text = do
  putStrLn text
  hFlush stdout
