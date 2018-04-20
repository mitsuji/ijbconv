module Main where

import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.IO as LT
import System.IO (stdin,stdout)

import Data.List (sort)
import System.Environment (getArgs)

import Language.IchigoJamBASIC.CodeConverter (fromText,toText,fromBinary,toBinary)



binaryToText :: IO ()
binaryToText = do
  e <- fromBinary <$> LBS.hGetContents stdin
  case e of
    Left msg -> putStrLn $ "err: " <> msg
    Right cl -> LT.hPutStr stdout $ toText $ sort cl

  
textToBinary :: IO ()
textToBinary = do
  e <- fromText <$> LT.hGetContents stdin
  case e of
    Left msg -> putStrLn $ "err: " <> msg
    Right cl -> LBS.hPutStr stdout $ toBinary $ sort cl
  

main :: IO ()
main = do
  args <- getArgs
  case args of
    "bt":_ -> binaryToText
    "tb":_ -> textToBinary
    _ -> do
      putStrLn "ijbconv 0.4.0.0"
      putStrLn ""
      putStrLn "Usage: ijbconv-exe [command] < [from file] > [to file]"
      putStrLn ""
      putStrLn "Available command:"
      putStrLn "\tbt\tconvert binary to text"
      putStrLn "\ttb\tconvert text to binary"
