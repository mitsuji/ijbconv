module Main where

import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.IO as LT
import System.IO (stdin,stdout)

import Data.List (sort)
import System.Environment (getArgs)

import Language.IchigoJamBASIC.CodeConverter (KanaDecode(..),fromText,toText,fromBinary,toBinary)



binaryToText :: KanaDecode -> IO ()
binaryToText kd = do
  e <- fromBinary <$> LBS.hGetContents stdin
  case e of
    Left msg -> putStrLn $ "err: " <> msg
    Right cl -> LT.hPutStr stdout $ toText kd $ sort cl

  
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
    "bt":"--kana-decode=katakana-half":_ -> binaryToText KDKatakanaHalfWidth
    "bt":"--kana-decode=katakana-full":_ -> binaryToText KDKatakanaFullWidth
    "bt":"--kana-decode=hiragana-full":_ -> binaryToText KDHiraganaFullWidth
    "bt":_ -> binaryToText KDKatakanaHalfWidth
    "tb":_ -> textToBinary
    _ -> do
      putStrLn "ijbconv 0.5.0.0"
      putStrLn ""
      putStrLn "Usage: ijbconv-exe [command] [option] < [from file] > [to file]"
      putStrLn ""
      putStrLn "Available command:"
      putStrLn "\tbt\tconvert binary to text"
      putStrLn "\ttb\tconvert text to binary"
      putStrLn ""
      putStrLn "Available option:"
      putStrLn "\tbt --kana-decode=katakana-half\tdecode kana as half width katakana"
      putStrLn "\tbt --kana-decode=katakana-full\tdecode kana as full width katakana"
      putStrLn "\tbt --kana-decode=hiragana-full\tdecode kana as full width hiragana"

