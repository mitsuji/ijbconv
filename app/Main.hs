module Main where

import Language.IchigoJamBASIC.CodeConverter (CodeLine(..),readBinary,writeText,readText,writeBinary)
import System.IO (stdin,stdout)
import Data.List (sortOn)
import System.Environment (getArgs)

-- 念のため アドレス順に並べ替え
sortList :: [CodeLine] -> [CodeLine]
sortList = sortOn $ \(CodeLine num _) -> num

binaryToText :: IO ()
binaryToText = readBinary stdin >>= return . sortList >>= writeText stdout
  
textToBinary :: IO ()
textToBinary = readText stdin >>= return . sortList >>= writeBinary stdout


main :: IO ()
main = do
  args <- getArgs
  case args of
    "bt":_ -> binaryToText
    "tb":_ -> textToBinary
    _ -> binaryToText
  
