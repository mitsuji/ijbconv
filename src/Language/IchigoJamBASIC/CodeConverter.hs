module Language.IchigoJamBASIC.CodeConverter
       ( CodeLine(..)
       , CodeList(..)
       , readBinary
       , readBinaryCodeLine
       , writeText
       , writeTextCodeLine
       , readText
       , readTextCodeLine
       , writeBinary
       , writeBinaryCodeLine
       ) where


import Data.Word (Word8)
import Data.Int (Int16)
import qualified Data.Text as Txt
import System.IO (Handle)
import qualified System.IO as IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as Txt
import Control.Monad (forM,forM_)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Binary.Get (runGet,getInt16le)
import Data.Binary.Put (runPut,putInt16le)
import Numeric (showHex)
import Text.Read (readMaybe)
import Data.Word8 (_space,_numbersign,_parenleft,_parenright)


data CodeLine = CodeLine Int16 BS.ByteString deriving (Show)
type CodeList = [CodeLine]


decode :: BS.ByteString -> BS.ByteString
decode bc = BS.pack $ foldr f [] $ BS.unpack bc
  where
    f :: Word8 -> [Word8] -> [Word8]
    f b xs = if b <= 0x7f then b : xs
             else
               -- convert emojis
               let
                 num = BS.unpack $ encodeUtf8 $ Txt.pack $ showHex b ""
               in [_parenleft,_numbersign] <> num <> [_parenright] <> xs -- "(#" ++ num ++ ")"




readBinary :: Handle -> IO CodeList
readBinary cin = f []
  where
    f :: [CodeLine] -> IO [CodeLine]
    f xs = do
      mx <- readBinaryCodeLine cin
      case mx of
        Nothing -> return xs
        Just x -> f $ xs <> [x]
      

readBinaryCodeLine :: Handle -> IO (Maybe CodeLine)
readBinaryCodeLine cin = do
  eof <- IO.hIsEOF cin
  if eof then return Nothing -- ending of file
    else do
    idxs <- BS.unpack <$> BS.hGet cin 2
    case idxs of
      [0,0] -> return Nothing -- ending of data
      [low,high] -> do
        [len] <- BS.unpack <$> BS.hGet cin 1
        bcode <- BS.hGet cin $ fromIntegral len
        [eol] <- BS.unpack <$> BS.hGet cin 1
        if eol /= 0 then return Nothing -- [TODO] throwIO bad format
          else do
          let
            bcode' = case BS.last bcode of -- strip NULL on line end
              0 -> BS.init bcode
              _ -> bcode
            num = runGet getInt16le $ LBS.pack [low,high]
          return $ Just $ CodeLine num $ decode bcode'


writeText :: Handle -> CodeList -> IO ()
writeText cout xs = forM_ xs $ writeTextCodeLine cout


writeTextCodeLine :: Handle -> CodeLine -> IO ()
writeTextCodeLine cout (CodeLine num line) =
  IO.hPutStr cout (show num) >> IO.hPutChar cout ' ' >> BS.hPutStr cout line >> IO.hPutChar cout '\n'




readText :: Handle -> IO CodeList
readText cin = f []
  where
    f :: [CodeLine] -> IO [CodeLine]
    f xs = do
      mx <- readTextCodeLine cin
      case mx of
        Nothing -> return xs
        Just x -> f $ xs <> [x]
  
  
readTextCodeLine :: Handle -> IO (Maybe CodeLine)
readTextCodeLine cin = do
  eof <- IO.hIsEOF cin
  if eof then return Nothing -- ending of file
    else do
    line <- BS.unpack <$> BS.hGetLine cin
    let
      (wnum, wcode) = break (==_space) line  -- 10  LED 1
      snum = Txt.unpack $ decodeUtf8 $ BS.pack wnum  -- String of line number
      code = BS.pack $ dropWhile (==_space) wcode -- ByteString of code
    case readMaybe snum of
      Nothing  -> return Nothing -- [TODO] throwIO bad format
      Just num -> return $ Just $ CodeLine num code


writeBinary :: Handle -> CodeList -> IO ()
writeBinary cout xs = do
  rcount <- sum <$> forM xs (writeBinaryCodeLine cout)
  BS.hPut cout $ BS.pack $ replicate (0x400 - rcount) 0 -- ending of data


writeBinaryCodeLine :: Handle -> CodeLine -> IO Int
writeBinaryCodeLine cout (CodeLine num code) = do
  let
    [low,high] = LBS.unpack $ runPut (putInt16le num)
    len = fromIntegral $ BS.length code
    pad = if even len then 0 else 1 -- 奇数のときNULLで埋める
  BS.hPut cout $ BS.pack [low,high]
  BS.hPut cout $ BS.pack [len + (fromIntegral pad)]
  BS.hPut cout code
  BS.hPut cout $ BS.pack $ replicate (1+pad) 0
  return $ 2 + 1 + (fromIntegral len) + (1+pad)
