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


data CodeLine = CodeLine Int16 Txt.Text deriving (Show)
type CodeList = [CodeLine]


decode :: BS.ByteString -> Txt.Text
decode bc = decodeUtf8 $ BS.pack $ foldr f [] $ BS.unpack bc
  where
    f :: Word8 -> [Word8] -> [Word8]
    f b xs = if b <= 0x7f then b : xs
             else
               -- convert emojis
               let
                 num = BS.unpack $ encodeUtf8 $ Txt.pack $ showHex b ""
               in [0x28,0x23] <> num <> [0x29] <> xs -- "(#" ++ num ++ ")"




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
  IO.hPutStr cout (show num) >> IO.hPutChar cout ' ' >> Txt.hPutStrLn cout line




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
    line <- IO.hGetLine cin
    case reads line of
      [] -> return Nothing -- [TODO] throwIO bad format
      (num, code):_ -> 
        return $ Just $ CodeLine num $ Txt.strip $ Txt.pack code


writeBinary :: Handle -> CodeList -> IO ()
writeBinary cout xs = do
  rcount <- sum <$> forM xs (writeBinaryCodeLine cout)
  BS.hPut cout $ BS.pack $ replicate (0x400 - rcount) 0 -- ending of data


writeBinaryCodeLine :: Handle -> CodeLine -> IO Int
writeBinaryCodeLine cout (CodeLine num code) = do
  let
    bcode = encodeUtf8 code -- [TODO] convert emojis
    [low,high] = LBS.unpack $ runPut (putInt16le num)
    len = fromIntegral $ BS.length bcode
    pad = if even len then 0 else 1 -- 奇数のときNULLで埋める
  BS.hPut cout $ BS.pack [low,high]
  BS.hPut cout $ BS.pack [len + (fromIntegral pad)]
  BS.hPut cout bcode
  BS.hPut cout $ BS.pack $ replicate (1+pad) 0
  return $ 2 + 1 + (fromIntegral len) + (1+pad)
