{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import Data.List (sort)
import System.Environment (getArgs)

import Data.String (fromString)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Parse
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.Wai.Application.Static as Static
import Data.Maybe (fromJust)
import WaiAppStatic.Types (toPieces)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Archive.Zip as Zip
import Data.Either (partitionEithers)

import Language.IchigoJamBASIC.CodeConverter (KanaDecode(..),fromText,toText,fromBinary,toBinary)



main :: IO ()
main = do
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ routerApp


routerApp :: Wai.Application
routerApp req respond = case Wai.pathInfo req of
  "ijbconv" : _ -> jibconvApp req respond --[TODO] catch exception
  _ -> staticApp  req respond -- static html/js/css files


jibconvApp :: Wai.Application
jibconvApp req respond = case (M.parseMethod (Wai.requestMethod req), Wai.pathInfo req) of
  (Right M.POST, [_, "bt"])  -> binaryToTextApp req respond -- /ijbconv/bt
  (Right M.POST, [_, "tb"])  -> textToBinaryApp req respond -- /ijbconv/tb
  (Right M.POST, [_, "mbtz"]) -> multipleBinaryToTextZipApp req respond -- /ijbconv/mbtz
  _ -> staticApp req respond -- static html/js/css files


{--
curl -X POST http://localhost:9999/ijbconv/bt -F "binary=@data/test1.bin"
--}
binaryToTextApp :: Wai.Application
binaryToTextApp req respond = do
  (_,fs) <- Parse.parseRequestBody Parse.lbsBackEnd req
  case lookup "binary" fs of
    Nothing -> respond $ response500 "parameter \"binary\" was not specified.."
    Just fi -> case fromBinary $ Parse.fileContent fi of
      Left msg -> respond $ response500 $ LT.encodeUtf8 $ "err: " <> (LT.pack msg)
      Right cl -> respond $ responseTxt $ LT.encodeUtf8 $ toText KDKatakanaHalfWidth $ sort cl


{--
curl -X POST http://localhost:9999/ijbconv/tb --data-urlencode "text@data/test1.txt"
--}
textToBinaryApp :: Wai.Application
textToBinaryApp req respond = do
  (ps,_) <- Parse.parseRequestBody Parse.lbsBackEnd req
  case lookup "text" ps of
    Nothing -> respond $ response500 "parameter \"text\" was not specified.."
    Just bs -> case fromText $ LT.fromStrict $ T.decodeUtf8 bs of
      Left msg -> respond $ response500 $ LT.encodeUtf8 $ "err: " <> (LT.pack msg)
      Right cl -> respond $ responseBin "code.bin" $ toBinary $ sort cl


{--
curl -X POST http://localhost:9999/ijbconv/mbtz -F "binary=@data/test1.bin" -F "binary=@data/test2.bin"
--}
multipleBinaryToTextZipApp :: Wai.Application
multipleBinaryToTextZipApp req respond = do
  (_,fs) <- Parse.parseRequestBody Parse.lbsBackEnd req
  case map snd $ filter (\x -> fst x == "binary") fs of
    [] -> respond $ response500 "parameter \"binary\" was not specified.."
    fs' -> do
      es <- binariesToTexts KDKatakanaHalfWidth fs'
      case partitionEithers es of
        ([],ts) -> respond $ responseBin "ijbtext.zip" $ zipArchive ts
        (es,_)  -> respond $ response500 "err: something wrong" -- [TODO] error message
  where

    -- [TODO] ファイル名に日付
    binariesToTexts :: KanaDecode -> [Parse.FileInfo LBS.ByteString] -> IO [Either (T.Text,String) (T.Text,LT.Text)]
    binariesToTexts kd = mapM f
      where
        f :: Parse.FileInfo LBS.ByteString -> IO (Either (T.Text,String) (T.Text,LT.Text))
        f (Parse.FileInfo fn _ fc) = case fromBinary fc of
          Left msg -> return $ Left  (T.decodeUtf8 fn, msg)
          Right cl -> return $ Right (T.decodeUtf8 fn, toText kd $ sort cl)

    -- [TODO] ファイル日付
    zipArchive :: [(T.Text,LT.Text)] -> LBS.ByteString
    zipArchive = Zip.fromArchive . (foldr f Zip.emptyArchive)
      where
        f :: (T.Text,LT.Text) -> Zip.Archive -> Zip.Archive
        f (p,c) =
          let
            p' = T.unpack $ "ijbtext/" <> (T.takeWhile ('.'/=) p) <> ".txt"
            c' = LT.encodeUtf8 c
          in Zip.addEntryToArchive $ Zip.toEntry p' 0 c'


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
    indices = fromJust $ toPieces ["main.html"] -- default content



response500 :: LBS.ByteString -> Wai.Response
response500 = Wai.responseLBS H.status500 [("Content-Type","text/plain")]

responseTxt :: LBS.ByteString -> Wai.Response
responseTxt = Wai.responseLBS H.status200 [("Content-Type","text/plain; charset=UTF-8")]

responseBin :: BS.ByteString -> LBS.ByteString -> Wai.Response
responseBin fn =
  let
    ct = ("Content-Type","application/octet-stream")
    cd = ("Content-Disposition","attachment; filename=\"" <> fn <> "\"")
  in Wai.responseLBS H.status200 [ct,cd]
