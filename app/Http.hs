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

import Language.IchigoJamBASIC.CodeConverter (fromText,toText,fromBinary,toBinary)



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
  (Right M.POST, [_, "bt"]) -> binaryToTextApp req respond -- /ijbconv/bt/{var}
  (Right M.POST, [_, "tb"]) -> textToBinaryApp req respond -- /ijbconv/tb/{var}
  _ -> staticApp req respond -- static html/js/css files


{--
curl -X POST http://localhost:9999/ijbconv/bt -F "binary=@data/test1.bin"
--}
binaryToTextApp :: Wai.Application
binaryToTextApp req respond = do
  (_,fs) <- Parse.parseRequestBody Parse.lbsBackEnd req
  case lookup "binary" fs of
    Nothing -> respond $ Wai.responseLBS H.status500 [("Content-Type","text/plain")] "parameter \"binary\" was not specified.."
    Just fi -> case fromBinary $ Parse.fileContent fi of
      Left msg -> respond $ Wai.responseLBS H.status500 [("Content-Type","text/plain")] $ LT.encodeUtf8 $ "err: " <> (LT.pack msg)
      Right cl -> respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain; charset=UTF-8")] $ LT.encodeUtf8 $ toText $ sort cl


{--
curl -X POST http://localhost:9999/ijbconv/tb --data-urlencode "text@data/test1.txt"
--}
textToBinaryApp :: Wai.Application
textToBinaryApp req respond = do
  (ps,_) <- Parse.parseRequestBody Parse.lbsBackEnd req
  case lookup "text" ps of
    Nothing -> respond $ Wai.responseLBS H.status500 [("Content-Type","text/plain")] "parameter \"text\" was not specified.."
    Just bs -> case fromText $ LT.fromStrict $ T.decodeUtf8 bs of
      Left msg -> respond $ Wai.responseLBS H.status500 [("Content-Type","text/plain")] $ LT.encodeUtf8 $ "err: " <> (LT.pack msg)
      Right cl -> respond $ Wai.responseLBS H.status200 [("Content-Type","application/octet-stream"),("Content-Disposition", "attachment; filename=\"code.bin\"")] $ toBinary $ sort cl


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
    indices = fromJust $ toPieces ["main.html"] -- default content
