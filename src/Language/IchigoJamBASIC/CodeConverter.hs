{-# LANGUAGE OverloadedStrings #-}

module Language.IchigoJamBASIC.CodeConverter
       ( CodeLine(..)
       , CodeList(..)
       , fromText
       , toText
       , fromBinary
       , toBinary
       ) where


import Data.Word (Word8)
import Data.Int (Int16)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as LBSB
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Control.Applicative ((<|>))
import Control.Monad (replicateM_)
import Data.Monoid ((<>))

import Data.Maybe (fromJust)
import Data.Char (isAscii)
import Numeric (showHex)
import Data.Tuple (swap)

import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

import qualified Data.Attoparsec.Text.Lazy as AP
import Data.Text.Read (decimal)

import Control.Monad.Writer (Writer(..),execWriter,tell)





data CodeLine = CodeLine Int16 BS.ByteString
instance Eq CodeLine where
  (==) (CodeLine x _) (CodeLine y _) = x == y
instance Ord CodeLine where
  compare (CodeLine x _) (CodeLine y _) =  compare x y

type CodeList = [CodeLine]

{--

カナ
http://www.asahi-net.or.jp/~ax2s-kmtn/ref/unicode/uff00.html
http://www.asahi-net.or.jp/~ax2s-kmtn/ref/unicode/u30a0.html

グラフ記号
http://www.asahi-net.or.jp/~ax2s-kmtn/ref/unicode/u25a0.html
http://www.asahi-net.or.jp/~ax2s-kmtn/ref/unicode/u2500.html
http://www.asahi-net.or.jp/~ax2s-kmtn/ref/unicode/u2580.html
--}


-- decode カナ(全角カナに濁点・半濁点も1文字で変換)
-- encode カナ(カタカナひらがなを変換,濁点・半濁点つきの文字は2文字に変換)


-- [#00 - #0F] 特殊絵文字(ASCIIの非表示範囲)
-- [#10 - #1F] 特殊絵文字(ASCIIの非表示範囲)
-- [#20 - #7F] 記号・数値・アルファベット(ASCIIの7bit表示範囲)
-- [#80 - #9F] グラフ記号
-- [#A0 - #DF] カナ
-- [#E0 - #FF] 絵文字

-- 2020/04 までの独自定義
graphLegacy :: [(Word8,Int)]
graphLegacy = [
   (0x80, 0x2003) -- ' '
  ,(0x81, 0x2598) -- '▘'
  ,(0x82, 0x259D) -- '▝'
  ,(0x83, 0x2580) -- '▀'  UPPER HALF
  ,(0x84, 0x2596) -- '▖'
  ,(0x85, 0x258C) -- '▌'  LEFT HALF
  ,(0x86, 0x259E) -- '▞'
  ,(0x87, 0x259B) -- '▛'
  ,(0x88, 0x2597) -- '▗'
  ,(0x89, 0x259A) -- '▚'
  ,(0x8A, 0x2590) -- '▐'  RIGHT HALF
  ,(0x8B, 0x259C) -- '▜'
  ,(0x8C, 0x2584) -- '▄'  LOWER HALF
  ,(0x8D, 0x2599) -- '▙'
  ,(0x8E, 0x259F) -- '▟'
  ,(0x8F, 0x2588) -- '█'

  ,(0x90, 0x30FB) -- '・' -- カナの中黒とおなじ。ほんとは変えたい。
  ,(0x91, 0x2501) -- '━'
  ,(0x92, 0x2503) -- '┃'
  ,(0x93, 0x254B) -- '╋'
  ,(0x94, 0x252B) -- '┫'
  ,(0x95, 0x2523) -- '┣'
  ,(0x96, 0x253B) -- '┻'
  ,(0x97, 0x2533) -- '┳'
  ,(0x98, 0x250F) -- '┏'
  ,(0x99, 0x2513) -- '┓'
  ,(0x9A, 0x2517) -- '┗'
  ,(0x9B, 0x251B) -- '┛'
  ,(0x9C, 0x25E4) -- '◤'
  ,(0x9D, 0x25E5) -- '◥'
  ,(0x9E, 0x25E3) -- '◣'
  ,(0x9F, 0x25E2) -- '◤'
  ]

-- 2020/05 taisukef定義
graph :: [(Word8,Int)]
graph = [
   (0x80, 0x3000) -- '　'
  ,(0x81, 0x2598) -- '▘'
  ,(0x82, 0x259D) -- '▝'
  ,(0x83, 0x2580) -- '▀'  UPPER HALF
  ,(0x84, 0x2596) -- '▖'
  ,(0x85, 0x258C) -- '▌'  LEFT HALF
  ,(0x86, 0x259E) -- '▞'
  ,(0x87, 0x259B) -- '▛'
  ,(0x88, 0x2597) -- '▗'
  ,(0x89, 0x259A) -- '▚'
  ,(0x8A, 0x2590) -- '▐'  RIGHT HALF
  ,(0x8B, 0x259C) -- '▜'
  ,(0x8C, 0x2584) -- '▄'  LOWER HALF
  ,(0x8D, 0x2599) -- '▙'
  ,(0x8E, 0x259F) -- '▟'
  ,(0x8F, 0x2588) -- '█'
   
  ,(0x90, 0x30FB) -- '・' -- カナの中黒とおなじ。
  ,(0x91, 0x2501) -- '━'
  ,(0x92, 0x2503) -- '┃'
  ,(0x93, 0x254B) -- '╋'
  ,(0x94, 0x252B) -- '┫'
  ,(0x95, 0x2523) -- '┣'
  ,(0x96, 0x253B) -- '┻'
  ,(0x97, 0x2533) -- '┳'
  ,(0x98, 0x250F) -- '┏'
  ,(0x99, 0x2513) -- '┓'
  ,(0x9A, 0x2517) -- '┗'
  ,(0x9B, 0x251B) -- '┛'
  ,(0x9C, 0x25E4) -- '◤'
  ,(0x9D, 0x25E5) -- '◥'
  ,(0x9E, 0x25E3) -- '◣'
  ,(0x9F, 0x25E2) -- '◤'
  ]


kanaFullWidth :: [(Word8,Int)]
kanaFullWidth= [
   (0xA0, 0xFFE5) -- '￥'
  ,(0xA1, 0x3002) -- '。'
  ,(0xA2, 0x300C) -- '「'
  ,(0xA3, 0x300D) -- '」'
  ,(0xA4, 0x3001) -- '、'
  ,(0xA5, 0x30FB) -- '・'
  ,(0xA6, 0x30F2) -- 'ヲ'
  ,(0xA7, 0x30A1) -- 'ァ'
  ,(0xA8, 0x30A3) -- 'ィ'
  ,(0xA9, 0x30A5) -- 'ゥ'
  ,(0xAA, 0x30A7) -- 'ェ'
  ,(0xAB, 0x30A9) -- 'ォ'
  ,(0xAC, 0x30E3) -- 'ャ'
  ,(0xAD, 0x30E5) -- 'ュ'
  ,(0xAE, 0x30E7) -- 'ョ'
  ,(0xAF, 0x30C3) -- 'ッ'
   
  ,(0xB0, 0x30FC) -- 'ー'
  ,(0xB1, 0x30A2) -- 'ア'
  ,(0xB2, 0x30A4) -- 'イ'
  ,(0xB3, 0x30A6) -- 'ウ'
  ,(0xB4, 0x30A8) -- 'エ'
  ,(0xB5, 0x30AA) -- 'オ'
  ,(0xB6, 0x30AB) -- 'カ'
  ,(0xB7, 0x30AD) -- 'キ'
  ,(0xB8, 0x30AF) -- 'ク'
  ,(0xB9, 0x30B1) -- 'ケ'
  ,(0xBA, 0x30B3) -- 'コ'
  ,(0xBB, 0x30B5) -- 'サ'
  ,(0xBC, 0x30B7) -- 'シ'
  ,(0xBD, 0x30B9) -- 'ス'
  ,(0xBE, 0x30BB) -- 'セ'
  ,(0xBF, 0x30BD) -- 'ソ'
   
  ,(0xC0, 0x30BF) -- 'タ'
  ,(0xC1, 0x30C1) -- 'チ'
  ,(0xC2, 0x30C4) -- 'ツ'
  ,(0xC3, 0x30C6) -- 'テ'
  ,(0xC4, 0x30C8) -- 'ト'
  ,(0xC5, 0x30CA) -- 'ナ'
  ,(0xC6, 0x30CB) -- 'ニ'
  ,(0xC7, 0x30CC) -- 'ヌ'
  ,(0xC8, 0x30CD) -- 'ネ'
  ,(0xC9, 0x30CE) -- 'ノ'
  ,(0xCA, 0x30CF) -- 'ハ'
  ,(0xCB, 0x30D2) -- 'ヒ'
  ,(0xCC, 0x30D5) -- 'フ'
  ,(0xCD, 0x30D8) -- 'ヘ'
  ,(0xCE, 0x30DB) -- 'ホ'
  ,(0xCF, 0x30DE) -- 'マ'
   
  ,(0xD0, 0x30DF) -- 'ミ'
  ,(0xD1, 0x30E0) -- 'ム'
  ,(0xD2, 0x30E1) -- 'メ'
  ,(0xD3, 0x30E2) -- 'モ'
  ,(0xD4, 0x30E4) -- 'ヤ'
  ,(0xD5, 0x30E6) -- 'ユ'
  ,(0xD6, 0x30E8) -- 'ヨ'
  ,(0xD7, 0x30E9) -- 'ラ'
  ,(0xD8, 0x30EA) -- 'リ'
  ,(0xD9, 0x30EB) -- 'ル'
  ,(0xDA, 0x30EC) -- 'レ'
  ,(0xDB, 0x30ED) -- 'ロ'
  ,(0xDC, 0x30EF) -- 'ワ'
  ,(0xDD, 0x30F3) -- 'ン'
  ,(0xDE, 0x309B) -- '゛'
  ,(0xDF, 0x309C) -- '゜'
  ]


kanaFullWidthMulti :: [([Word8],Int)]
kanaFullWidthMulti= [
   ([0xB6,0xDE], 0x30AC) -- 'ガ'
  ,([0xB7,0xDE], 0x30AE) -- 'ギ'
  ,([0xB8,0xDE], 0x30B0) -- 'グ'
  ,([0xB9,0xDE], 0x30B2) -- 'ゲ'
  ,([0xBA,0xDE], 0x30B4) -- 'ゴ'
  ,([0xBB,0xDE], 0x30B6) -- 'ザ'
  ,([0xBC,0xDE], 0x30B8) -- 'ジ'
  ,([0xBD,0xDE], 0x30BA) -- 'ズ'
  ,([0xBE,0xDE], 0x30BC) -- 'ゼ'
  ,([0xBF,0xDE], 0x30BE) -- 'ゾ'
  ,([0xC0,0xDE], 0x30C0) -- 'ダ'
  ,([0xC1,0xDE], 0x30C2) -- 'ヂ'
  ,([0xC2,0xDE], 0x30C5) -- 'ヅ'
  ,([0xC3,0xDE], 0x30C7) -- 'デ'
  ,([0xC4,0xDE], 0x30C9) -- 'ド'
  ,([0xCA,0xDE], 0x30D0) -- 'バ'
  ,([0xCA,0xDF], 0x30D1) -- 'パ'
  ,([0xCB,0xDE], 0x30D3) -- 'ビ'
  ,([0xCB,0xDF], 0x30D4) -- 'ピ'
  ,([0xCC,0xDE], 0x30D6) -- 'ブ'
  ,([0xCC,0xDF], 0x30D7) -- 'プ'
  ,([0xCD,0xDE], 0x30D9) -- 'ベ'
  ,([0xCD,0xDF], 0x30DA) -- 'ペ'
  ,([0xCE,0xDE], 0x30DC) -- 'ボ'
  ,([0xCE,0xDF], 0x30DD) -- 'ポ'
  ,([0xB3,0xDE], 0x30F4) -- 'ヴ'
  ,([0xDC,0xDE], 0x30F7) -- 'ヷ'
  ,([0xA6,0xDE], 0x30FA) -- 'ヺ'
  ]


emojiLegacy :: [(Word8,Int)]
emojiLegacy = [
   (0xe0,0x2b05)  -- 左矢印 ⬅
  ,(0xe1,0x27a1)  -- 右矢印 ➡
  ,(0xe2,0x2b06)  -- 上矢印 ⬆
  ,(0xe3,0x2b07)  -- 下矢印 ⬇
  ,(0xe4,0x2660)  -- スペード ♠
  ,(0xe5,0x2665)  -- ハート ♥
  ,(0xe6,0x2663)  -- クラブ ♣
  ,(0xe7,0x2666)  -- ダイヤ ♦
  ,(0xe8,0x25cb)  -- 白丸 ○
  ,(0xe9,0x25cf)  -- 黒丸 ●
  ,(0xea,0x2491)  -- 10.
  ,(0xeb,0x1f359) -- おにぎり
  ,(0xec,0x1f431) -- 猫(顔)
  ,(0xed,0x1f47e) -- 宇宙人
  ,(0xee,0x266a)  -- 音符 ♪
  ,(0xef,0x1f365) -- なると
   
  ,(0xf0,0x1f680) -- ロケット
--  ,(0xf1,0x1f6f8) -- UFO flying Saucer
  ,(0xf1,0x76bf)  -- UFO '皿'
  ,(0xf2,0x26a1)  -- 稲妻 ⚡
  ,(0xf3,0x1f681) -- ヘリ
--  ,(0xf4,1f4a5)   -- 衝突 COLLISTION SYMBOL 
  ,(0xf4,0x7206)  -- 衝突 '爆'
--  ,(0xf5,1f62e)   -- 缶 Oil Drum 
  ,(0xf5,0x7f36)  -- 缶 '缶'
  ,(0xf6,0x2709)  -- 封筒 ✉
  ,(0xf7,0x6bb5)  -- 階段 '段'
  ,(0xf8,0x1f6aa) -- ドア
  ,(0xf9,0x4eba)  -- 人 '人'
  ,(0xfa,0x5a01)  -- 人(手足開) '威'
  ,(0xfb,0x4f50)  -- 人(左歩行) '佐'
  ,(0xfc,0x3010)  -- 左枠 '【'
  ,(0xfd,0x4f51)  -- 人(右歩行) '佑'
  ,(0xfe,0x3011)  -- 右枠 '】'
  ,(0xff,0x1f353) -- いちご
  ]

emoji :: [(Word8,Int)]
emoji = [
   (0xe0,0x2190)  -- 左矢印 ←
  ,(0xe1,0x2192)  -- 右矢印 →
  ,(0xe2,0x2191)  -- 上矢印 ↑
  ,(0xe3,0x2193)  -- 下矢印 ↓
  ,(0xe4,0x2660)  -- スペード ♠
  ,(0xe5,0x2665)  -- ハート ♥
  ,(0xe6,0x2663)  -- クラブ ♣
  ,(0xe7,0x2666)  -- ダイヤ ♦
  ,(0xe8,0x26AA)  -- 白丸 ⚪  -- 一旦公式とは逆にしておく
  ,(0xe9,0x26AB)  -- 黒丸 ⚫  -- 一旦公式とは逆にしておく
  ,(0xea,0x1f51f) -- 10.
  ,(0xeb,0x1f359) -- おにぎり
  ,(0xec,0x1f431) -- 猫(顔)
  ,(0xed,0x1f47e) -- 宇宙人
  ,(0xee,0x266a)  -- 音符 ♪
  ,(0xef,0x1f300) -- Cyclone
  ,(0xf0,0x1f680) -- ロケット
  ,(0xf1,0x1f6f8) -- UFO flying Saucer
  ,(0xf2,0x2307)  -- WAVY LINE
  ,(0xf3,0x1f681) -- ヘリ
  ,(0xf4,0x1f4a5) -- 衝突 COLLISTION SYMBOL 
  ,(0xf5,0x1f4b0) -- Money bag
  ,(0xf6,0x1f9f0) -- Toolbox
  ,(0xf7,0x1f4f6) -- Antenna With Bars
  ,(0xf8,0x1f6aa) -- ドア
  ,(0xf9,0x1f574) -- Man In Business Suit Levitating
  ,(0xfa,0x1f57a) -- Man Dancing
  ,(0xfb,0x1f483) -- Dancer
  ,(0xfc,0x1f3cc) -- Golfer
  ,(0xfd,0x1f3c3) -- Runner
  ,(0xfe,0x1f6b6) -- Pedestrain
  ,(0xff,0x1f353) -- Strawberry
  ]

-- [TODO] カナの全角半角選択 -> 一旦全角固定 -> 半角に変更(公式追従 2020/05)
-- [TODO] 絵文字対応         -> 一旦豆腐
-- [TODO] 特殊絵文字対応     -> 一旦豆腐
decode :: BS.ByteString -> LTB.Builder
decode bc = BS.foldl f "" bc
  where
    f :: LTB.Builder -> Word8 -> LTB.Builder
    f xs b = 
      let
        x = fromJust $ decodeVisible7bit b <|> decodeKanaHalfWidth b
                                           <|> decodeGraph b
                                           <|> decodeEmoji b
                                           <|> decodeOther b
      in xs <> x

    decodeVisible7bit :: Word8 -> Maybe LTB.Builder
    decodeVisible7bit b
      | 0x20 <=b && b <= 0x7E = Just $ LTB.singleton $ toEnum $ fromIntegral b
      | otherwise = Nothing
    
    decodeKanaFullWidth :: Word8 -> Maybe LTB.Builder
    decodeKanaFullWidth b = LTB.singleton . toEnum <$> g
      where
        g :: Maybe Int
        g = lookup b kanaFullWidth
        
    decodeKanaHalfWidth :: Word8 -> Maybe LTB.Builder
    decodeKanaHalfWidth b = LTB.singleton . toEnum <$> g
      where
        g :: Maybe Int
        g
          | b == 0xA0              = Just 0xA5
          | 0xA1 <= b && b <= 0xDF = Just $ (fromIntegral b) + (0xFF61-0xA1)
          | otherwise              = Nothing

    decodeGraph :: Word8 -> Maybe LTB.Builder
    decodeGraph b = LTB.singleton . toEnum <$> g
      where
        g :: Maybe Int
        g = lookup b graph

    decodeEmoji :: Word8 -> Maybe LTB.Builder
    decodeEmoji b = LTB.singleton . toEnum <$> g
      where
        g :: Maybe Int
        g = lookup b emoji

    decodeOther :: Word8 -> Maybe LTB.Builder
    decodeOther b = Just $ LTB.singleton '█' -- 豆腐

{--         
    decodeOther :: Word8 -> Maybe LTB.Builder
    decodeOther b =
      -- convert emojis
      let
        hnum = showHex b ""
      in Just $ "(#" <> LTB.fromString hnum <> ")" -- "(#" ++ num ++ ")"
--}


-- [TODO] Either にしてエラー処理
-- [TODO] 変換できない文字 -> エラー? 豆腐? -> 一旦豆腐
-- [TODO] 全角かな対応     -> 1文字濁音は2文字に -> (ひらがなも対応 2020/05)
-- [TODO] 絵文字対応       -> 一旦なし
-- [TODO] 特殊絵文字対応   -> 一旦そのまま
encode :: T.Text -> LBSB.Builder
encode tx = T.foldl f "" tx
  where
    f :: LBSB.Builder -> Char -> LBSB.Builder
    f xs b = 
      let
        x = fromJust $ encodeVisible7bit b <|> encodeKanaFullWidth b <|> encodeKanaFullWidthMulti b
                                           <|> encodeKanaHalfWidth b
                                           <|> encodeGraph b
                                           <|> encodeEmoji b
                                           <|> encodeGraphLegacy b
                                           <|> encodeEmojiLegacy b
                                           <|> encodeOther b
      in xs <> x
    
    encodeVisible7bit :: Char -> Maybe LBSB.Builder
    encodeVisible7bit b
      | isAscii b = Just $ LBSB.char7 b
      | otherwise = Nothing
                    
    encodeKanaFullWidth :: Char -> Maybe LBSB.Builder
    encodeKanaFullWidth b = LBSB.word8 <$> (g $ fromEnum b)
      where
        g :: Int -> Maybe Word8
        g c = lookup c (swap <$> kanaFullWidth)
        
    encodeKanaFullWidthMulti :: Char -> Maybe LBSB.Builder
    encodeKanaFullWidthMulti b = mconcat <$> (map LBSB.word8) <$> (g $ fromEnum b)
      where
        g :: Int -> Maybe [Word8]
        g c = lookup c (swap <$> kanaFullWidthMulti)

    encodeKanaHalfWidth :: Char -> Maybe LBSB.Builder
    encodeKanaHalfWidth b = LBSB.word8 <$> (g $ fromEnum b)
      where
        g :: Int -> Maybe Word8
        g c
          | c == 0xA5                  = Just 0xA0
          | 0xFF61 <= c && c <= 0xFF9F = Just $ fromIntegral $ c - (0xFF61-0xA1)
          | otherwise                  = Nothing

    encodeGraphLegacy :: Char -> Maybe LBSB.Builder
    encodeGraphLegacy b = LBSB.word8 <$> (g $ fromEnum b)
      where
        g :: Int -> Maybe Word8
        g c = lookup c (swap <$> graphLegacy)

    encodeGraph :: Char -> Maybe LBSB.Builder
    encodeGraph b = LBSB.word8 <$> (g $ fromEnum b)
      where
        g :: Int -> Maybe Word8
        g c = lookup c (swap <$> graph)

    encodeEmojiLegacy :: Char -> Maybe LBSB.Builder
    encodeEmojiLegacy b = LBSB.word8 <$> (g $ fromEnum b)
      where
        g :: Int -> Maybe Word8
        g c = lookup c (swap <$> emojiLegacy)

    encodeEmoji :: Char -> Maybe LBSB.Builder
    encodeEmoji b = LBSB.word8 <$> (g $ fromEnum b)
      where
        g :: Int -> Maybe Word8
        g c = lookup c (swap <$> emoji)

    encodeOther :: Char -> Maybe LBSB.Builder
    encodeOther _ = Just $ LBSB.word8 0x8F -- 豆腐





-- [前] 各文字コードから事前に LT.Text に デコード しておく前提
-- attoparsec で 行番号, コード を parse
-- BASICコードをLT.TextからIchigoJamの文字コードに変換(encode)
-- [TODO] check コード行は255文字まで
-- [TODO] コードをトリム?
-- [TODO] endOfInput?
fromText :: LT.Text -> Either String CodeList
fromText xs =
  case AP.parse (AP.many' parseLine) xs of
    AP.Fail _ _ msg -> Left msg
    AP.Done _ cl    -> Right cl
  where
    parseLine :: AP.Parser CodeLine
    parseLine = do
      line <- AP.decimal
      AP.char ' '
      code <- AP.takeWhile1 (not . AP.isEndOfLine)
      AP.endOfLine
      return $ CodeLine line $ LBS.toStrict $ LBSB.toLazyByteString $ encode code



-- [前] 各文字コードには事後に LT.Text から エンコード する前提
-- LTB.Builder を Writer で構築して LT.Text を生成
-- BASICコードをIchigoJamの文字コードからLTB.Builderに変換(decode)
toText :: CodeList -> LT.Text
toText = LTB.toLazyText . execWriter . (mapM_ writeLine)
  where
    writeLine ::  CodeLine -> Writer LTB.Builder ()
    writeLine (CodeLine num line) = do
      tell $ LTB.fromString $ show num
      tell $ LTB.singleton ' '
      tell $ decode line
      tell $ LTB.singleton '\n'



-- Binary.Get で 行番号, コードを parse
-- [TODO] check コード行は255文字まで
-- [TODO] check num > 0
fromBinary :: LBS.ByteString -> Either String CodeList
fromBinary bs =
  case G.runGetOrFail getLines bs of
    Left (_,_,msg) -> Left msg
    Right (_,_,cl) -> Right cl                  
  where
    getLine :: Int16 ->  G.Get CodeLine
    getLine num = do
      len   <- G.getWord8
      bcode <- G.getByteString $ fromIntegral len
      eol   <- G.getWord8
      if eol /= 0 then fail "bad format"
        else
          let bcode' = case BS.last bcode of -- strip NULL on line end
                0 -> BS.init bcode
                _ -> bcode
          in return $ CodeLine num $ bcode'
      
    getLines :: G.Get [CodeLine]
    getLines = do
      e <- G.isEmpty
      if e then return []
        else do
          num <- G.getInt16le
          if num == 0 then return []
            else do
              line  <- getLine num
              lines <- getLines
              return $ line:lines 
      


-- Binary.Put で LBS.ByteString を生成
toBinary :: CodeList -> LBS.ByteString
toBinary xs = P.runPut $ do
  rcount <- sum <$> mapM putLine xs
  replicateM_ (0x400 - rcount) (P.putWord8 0)
  where
    putLine :: CodeLine -> P.PutM Int
    putLine (CodeLine num code) = do
      let
        len = BS.length code
        pad = if even len then 0 else 1 -- 奇数のときNULLで埋める
      P.putInt16le num
      P.putWord8 $ fromIntegral $ len + pad
      P.putByteString code
      replicateM_ (1+pad) (P.putWord8 0) 
      return $ 2 + 1 + len + (1+pad)


