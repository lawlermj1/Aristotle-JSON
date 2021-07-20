{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

{- |
   Module      : CassavaUtils
   Description : This module defines common CSV, String, Word8 and Bytestring functions. 
   Copyright   : ( c ) Matthew Lawler 2021   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Site: 
   https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/ 

   Possible to do: 
   Maybe, add in diff functions. 
   Also need a KV Map. 
   Add in UUID = length 18 + enum 

 -}
 
module CassavaUtils 
    (  

      FromField,
      ToField,

      showUTCTimeDate, 
      catchShowIO, 

      ctlChar2SpacesW8, 
      ctlChar2SpacesBS, 

--      isHyphen, 
      c2w, 
      w2c, 
      s2bs, 

      trim, 

     ) where

import Data.Csv  
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Control.Monad 

import qualified Data.ByteString.Lazy as B 
import qualified Data.ByteString.Internal as I 
--pack :: String -> ByteString 
--unpack :: ByteString -> [Char]

import qualified Data.List as L  
import qualified Data.Char as C 
import Data.Word8   
import GHC.Base (unsafeChr) 
import Data.Time 


catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show


instance FromField Bool where
    parseField s
        | s == "false"  = pure False
        | s == "False"  = pure False 
        | s == "0"  = pure False         
        | s == "True"  = pure True                
        | s == "true"  = pure True
        | s == "1"  = pure True 
        | otherwise = mzero 

instance ToField Bool where
    toField False = "False"
    toField True = "True"   

-- ----------------- 
iso_8601_fmt :: String
iso_8601_fmt = "%Y-%m-%dT%H:%M:%S,%q+0000" 

-- takes yyyy-mm-dd from UTCtime for use in csv 
showUTCTimeDate :: UTCTime -> String 
showUTCTimeDate t = L.take 10 (formatTime defaultTimeLocale iso_8601_fmt t)  

-- ----------------- 
isASCIIW8 :: Word8 -> Bool 
isASCIIW8 w = (f > 31) && (f < 126) where f = fromEnum w 

ctlChar2Space :: Word8 -> Word8 
ctlChar2Space w = if isASCIIW8 w then w else _space 

ctlChar2SpacesW8 :: [Word8] -> [Word8] 
ctlChar2SpacesW8 = map ctlChar2Space 

-- Cleans out control chars 
ctlChar2SpacesBS :: B.ByteString -> B.ByteString 
ctlChar2SpacesBS = (B.pack).ctlChar2SpacesW8.(B.unpack) 

removeCtlChars :: [Word8] -> [Word8] 
removeCtlChars = filter (((&&) <$> (> 31) <*> (< 126)) . fromEnum)


c2w :: Char -> Word8
c2w = fromIntegral . C.ord 

w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral 

s2bs :: String -> I.ByteString 
s2bs s = I.packBytes (map c2w s)  

-- not needed 
isHyphen :: Word8 -> Word8 -> Word8 -> Bool 
isHyphen w x y = if w == (c2w '\226') && x == (c2w '\128') && y == (c2w '\148') then True else False 
-- isHyphen w x y = if w == u+00e2 && x == u+0080 && y == u+0094 then True else False 


p2 :: [a] -> a
p2 xs = head (tail xs) 

p3 :: [a] -> a
p3 xs = head (tail (tail xs)) 

t3 :: [a] -> [a] 
t3 = tail.tail.tail 

-- does not catch the case
-- also takes 2 minutes on 25 line file, so it is quite inefficient 
fixCtlChars :: [Word8] -> [Word8] -> [Word8] 
fixCtlChars ws acc 
    | (length ws) == 0 = acc 
    | (length ws) == 1 = acc ++ [ctlChar2Space (head ws)] 
    | ((length ws) > 3) && (isHyphen (head ws) (p2 ws) (p2 ws)) 
       = fixCtlChars (t3 ws) (acc ++ [(c2w '\45')]) 
    | otherwise = fixCtlChars (tail ws) (acc ++ [ctlChar2Space (head ws)]) 


-- [] ws = ws 
-- fixCtlChars [w] ws =  ws ++ [ctlChar2Space w]  
-- fixCtlChars (w:x:y:zs) ws 
--    | isHyphen w x y = fixCtlChars zs (ws ++ [(c2w '\45')]) 
--    | otherwise = fixCtlChars (x:y:zs) (ws ++ [ctlChar2Space w]) 
-- fixCtlChars (w:xs) ws = fixCtlChars xs (ws ++ [ctlChar2Space w]) 

--  bug: how does - becomes â? 
--   PIA_Person—date <> PIA_Personâdate

-- https://stackoverflow.com/questions/1461907/html-encoding-issues-%C3%82-character-showing-up-instead-of-nbsp

--  do a filtered diff between the files? 

-- ----------------- 
trim :: String -> String
trim = trimEnd . trimStart 

-- | Remove spaces from the start of a string, see 'trim'.
trimStart :: String -> String
trimStart = L.dropWhile C.isSpace

-- | Remove spaces from the end of a string, see 'trim'.
trimEnd :: String -> String
trimEnd = L.dropWhileEnd C.isSpace
