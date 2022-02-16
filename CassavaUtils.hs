{-# LANGUAGE FlexibleInstances #-}

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
   Add in UUID = L.length 18 + enum 

Add logging 
   -- https://stackoverflow.com/questions/41655218/printing-timestamps-while-debugging-in-haskell

 -}
 
module CassavaUtils 
    (  

      CV.FromField,
      CV.ToField,
      decodeByNameOK, 
      decodeByNameEither, 

      s2bs, 
      bs2s, 
      ctlChar2SpacesBS, 

      lc, 
      trim, 
      isBracket, 
      noquotes, 
      char2Word8, 
      isHyphen, 
      ctlChar2SpacesW8, 

      showUTCTimeDate, 
      epochUTCTime, 
      messageTime, 

      sortFoldlTupleAny,

      rmdups, 
      rmdupkvs, 
      freq, 
--      cartProd, 
--      evenOddSplit, 
--      take2, 
--      lasthead, 
--      makepairs, 
      halfCartProd, 

     ) where

import qualified Data.Csv as CV 
import Control.Exception (IOException)
import qualified Control.Exception as Exception

-- P for Prelude 
import GHC.Base as P 
import qualified Data.List as L  
import qualified Data.Ord as O   
import qualified Data.Time as T 
import qualified Data.Vector as V 
import qualified Data.Set as S 
import qualified Control.Monad as CM  

-- -----------------
-- String <-> ByteString
import qualified Data.Char as C 
import qualified Data.Word8 as W8 
import Data.ByteString.Lazy as BL 
import Data.ByteString.Internal as I 
-- packChars :: [Char] -> ByteString 
-- unpackChars :: ByteString -> [Char]
-- packBytes :: [Word8] -> ByteString
-- unpackBytes :: ByteString -> [Word8]

-- ----------------- 
-- Cassava 
-- parsing input 
--decodeByName :: FromNamedRecord a => ByteString -> Either String (Header, Vector a)  
decodeByNameEither :: CV.FromNamedRecord a => BL.ByteString -> Either String [a] 
decodeByNameEither fnr = 
     case (CV.decodeByName fnr) of 
       Left err -> Left err  
       Right (_, v) -> Right (V.toList v) 

decodeByNameOK :: CV.FromNamedRecord a => BL.ByteString -> [a] 
decodeByNameOK fnr = 
     case (CV.decodeByName fnr) of 
       Left err -> [] 
       Right (_, v) -> V.toList v 

instance CV.FromField Bool where
    parseField bs
        | s == "false"  = pure False
        | s == "False"  = pure False 
        | s == "0"  = pure False         
        | s == "True"  = pure True                
        | s == "true"  = pure True
        | s == "1"  = pure True 
        | otherwise = mzero 
        where s = bs2s bs         

instance CV.ToField Bool where
    toField False = s2bs "False"
    toField True = s2bs "True"   

-- needed for Cassava type checking 
-- never going to do this, and this will trigger an error if used 
instance CV.FromField [String] where
    parseField s = mzero 

-- awful, but it works when needed for dumping output for unit testing 
instance CV.ToField [String] where
    toField ss = s2bs (L.intercalate ", " ss) 

-- kv pairs 
-- never going to do this, and this will trigger an error if used 
instance CV.FromField [(String,Int)] where 
    parseField s = mzero 

-- awful, but it works when needed for dumping output for unit testing 
instance CV.ToField [(String,Int)] where 
    toField ss = s2bs (L.intercalate ", " (L.map (\(s,i) -> "("++ s ++", "++ show i ++")" ) ss))  

-- ----------------- 
-- essential for converting a sum type to ToField e.g. 
-- instance ToField XYZ where toField a = (s2bs.show) a 
-- packChars :: [Char] -> ByteString 
s2bs :: String -> I.ByteString 
s2bs = packChars 
--    I.packBytes (L.map char2Word8 s) 

-- inverse 
-- unpackChars :: ByteString -> [Char]
bs2s :: I.ByteString -> String
bs2s = unpackChars 

-- based on c2w in Data.ByteString.Internal 
char2Word8 :: Char -> W8.Word8
char2Word8 = fromIntegral . C.ord 

-- ----------------- 
-- Cleans out control chars 
-- used when reading JSON full of windows junk chars 
ctlChar2SpacesBS :: BL.ByteString -> BL.ByteString 
ctlChar2SpacesBS = (BL.pack).ctlChar2SpacesW8.(BL.unpack) 

-- in a string, converts all junk to spaces, and retains ASCII chars 
ctlChar2SpacesW8 :: [W8.Word8] -> [W8.Word8] 
ctlChar2SpacesW8 = L.map ctlChar2Space 

-- Convert a non-ASCII char to a space; clobbers junk chars 
ctlChar2Space :: W8.Word8 -> W8.Word8 
ctlChar2Space w = if isASCIIW8 w then w else W8._space 

-- is this an valid ASCII char? 
isASCIIW8 :: W8.Word8 -> Bool 
isASCIIW8 w = (f > 31) && (f < 126) where f = fromEnum w 

-- is this a non alpha char? 
isBracket :: Char -> Bool 
isBracket c = (c == '(') || (c == ')') || (c == ',') || (c == '/') || (c == '-') 

-- is this a double quote? 
isQuote :: Char -> Bool 
isQuote c = (c == '"') 

noquotes :: String -> String 
noquotes s = L.filter (not.isQuote) s 

-- ----------------- 
-- encoding dead ends 
-- not used as this a replace with space approach is used above. 
removeCtlChars :: [W8.Word8] -> [W8.Word8] 
removeCtlChars = L.filter (((&&) <$> (> 31) <*> (< 126)) . fromEnum)

-- possible inverse function 
w2c :: W8.Word8 -> Char
w2c = unsafeChr . fromIntegral 

-- not used as there are many weird windows chars. So all are replaced with a space above.  
isHyphen :: W8.Word8 -> W8.Word8 -> W8.Word8 -> Bool 
isHyphen w x y = if w == (char2Word8 '\226') && x == (char2Word8 '\128') && y == (char2Word8 '\148') then True else False 
-- isHyphen w x y = if w == u+00e2 && x == u+0080 && y == u+0094 then True else False 

-- also takes 2 minutes on 25 line file, so it is quite inefficient 
fixCtlChars :: [W8.Word8] -> [W8.Word8] -> [W8.Word8] 
fixCtlChars ws acc 
    | (L.length ws) == 0 = acc 
    | (L.length ws) == 1 = acc ++ [ctlChar2Space (L.head ws)] 
    | ((L.length ws) > 3) && (isHyphen (L.head ws) (p2 ws) (p2 ws)) 
       = fixCtlChars (t3 ws) (acc ++ [(char2Word8 '\45')]) 
    | otherwise = fixCtlChars (L.tail ws) (acc ++ [ctlChar2Space (L.head ws)]) 

-- [] ws = ws 
-- fixCtlChars [w] ws =  ws ++ [ctlChar2Space w]  
-- fixCtlChars (w:x:y:zs) ws 
--    | isHyphen w x y = fixCtlChars zs (ws ++ [(char2Word8 '\45')]) 
--    | otherwise = fixCtlChars (x:y:zs) (ws ++ [ctlChar2Space w]) 
-- fixCtlChars (w:xs) ws = fixCtlChars xs (ws ++ [ctlChar2Space w]) 

--  bug: how does - becomes â? 
--   PIA_Person—date <> PIA_Personâdate

-- https://stackoverflow.com/questions/1461907/html-encoding-issues-%C3%82-character-showing-up-instead-of-nbsp

-- ----------------- 
-- fixing strings 
lc :: (Show a) => a -> String 
lc a = L.map C.toLower (show a) 

trim :: String -> String
trim = trimEnd . trimStart 

-- | Remove spaces from the start of a string, see 'trim'.
trimStart :: String -> String
trimStart = L.dropWhile C.isSpace 

-- | Remove spaces from the end of a string, see 'trim'.
trimEnd :: String -> String
trimEnd = L.dropWhileEnd C.isSpace 

-- ----------------- 
-- time 
iso_8601_fmt :: String
iso_8601_fmt = "%Y-%m-%dT%H:%M:%S,%q+0000" 

-- takes yyyy-mm-dd from UTCtime for use in csv 
-- toss away time component 
showUTCTimeDate :: T.UTCTime -> String 
showUTCTimeDate t = L.take 10 (T.formatTime T.defaultTimeLocale iso_8601_fmt t)  

--    Default for time types   
--    "1-Jan-70" 
--    showGregorian epochDay = 1970-01-01 
--    The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
--    Complexity O ( 1 )    
epochDay :: T.Day 
epochDay = T.fromGregorian 1970 1 1 

--    Day and time from midnight (up to 86401 seconds)
--    Complexity O ( 1 ) 
epochUTCTime :: T.UTCTime 
epochUTCTime = T.parseTimeOrError True T.defaultTimeLocale "%-d-%b-%y" "1-Jan-70" 

-- formatTime :: FormatTime t => TimeLocale -> String -> t -> String
makeTime :: IO String 
makeTime = T.formatTime T.defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> T.getZonedTime

messageTime :: String -> IO String  
messageTime m = (liftM2 (++) (pure m ) makeTime ) 

-- ----------------- 
-- forming relations - adjuncts? 
--    callable by a fold to accumulate lists with the same key value 
--    works best if the ( k,_ ) is sorted 
--    Complexity O( n ) where n = L.length acc  
foldlTuple :: ( Eq k ) => [( k,[v] )] -> ( k,v ) -> [( k,[v] )] 
foldlTuple acc ( k,v ) 
--    | trkce ( "tF " ++ show s ++ " " ++ show x ++ " "  ) False = undefined 
--    make L.last safe, and create first acc value 
    | L.length acc == 0 = [( k,[v] )] 
--    if previous key is the same, append to L.last acc value  
    | fst la == k = ( ( initSafe acc ) ++ [( fst la,( snd la ++ [v] ) )] ) 
--    else start new key 
    | otherwise = ( acc ++ [( k,[v] )] ) 
    where la = L.last acc 

--    List 
--  creates a list of (key, value list) tuples 
--    L.foldl wrapper 
--    e.g. FROM L.filter ( /= ( "",[0] ) ) ( L.foldl foldlTuple [( "",[0] )] tBL1 ) 
--    need to L.filter out defaults kDef vDef 
--    nub removes duplicates 
--    Complexity O( n^2 + 3n ) where n = L.length ts   
foldlTupleWrap :: ( Eq k, Eq v, Ord v ) => k -> v -> [( k,v )] -> [( k,[v] )] 
foldlTupleWrap kDef vDef ts 
    = L.map ( \( k,vs ) -> ( k, L.nub vs ) ) ( L.filter ( /= ( kDef,[vDef] ) ) ( L.foldl foldlTuple [( kDef,[vDef] )] ts ) ) 

--    Ord 
--    sort, fold and tuple up pattern for any type  ( L.filter ( ( /= [] ).snd ) 
--    Complexity O( n*log n ) where n = L.length ts   
sortFoldlTupleAny :: ( Ord k, Ord v ) => k -> v -> [( k,v )] -> [( k,[v] )] 
sortFoldlTupleAny kDef vDef ts 
    = foldlTupleWrap kDef vDef ( L.sortBy ( O.comparing fst ) ts ) 

-- ----------------- 
-- Remove duplicates by using Set 
-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
-- member :: Ord a => a -> Set a -> Bool
-- insert :: Ord a => a -> Set a -> Set a 
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' S.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if S.member b a
    then rmdups' a c
    else b : rmdups' (S.insert b a) c    

-- remove duplicate keys from kv pairs 
-- fst :: (a, b) -> a 
-- carries keys in ks 
rmdupkvs :: (Ord k, Ord v) => [(k,v)] -> [(k,v)] 
rmdupkvs = rmdupkvs' S.empty S.empty where 
  rmdupkvs' _ _ [] = [] 
  rmdupkvs' kvs ks (kv : kvsr) = if S.member (fst kv) ks  
    then rmdupkvs' kvs ks kvsr
    else kv : (rmdupkvs' (S.insert kv kvs) (S.insert (fst kv) ks) kvsr) 

-- gets frequency of common elements 
freq :: (Eq a, Ord a) => [a] -> [(a,Int)]
freq s = L.map (\x -> (L.head x, L.length x)) . L.group . L.sort $ s 

-- takes 2 and return pairs ; carry around head until at end   
take2 :: [a] -> [(a,a)] -> [(a,a)]  
take2 xs ps 
  | L.length xs == 0 = ps 
  | L.length xs == 1 = ps 
  | L.length xs == 2 = ((L.head xs, L.last xs):ps)      
  | otherwise = take2 (L.tail xs) ((L.head xs, (L.head (L.tail xs))):ps)  

-- loop around lasthead 
lasthead :: [a] -> [(a,a)] 
lasthead xs 
  | L.length xs < 2 = [] 
  | otherwise = [(L.last xs, L.head xs)] 

-- combine them 
makepairs :: [a] -> [(a,a)] 
makepairs xs = (take2 xs []) ++ lasthead xs 

-- cartesian product 
cartProd :: [a] -> [b] -> [(a, b)]
cartProd = CM.liftM2 (,) 

-- construct a self cross product, but only upper triangle matrix 
halfCartProd :: [a] -> [(a, a)] -> [(a, a)]
halfCartProd xs ps 
  | L.length xs == 0 = ps 
  | L.length xs == 1 = ps 
--  | L.length xs == 2 = ((L.head xs, L.last xs):ps)      
--  | otherwise = halfCartProd (L.tail xs) ((L.concat (L.map (\x -> (L.head xs,x)) xs)):ps) 
  | otherwise = halfCartProd (L.tail xs)  ((L.map (\x -> (L.head xs,x)) (L.tail xs)) ++ ps)   

-- print (evenOddSplit [1,2,3,4]) 
evenOddSplit :: [a] -> ([a], [a])
evenOddSplit = P.foldr f ([], [])
  where f a (ls, rs) = (rs, a : ls)

-- UNUSED 
-- ----------------- 
-- some safety 
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

--     a safe or total (non-partial) version of init  
--    Complexity O ( n )  
initSafe :: [a] -> [a]
initSafe = liftSafe L.init L.null

--    adds safety to unsafe or partial functions 
--    Complexity O ( 2 ) 
liftSafe :: ( a -> a ) -> ( a -> Bool ) -> ( a -> a )
liftSafe func test val = if test val then val else func val

-- and not so safe 
p2 :: [a] -> a
p2 xs = L.head (L.tail xs) 

p3 :: [a] -> a
p3 xs = L.head (L.tail (L.tail xs)) 

t3 :: [a] -> [a] 
t3 = L.tail.L.tail.L.tail 

-- ----------------- 
-- ?? 
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

-- ----------------- 
