{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}

{- |
   Module      : AristotleConceptDelta
   Description : This contains specific JSON and CSV parsing functions for the Aristotle ConceptDelta.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/ 

   need to make permanent 
:set -package uuid-types

   Each page provides 25 records - not a parameter. 
   Time for 50,000: 

   No need to fold by uuid or date, as there is only one record per UUID. 
   test with 2 page gets only - 50 records 

   Cycle single AnyItem through ConceptDelta 
   The single API does not return a paging wrapper with: 
   "count": nn,     "next": null,     "previous": null,     "results": [
   Use fetchUUID instead. 

 -}
module AristotleConceptDelta  
    (  
 
      AristotleOutCD( .. ), 
      
      writeAristotleCD, 
      web2aoc, 
      file2aoc, 
 
      AristotleConceptDeltaIn( .. ), 
      acd2acdIn, 
      acd2KV,
      acd2Map, 


     ) where
    
import Data.Aeson
import Data.Either 
import Data.List 
import Data.Maybe  
import Data.Time  
import GHC.Generics
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered, encodeDefaultOrderedByName)  
import qualified Data.ByteString.Lazy as B 
import qualified Network.Wreq as W 
import Data.Data 
import Data.UUID.Types 
import qualified Data.Map.Strict as M 

import CassavaUtils 
import AristotleCommon  

-- Primary type 
data AristotleConceptDelta  = 
  AristotleConceptDelta { 
            acd_uuid :: String 
          , acd_date :: UTCTime            
          , acd_action :: String 
          } deriving (Show,Generic) 

instance ToJSON AristotleConceptDelta where 
    toJSON (AristotleConceptDelta acd_uuid acd_date acd_action ) = 
        object [ "uuid" .= acd_uuid 
                ,"date" .= acd_date         
                ,"action" .= acd_action ] 

instance FromJSON AristotleConceptDelta where 
    parseJSON = withObject "AristotleConceptDelta" $ \v -> AristotleConceptDelta  
        <$> v .: "uuid" 
        <*> v .: "date" 
        <*> v .: "action"   

instance Eq AristotleConceptDelta where
  (AristotleConceptDelta uuid1 _ _ ) == (AristotleConceptDelta uuid2 _ _ ) = uuid1 == uuid2  

instance Ord AristotleConceptDelta where
  (AristotleConceptDelta uuid1 _ _  ) `compare` (AristotleConceptDelta uuid2 _ _  ) = uuid1 `compare` uuid2  

data AristotleConceptDeltaJSON =
  AristotleConceptDeltaJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleConceptDelta] 
           } deriving (Show,Generic) 

instance Eq AristotleConceptDeltaJSON where 
  (AristotleConceptDeltaJSON c1 _ _ _ ) == (AristotleConceptDeltaJSON c2 _ _ _ ) = c1 == c2 

instance ToJSON AristotleConceptDeltaJSON 
instance FromJSON AristotleConceptDeltaJSON 

defaultAristotleConceptDeltaJSON = AristotleConceptDeltaJSON 0 Nothing Nothing [] 

data AristotleConceptDeltaOut  = 
  AristotleConceptDeltaOut { 
            cd2o_uuid :: String 
          , cd2o_date :: String
          , cd2o_action :: String          
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleConceptDeltaOut
instance ToNamedRecord AristotleConceptDeltaOut
instance DefaultOrdered AristotleConceptDeltaOut 

defaultAristotleConceptDeltaOut = AristotleConceptDeltaOut "" "" "" 

acd2acdOut :: AristotleConceptDelta -> AristotleConceptDeltaOut  
acd2acdOut p = 
       AristotleConceptDeltaOut 
          (acd_uuid p) 
          (showUTCTimeDate (acd_date p)) 
          (acd_action p) 

-- ----------
-- Input 
data Action = Created | Modified | Deleted | Undefined 
     deriving (  Eq, Ord, Typeable, Show, Read, Data )    

-- if created, then it is new, and not yet modified. 
-- if modified, then it has had at least one change. 
parseAction :: String -> Action 
parseAction s 
   | s == "created" = Created 
   | s == "modified" = Modified 
   | s == "deleted" = Deleted 
   | otherwise = Undefined 

data AristotleConceptDeltaIn  = 
  AristotleConceptDeltaIn { 
            cd2i_uuid :: UUID  
          , cd2i_date :: UTCTime
          , cd2i_action :: Action           
           } deriving (Show, Generic) 

defaultAristotleConceptDeltaIn = AristotleConceptDeltaIn nil epochUTCTime Undefined 

instance Eq AristotleConceptDeltaIn where
  (AristotleConceptDeltaIn id1 _ _ ) == (AristotleConceptDeltaIn id2 _ _ ) = id1 == id2  

instance Ord AristotleConceptDeltaIn where
  (AristotleConceptDeltaIn id1 _ _ ) `compare` (AristotleConceptDeltaIn id2 _ _ ) = id1 `compare` id2  

-- remove deleted UUIDs 
filterDeleted :: AristotleConceptDelta -> Bool  
filterDeleted acd = (acd_action acd) /= "deleted"  

acd2acdIn :: AristotleConceptDelta -> AristotleConceptDeltaIn  
acd2acdIn p = 
       AristotleConceptDeltaIn 
          (fromMaybe nil (fromString (acd_uuid p)))   
          (acd_date p) 
          (parseAction (acd_action p)) 

-- create a tuple 
acdTuple :: AristotleConceptDeltaIn -> (UUID, AristotleConceptDeltaIn) 
acdTuple p = ((cd2i_uuid p), p) 

--  filter delted and turn into a tuple    
acd2KV :: [AristotleConceptDelta] -> [(UUID, AristotleConceptDeltaIn)] 
acd2KV acds = map (acdTuple.acd2acdIn) (filter filterDeleted acds) 

-- sorts and lift into a Map 
acd2Map :: [(UUID, AristotleConceptDeltaIn)] -> M.Map UUID AristotleConceptDeltaIn
acd2Map acdis = M.fromList acdis 


-- no need to get max date, as profiling shows that there is only 1 record per UUID 
-- find max date 
compDate :: UTCTime -> AristotleConceptDeltaIn -> UTCTime 
compDate u acd = if u > (cd2i_date acd) then u else (cd2i_date acd) 

-- foldl :: (b -> a -> b) -> b -> t a -> b
maxDate :: [AristotleConceptDeltaIn] -> UTCTime  
maxDate acdis = foldl compDate epochUTCTime acdis 

-- filter :: (a -> Bool) -> [a] -> [a] 
-- get most recent or maximum acd 
getMaxDateLU :: [(UUID, [AristotleConceptDeltaIn])] -> [(UUID, AristotleConceptDeltaIn)] 
getMaxDateLU uacds
  = map (\(u,acdis) -> (u, head (filter (\a -> (cd2i_date a) == (maxDate acdis)) acdis))) uacds 


-- ------------------------
-- Output 
-- extract all primary JSON objects
acdj2acd :: [Either String AristotleConceptDeltaJSON] -> [AristotleConceptDelta] 
acdj2acd flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
acd2Out :: [AristotleConceptDelta] -> [AristotleConceptDeltaOut] 
acd2Out jList = map acd2acdOut jList   

-- All CD output collected into a single type   
data AristotleOutCD = 
  AristotleOutCD { 
            aoc_AristotleAnyItemType :: AristotleAnyItemType 
          , aoc_AristotleConceptDelta :: [AristotleConceptDelta]   
          , aoc_AristotleProcessResults :: AristotleProcessResults                     
          , aoc_AristotleConceptDeltaOut :: [AristotleConceptDeltaOut] 
          , aoc_AristotleConceptDeltaIn :: [(UUID, AristotleConceptDeltaIn)]          
          } deriving (Show,Generic)  

defaultAristotleOutCD = AristotleOutCD ConceptDelta [] defaultAristotleProcessResults  [] [] 

populateAristotleOutCD :: AristotleAnyItemType -> [AristotleConceptDelta] -> AristotleProcessResults -> AristotleOutCD 
populateAristotleOutCD aait ps apr 
  = AristotleOutCD aait ps apr (acd2Out ps) (acd2KV ps) 

-- full proc  
getAristotleCD :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutCD 
getAristotleCD aait fetched mts mte 
  = populateAristotleOutCD aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = acdj2acd parsed 
      ajc = count (safeEither defaultAristotleConceptDeltaJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleConceptDeltaJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleCD :: AristotleOutCD -> String -> IO ()  
writeAristotleCD aoc fp 
  = do 
  if length (aoc_AristotleConceptDeltaOut aoc) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aoc_AristotleConceptDeltaOut aoc))  
  else return ()  

      where aait = (aoc_AristotleAnyItemType aoc) 

-- wrapper for the CD web request 
web2aoc :: W.Options -> Int -> IO AristotleOutCD 
web2aoc opts1 limitpage  = 
  do 
   let aait = ConceptDelta 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 0)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleCD aait fetched mts mte)  

-- wrapper for the CD file read and write  
file2aoc fCD aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((acd2Out.acdj2acd) [(rb2Aristotle fCD)] ))  
