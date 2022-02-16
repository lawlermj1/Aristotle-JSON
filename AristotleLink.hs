{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleLink
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Object Class.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/

   [AristotleLinkEnd] always has a cardinality of 2 
   Need to add this as a test. 
   So this is flattened to the primary type. 

 -}
module AristotleLink  
    (  
      
      AristotleOutL( .. ), 
      
      writeAristotleL, 
      web2aol, 
      file2aol, 

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

import CassavaUtils 
import AristotleCommon  

-- used only by Link  
data AristotleLinkEnd  = 
  AristotleLinkEnd { 
            ale_role :: String 
          , ale_concept :: Int
          } deriving (Show,Generic) 

instance ToJSON AristotleLinkEnd where 
    toJSON (AristotleLinkEnd ale_role ale_concept  ) = 
        object ["role" .= ale_role ,"concept" .= ale_concept ]

instance FromJSON AristotleLinkEnd where 
    parseJSON = withObject "AristotleLinkEnd" $ \v -> AristotleLinkEnd  
        <$> v .: "role" 
        <*> v .: "concept" 

defaultAristotleLinkEnd = AristotleLinkEnd "" 0 

-- Primary type 
data AristotleLink  = 
  AristotleLink { 
            al_pk :: Int 
          , al_relation :: String
          , al_root_item :: String    
          , al_linkend_set :: [AristotleLinkEnd]             
          , al_modified :: UTCTime
          } deriving (Show,Generic) 

instance ToJSON AristotleLink where 
    toJSON (AristotleLink 
              al_pk al_relation al_root_item 
              al_linkend_set 
              al_modified 
      ) = 
        object ["pk" .= al_pk 
               ,"relation" .= al_relation 
               ,"root_item" .= al_root_item 
               ,"linkend_set" .= al_linkend_set                     
               ,"modified" .= al_modified                                                                     
               ]

instance FromJSON AristotleLink where 
    parseJSON = withObject "AristotleLink" $ \v -> AristotleLink  
        <$> v .: "pk" 
        <*> v .: "relation" 
        <*> v .: "root_item"   
        <*> v .: "linkend_set"                
        <*> v .: "modified" 

instance Eq AristotleLink where
  (AristotleLink pk1 _ _ _ _) == (AristotleLink pk2 _ _ _ _ ) = pk1 == pk2  

instance Ord AristotleLink where
  (AristotleLink pk1 _ _ _ _ ) `compare` (AristotleLink pk2 _ _ _ _ ) = pk1 `compare` pk2  

data AristotleLinkJSON =
  AristotleLinkJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleLink] 
           } deriving (Show,Generic) 

instance Eq AristotleLinkJSON where 
  (AristotleLinkJSON c1 _ _ _  ) == (AristotleLinkJSON c2 _ _ _ )   
      = c1 == c2 

instance ToJSON AristotleLinkJSON 
instance FromJSON AristotleLinkJSON 

defaultAristotleLinkJSON = AristotleLinkJSON 0 Nothing Nothing [] 

data AristotleLinkOut  = 
  AristotleLinkOut { 
            l2o_pk :: String 
          , l2o_relation :: String
          , l2o_root_item :: String  
          , l2o_linkend_set :: String                    
          , l2o_modified :: String
          , l2o_linkend_from_role :: String    
          , l2o_linkend_from_concept :: String    
          , l2o_linkend_to_role :: String    
          , l2o_linkend_to_concept :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleLinkOut
instance ToNamedRecord AristotleLinkOut
instance DefaultOrdered AristotleLinkOut 

defaultAristotleLinkOut = AristotleLinkOut "" "" "" "" "" "" "" "" 

al2alOut :: AristotleLink -> AristotleLinkOut  
al2alOut p = 
       AristotleLinkOut 
          ((show.al_pk) p)  
          (al_relation p)
          (al_root_item p) 
          (concatMap show ap) 
          (showUTCTimeDate (al_modified p))  
          (ale_role ap0) 
          (show (ale_concept ap0)) 
          (ale_role ap1) 
          (show (ale_concept ap1))   
             where 
                ap = al_linkend_set p 
                ap0 = if length ap >= 1 then head ap else defaultAristotleLinkEnd 
                ap1 = if length ap == 2 then last ap else defaultAristotleLinkEnd 

-- ------------------------
-- Output 
-- extract all primary JSON objects
alj2al :: [Either String AristotleLinkJSON] -> [AristotleLink] 
alj2al flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
al2Out :: [AristotleLink] -> [AristotleLinkOut] 
al2Out jList = map al2alOut jList   

-- All L output collected into a single type   
data AristotleOutL = 
  AristotleOutL { 
            aol_AristotleAnyItemType :: AristotleAnyItemType 
          , aol_AristotleLink :: [AristotleLink]   
          , aol_AristotleProcessResults :: AristotleProcessResults                     
          , aol_AristotleLinkOut :: [AristotleLinkOut] 
          } deriving (Show,Generic)  

defaultAristotleOutL = AristotleOutL Link [] defaultAristotleProcessResults  [] 

populateAristotleOutL :: AristotleAnyItemType -> [AristotleLink] -> AristotleProcessResults -> AristotleOutL 
populateAristotleOutL aait ps apr 
  = AristotleOutL aait ps apr (al2Out ps)  

-- full proc  
getAristotleL :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutL 
getAristotleL aait fetched mts mte 
  = populateAristotleOutL aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = alj2al parsed 
      ajc = count (safeEither defaultAristotleLinkJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleLinkJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleL :: AristotleOutL -> String -> IO ()  
writeAristotleL aol fp 
  = do 
  if length (aol_AristotleLinkOut aol) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aol_AristotleLinkOut aol))  
  else return ()  

      where aait = (aol_AristotleAnyItemType aol) 

-- wrapper for the Link web request 
web2aol :: W.Options -> Int -> IO AristotleOutL 
web2aol opts1 limitpage  = 
  do 
   let aait = Link 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 0)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleL aait fetched mts mte)  

-- wrapper for the L file read and write  
file2aol fL aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((al2Out.alj2al) [(rb2Aristotle fL)] ))  
      


