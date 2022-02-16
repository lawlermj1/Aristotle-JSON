{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleDistribution
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Distribution Class.
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/ 

Fixes: 
The following concepts have a large amount of data (that is, > 30,000 chars length) in the addep_specific_information field.
These are truncated when exported into a CSV. 
43308, 47049, 47281, 47289, 49590, 49601, 49623, 49649, 49761, 51700, 51950, 53226, 53494, 53574, 53629, 53630, 53631, 
53632, 53633, 53634, 53635, 53637, 53638, 53814, 53817, 53836, 53928, 55354, 55423, 55457, 56138, 56298, 56299, 56586, 
57021, 57092, 57126 
  
   JSON Model bug 
   What about specialisation_classes? 
   "Error in $.results[3]['distributiondataelementpath_set'][0]['specialisation_classes'][0]: expected String, but encountered Number"
   These changed to Int. 
   Aristotle swagger spec needs to change. Minor problem. 

 -}
module AristotleDistribution  
    (  
      
      AristotleOutD( .. ), 
      
      writeAristotleD, 
      web2aod, 
      file2aod, 

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

-- used ONLY in Distribution 
data AristotleDistributionDataElementPath = 
  AristotleDistributionDataElementPath   { 
            addep_id :: String
          , addep_logical_path :: String 
          , addep_order :: Int 
          , addep_specific_information :: String
          , addep_data_element :: Maybe Int 
-- model spec is incorrect. Use int instead of string      
          , addep_specialisation_classes :: [Int]  
           } deriving (Show,Generic) 

instance ToJSON AristotleDistributionDataElementPath where 
    toJSON (AristotleDistributionDataElementPath 
            addep_id addep_logical_path addep_order addep_specific_information addep_data_element 
            addep_specialisation_classes
             ) = 
        object ["id" .= addep_id ,"logical_path" .= addep_logical_path  
               ,"order" .= addep_order  ,"specific_information" .= addep_specific_information
               ,"data_element" .= addep_data_element                 
               ,"specialisation_classes" .= addep_specialisation_classes        
               ]

instance FromJSON AristotleDistributionDataElementPath where 
    parseJSON = withObject "AristotleDistributionDataElementPath" $ \v -> AristotleDistributionDataElementPath 
        <$> v .: "id" 
        <*> v .: "logical_path" 
        <*> v .: "order" 
        <*> v .: "specific_information" 
        <*> v .:? "data_element"         
        <*> v .: "specialisation_classes"     

defaultAristotleDistributionDataElementPath = AristotleDistributionDataElementPath "" "" 0 "" Nothing [] 

data AristotleDistributionDataElementPathOut  = 
  AristotleDistributionDataElementPathOut { 
            ddep2o_concept_id :: String 
          , ddep2o_uuid :: String
          , ddep2o_id :: String          
          , ddep2o_logical_path :: String
          , ddep2o_order :: String
          , ddep2o_specific_information :: String 
          , ddep2o_data_element :: String 
          , ddep2o_specialisation_classes :: String                     
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDistributionDataElementPathOut
instance ToNamedRecord AristotleDistributionDataElementPathOut
instance DefaultOrdered AristotleDistributionDataElementPathOut 

defaultAristotleDistributionDataElementPathOut = AristotleDistributionDataElementPathOut "" "" "" "" "" "" "" "" 

addep2addepOut :: Int -> String -> [AristotleDistributionDataElementPath] -> [AristotleDistributionDataElementPathOut] 
addep2addepOut concept uuid ps = 
    map (\p -> AristotleDistributionDataElementPathOut  
                 (show concept) 
                 uuid 
                 (addep_id p)   
                 (addep_logical_path p) 
                 (show (addep_order p))  
-- addep_specific_information can contain a vast amount of html text, so this is truncated for cvs output                  
                 (if (length (addep_specific_information p)) < 30000 
                    then (addep_specific_information p) 
                    else ("! addep_specific_information too long - truncated to 30,000 chars" ++ take 30000 (addep_specific_information p))) 
                 (show (fromMaybe 0 (addep_data_element p)))    
                 (concatMap show (addep_specialisation_classes p))        
            ) ps

-- Primary type 
data AristotleDistribution  = 
  AristotleDistribution { 
            ad_id :: Int 
          , ad_uuid :: String
          , ad_release_date :: Maybe String
          , ad_updated_date :: Maybe String
          , ad_dataset :: Maybe String 
          , ad_license :: Maybe String
          , ad_rights :: Maybe String
          , ad_access_URL :: Maybe String
          , ad_download_URL :: Maybe String
          , ad_byte_size :: Maybe String     
          , ad_media_type :: Maybe String               
          , ad_format_type :: Maybe String  
          , ad_distributiondataelementpath_set :: [AristotleDistributionDataElementPath] 
          } deriving (Show,Generic) 

instance ToJSON AristotleDistribution where 
    toJSON (AristotleDistribution 
              ad_id  ad_uuid 
              ad_release_date ad_updated_date ad_dataset 
              ad_license ad_rights ad_access_URL ad_download_URL  
              ad_byte_size ad_media_type ad_format_type
              ad_distributiondataelementpath_set 
      ) = 
        object ["id" .= ad_id  ,"uuid" .= ad_uuid   
               ,"release_date" .= ad_release_date  ,"updated_date" .= ad_updated_date 
               ,"dataset" .= ad_dataset  ,"license" .= ad_license 
               ,"rights" .= ad_rights  ,"access_URL" .= ad_access_URL 
               ,"download_URL" .= ad_download_URL  ,"byte_size" .= ad_byte_size 
               ,"media_type" .= ad_media_type  ,"format_type" .= ad_format_type         
               ,"distributiondataelementpath_set" .= ad_distributiondataelementpath_set                             
               ]

instance FromJSON AristotleDistribution where 
    parseJSON = withObject "AristotleDistribution" $ \v -> AristotleDistribution  
        <$> v .: "id" 
        <*> v .: "uuid" 
        <*> v .:? "release_date"
        <*> v .:? "updated_date"
        <*> v .:? "dataset"
        <*> v .:? "license"
        <*> v .:? "rights"
        <*> v .:? "access_URL"
        <*> v .:? "download_URL"
        <*> v .:? "byte_size"
        <*> v .:? "media_type"   
        <*> v .:? "format_type" 
        <*> v .: "distributiondataelementpath_set"  

instance Eq AristotleDistribution where
  (AristotleDistribution id1 _ _ _ _ _ _ _ _ _ _ _ _ ) == 
   (AristotleDistribution id2 _ _ _ _ _ _ _ _ _ _ _ _ )  
      = id1 == id2  

instance Ord AristotleDistribution where
  (AristotleDistribution id1 _ _ _ _ _ _ _ _ _ _ _ _ ) `compare` 
   (AristotleDistribution id2 _ _ _ _ _ _ _ _ _ _ _ _ )  
      = id1 `compare` id2  


data AristotleDistributionJSON =
  AristotleDistributionJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleDistribution] 
           } deriving (Show,Generic) 

instance Eq AristotleDistributionJSON where 
  (AristotleDistributionJSON c1 _ _ _ ) == (AristotleDistributionJSON c2 _ _ _ )   
      = c1 == c2  

instance ToJSON AristotleDistributionJSON 
instance FromJSON AristotleDistributionJSON 

defaultAristotleDistributionJSON = AristotleDistributionJSON 0 Nothing Nothing [] 

data AristotleDistributionOut  = 
  AristotleDistributionOut { 
            d2o_id :: String 
          , d2o_uuid :: String
          , d2o_release_date :: String
          , d2o_updated_date :: String
          , d2o_dataset :: String
          , d2o_license :: String
          , d2o_rights :: String
          , d2o_access_URL :: String
          , d2o_downlod2o_URL :: String
          , d2o_byte_size :: String     
          , d2o_media_type :: String               
          , d2o_format_type :: String  
          , d2o_distributiondataelementpath_set :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDistributionOut
instance ToNamedRecord AristotleDistributionOut
instance DefaultOrdered AristotleDistributionOut 

defaultAristotleDistributionOut = AristotleDistributionOut "" "" "" "" "" "" "" "" "" "" "" "" "" 

ad2adOut :: AristotleDistribution -> AristotleDistributionOut  
ad2adOut p = 
       AristotleDistributionOut 
          ((show.ad_id) p) 
          (ad_uuid p) 
          (fromMaybe "" (ad_release_date p))   
          (fromMaybe "" (ad_updated_date p))  
          (fromMaybe "" (ad_dataset p)) 
          (fromMaybe "" (ad_license p)) 
          (fromMaybe "" (ad_rights p))
          (fromMaybe "" (ad_access_URL p))  
          (fromMaybe "" (ad_download_URL p))  
          (fromMaybe ""(ad_byte_size p))   
          (fromMaybe "" (ad_media_type p))                
          (fromMaybe "" (ad_format_type p)) 
          (if (length allddeps) < 30000 then allddeps 
             else "! ad_distributiondataelementpath_set too long - see child type for all - 30,000 char truncate = " ++ take 30000 allddeps ) 
             where allddeps = (concatMap show (ad_distributiondataelementpath_set p))  

-- ------------------------
-- Testing 
-- Cardinality 
data AristotleDistributionCardinality  = 
  AristotleDistributionCardinality { 
            ad_distributiondataelementpath_set_max :: Int   
          } deriving (Show,Generic) 

defaultAristotleDistributionCardinality = AristotleDistributionCardinality 0  

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
adfold :: AristotleDistributionCardinality -> AristotleDistribution -> AristotleDistributionCardinality
adfold card ap = 
    AristotleDistributionCardinality 
        (max (ad_distributiondataelementpath_set_max card) (length (ad_distributiondataelementpath_set ap)))  

-- ------------------------
-- Output 
-- extract all primary JSON objects
adj2ad :: [Either String AristotleDistributionJSON] -> [AristotleDistribution] 
adj2ad flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
ad2Out :: [AristotleDistribution] -> [AristotleDistributionOut] 
ad2Out jList = map ad2adOut jList   

-- convert JSON objects into ad_distributiondataelementpath_set dependent list 
ad2ddepOut :: [AristotleDistribution] -> [AristotleDistributionDataElementPathOut] 
ad2ddepOut ps = concatMap (\p -> addep2addepOut (ad_id p) (ad_uuid p) (ad_distributiondataelementpath_set p)) ps 


-- All D output collected into a single type   
data AristotleOutD = 
  AristotleOutD { 
            aod_AristotleAnyItemType :: AristotleAnyItemType 
          , aod_AristotleDistribution :: [AristotleDistribution]   
          , aod_AristotleProcessResults :: AristotleProcessResults                     
          , aod_AristotleDistributionOut :: [AristotleDistributionOut] 
          , aod_AristotleDistributionDataElementPathOut :: [AristotleDistributionDataElementPathOut] 
          } deriving (Show,Generic)  

defaultAristotleOutD = AristotleOutD Distribution [] defaultAristotleProcessResults  [] []  

populateAristotleOutD :: AristotleAnyItemType -> [AristotleDistribution] -> AristotleProcessResults -> AristotleOutD 
populateAristotleOutD aait ps apr 
  = AristotleOutD aait ps apr (ad2Out ps) (ad2ddepOut ps)  

-- full proc  
getAristotleD :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutD 
getAristotleD aait fetched mts mte 
  = populateAristotleOutD aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = adj2ad parsed 
      ajc = count (safeEither defaultAristotleDistributionJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleDistributionJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleD :: AristotleOutD -> String -> IO ()  
writeAristotleD aod fp 
  = do 
  if length (aod_AristotleDistributionOut aod) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aod_AristotleDistributionOut aod))  
  else return ()  

  if length (aod_AristotleDistributionDataElementPathOut aod) > 0 then 
    B.writeFile (aait2filename aait fp "DataElementPath") (encodeDefaultOrderedByName (aod_AristotleDistributionDataElementPathOut aod))  
  else return ()  

      where aait = (aod_AristotleAnyItemType aod) 

-- wrapper for the D web request 
web2aod :: W.Options -> Int -> IO AristotleOutD 
web2aod opts1 limitpage  = 
  do 
   let aait = Distribution 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 100)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleD aait fetched mts mte)  

-- wrapper for the D file read and write  
file2aod fD aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((ad2Out.adj2ad) [(rb2Aristotle fD)] ))  
  