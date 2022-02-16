{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleDataSetSpecification
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Data Set Specification Class. 
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/ 

 -}
module AristotleDataSetSpecification  
    (  

      AristotleOutDSS( .. ), 

      writeAristotleDSS, 
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

-- These used only in DSS   

-- ---------
data AristotleDSSDEInclusion = 
  AristotleDSSDEInclusion   { 
            adssdei_id :: String
--          , adssdei_specialisation_classes :: []              
          , adssdei_reference :: String 
          , adssdei_maximum_occurrences :: Int 
          , adssdei_inclusion :: String 
          , adssdei_specific_information :: String
          , adssdei_conditional_inclusion :: String          
          , adssdei_order :: Int 
          , adssdei_data_element :: Maybe Int 
          , adssdei_group :: Maybe String                     
           } deriving (Show,Generic) 

instance ToJSON AristotleDSSDEInclusion where 
    toJSON (AristotleDSSDEInclusion 
            adssdei_id 
--            adssdei_specialisation_classes
            adssdei_reference adssdei_maximum_occurrences
            adssdei_inclusion adssdei_specific_information
            adssdei_conditional_inclusion adssdei_order  
            adssdei_data_element adssdei_group) = 
        object ["id" .= adssdei_id 
--               ,"specialisation_classes" .= adssdei_specialisation_classes 
               ,"reference" .= adssdei_reference ,"maximum_occurrences" .= adssdei_maximum_occurrences
               ,"inclusion" .= adssdei_inclusion ,"specific_information" .= adssdei_specific_information
               ,"conditional_inclusion" .= adssdei_conditional_inclusion ,"order" .= adssdei_order  
               ,"data_element" .= adssdei_data_element ,"group" .= adssdei_group  
               ]

instance FromJSON AristotleDSSDEInclusion where 
    parseJSON = withObject "AristotleDSSDEInclusion" $ \v -> AristotleDSSDEInclusion 
        <$> v .: "id" 
--        <*> v .: "specialisation_classes"         
        <*> v .: "reference" 
        <*> v .: "maximum_occurrences" 
        <*> v .: "inclusion" 
        <*> v .: "specific_information" 
        <*> v .: "conditional_inclusion" 
        <*> v .: "order" 
        <*> v .:? "data_element"         
        <*> v .: "group"    

defaultAristotleDSSDEInclusion = AristotleDSSDEInclusion "" "" 0 "" "" "" 0 Nothing Nothing            

data AristotleDSSDEInclusionOut  = 
  AristotleDSSDEInclusionOut { 
            ddi2o_concept_id :: String 
          , ddi2o_uuid :: String
          , ddi2o_id :: String          
          , ddi2o_reference :: String
          , ddi2o_maximum_occurrences :: String
          , ddi2o_inclusion :: String 
          , ddi2o_specific_information :: String 
          , ddi2o_conditional_inclusion :: String      
          , ddi2o_order :: String  
          , ddi2o_data_element :: String  
          , ddi2o_group :: String  
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDSSDEInclusionOut
instance ToNamedRecord AristotleDSSDEInclusionOut
instance DefaultOrdered AristotleDSSDEInclusionOut 

defaultAristotleDSSDEInclusionOut = AristotleDSSDEInclusionOut "" "" "" "" "" "" "" "" "" "" "" 

addi2addiOut :: Int -> String -> [AristotleDSSDEInclusion] -> [AristotleDSSDEInclusionOut] 
addi2addiOut concept uuid ps = 
    map (\p -> AristotleDSSDEInclusionOut  
                 (show concept) 
                 uuid 
                 (adssdei_id p)   
                 (adssdei_reference p) 
                 (show (adssdei_maximum_occurrences p))  
                 (adssdei_inclusion p) 
                 (adssdei_specific_information p)   
                 (adssdei_conditional_inclusion p) 
                 (show (adssdei_order p)) 
                 (show (fromMaybe 0 (adssdei_data_element p)))  
                 (fromMaybe "" (adssdei_group p)) 
            ) ps 

-- ---------
data AristotleDSSClusterInclusion = 
  AristotleDSSClusterInclusion   { 
            adssci_id :: String
          , adssci_reference :: String 
          , adssci_maximum_occurrences :: Int 
          , adssci_inclusion :: String               
          , adssci_specific_information :: String
          , adssci_conditional_inclusion :: String          
          , adssci_order :: Int 
          , adssci_child :: Int 
           } deriving (Show,Generic) 

instance ToJSON AristotleDSSClusterInclusion where 
    toJSON (AristotleDSSClusterInclusion 
            adssci_id 
            adssci_reference adssci_maximum_occurrences
            adssci_inclusion adssci_specific_information 
            adssci_conditional_inclusion adssci_order  
            adssci_child) = 
        object ["id" .= adssci_id 
               ,"reference" .= adssci_reference ,"maximum_occurrences" .= adssci_maximum_occurrences
               ,"inclusion" .= adssci_inclusion ,"specific_information" .= adssci_specific_information
               ,"conditional_inclusion" .= adssci_conditional_inclusion ,"order" .= adssci_order  
               ,"child" .= adssci_child   
               ]

instance FromJSON AristotleDSSClusterInclusion where 
    parseJSON = withObject "AristotleDSSClusterInclusion" $ \v -> AristotleDSSClusterInclusion 
        <$> v .: "id" 
        <*> v .: "reference" 
        <*> v .: "maximum_occurrences" 
        <*> v .: "inclusion" 
        <*> v .: "specific_information" 
        <*> v .: "conditional_inclusion" 
        <*> v .: "order" 
        <*> v .: "child"   

defaultAristotleDSSClusterInclusion = AristotleDSSClusterInclusion "" "" 0 "" "" "" 0 0 

data AristotleDSSClusterInclusionOut  = 
  AristotleDSSClusterInclusionOut { 
            dci2o_concept_id :: String 
          , dci2o_uuid :: String
          , dci2o_id :: String          
          , dci2o_reference :: String
          , dci2o_maximum_occurrences :: String
          , dci2o_inclusion :: String 
          , dci2o_specific_information :: String 
          , dci2o_conditional_inclusion :: String  
          , dci2o_order :: String  
          , dci2o_child :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDSSClusterInclusionOut
instance ToNamedRecord AristotleDSSClusterInclusionOut
instance DefaultOrdered AristotleDSSClusterInclusionOut 

defaultAristotleDSSClusterInclusionOut = AristotleDSSClusterInclusionOut "" "" "" "" "" "" "" "" "" "" 

adci2adciOut :: Int -> String -> [AristotleDSSClusterInclusion] -> [AristotleDSSClusterInclusionOut] 
adci2adciOut concept uuid ps = 
    map (\p -> AristotleDSSClusterInclusionOut  
                 (show concept) 
                 uuid 
                 (adssci_id p)   
                 (adssci_reference p) 
                 (show (adssci_maximum_occurrences p))  
                 (adssci_inclusion p) 
                 (adssci_specific_information p) 
                 (adssci_conditional_inclusion p)                  
                 (show (adssci_order p))  
                 (show (adssci_child p))                
            ) ps

-- --------- 
data AristotleGroup = 
  AristotleGroup   { 
            g_id :: Int
          , g_value :: Int 
          , g_dummy :: Int                        
           } deriving (Show,Generic) 

instance ToJSON AristotleGroup where 
    toJSON (AristotleGroup g_id g_value g_dummy ) = 
        object ["id" .= g_id ,"value" .= g_value 
               ,"dummy" .= g_dummy 
               ]

instance FromJSON AristotleGroup where 
    parseJSON = withObject "AristotleGroup" $ \v -> AristotleGroup 
        <$> v .: "id" 
        <*> v .: "value" 
        <*> v .: "dummy"   

defaultAristotleGroup = AristotleGroup 0 0 0 

data AristotleGroupOut  = 
  AristotleGroupOut { 
            g2o_concept_id :: String 
          , g2o_uuid :: String
          , g2o_id :: String          
          , g2o_value :: String
          , g2o_dummy :: String
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleGroupOut
instance ToNamedRecord AristotleGroupOut
instance DefaultOrdered AristotleGroupOut 

defaultAristotleGroupOut = AristotleGroupOut "" "" "" "" "" 

ag2agOut :: Int -> String -> [AristotleGroup] -> [AristotleGroupOut] 
ag2agOut concept uuid ps = 
    map (\p -> AristotleGroupOut  
                 (show concept) 
                 uuid 
                 (show (g_id p))    
                 (show (g_value p)) 
                 (show (g_dummy p)) 
            ) ps

-- Primary parent - all boilerplate fields are on Any Item - just retain id for joining 
data AristotleDataSetSpecification  = 
  AristotleDataSetSpecification { 
            adss_id :: Int 
          , adss_uuid :: String
          , adss_statistical_unit :: Maybe String
          , adss_collection_method :: String
          , adss_groups :: [AristotleGroup] 
          , adss_dssdeinclusion_set :: [AristotleDSSDEInclusion] 
          , adss_dssclusterinclusion_set :: [AristotleDSSClusterInclusion] 
          } deriving (Show,Generic) 

instance ToJSON AristotleDataSetSpecification where 
    toJSON (AristotleDataSetSpecification 
              adss_id adss_uuid 
              adss_statistical_unit 
              adss_collection_method
              adss_groups
              adss_dssdeinclusion_set 
              adss_dssclusterinclusion_set 
      ) = 
        object ["id" .= adss_id , "uuid" .= adss_uuid   
               ,"statistical_unit" .= adss_statistical_unit 
               ,"collection_method" .= adss_collection_method 
               ,"groups" .= adss_groups 
               ,"dssdeinclusion_set" .= adss_dssdeinclusion_set 
               ,"dssclusterinclusion_set" .= adss_dssclusterinclusion_set
               ]

instance FromJSON AristotleDataSetSpecification where 
    parseJSON = withObject "AristotleDataSetSpecification" $ \v -> AristotleDataSetSpecification  
        <$> v .: "id" 
        <*> v .: "uuid" 
        <*> v .:? "statistical_unit"
        <*> v .: "collection_method"
        <*> v .: "groups"
        <*> v .: "dssdeinclusion_set"   
        <*> v .: "dssclusterinclusion_set"  

instance Eq AristotleDataSetSpecification where
  (AristotleDataSetSpecification id1 _ _ _ _ _ _ ) == 
   (AristotleDataSetSpecification id2 _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleDataSetSpecification where
  (AristotleDataSetSpecification id1 _ _ _ _ _ _ ) `compare` 
   (AristotleDataSetSpecification id2 _ _ _ _ _ _ )  
      = id1 `compare` id2  


data AristotleDataSetSpecificationJSON =
  AristotleDataSetSpecificationJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleDataSetSpecification] 
           } deriving (Show,Generic) 

instance Eq AristotleDataSetSpecificationJSON where 
  (AristotleDataSetSpecificationJSON c1 _ _ _ ) == (AristotleDataSetSpecificationJSON c2 _ _ _ )   
      = c1 == c2 

instance ToJSON AristotleDataSetSpecificationJSON 
instance FromJSON AristotleDataSetSpecificationJSON 

defaultAristotleDataSetSpecificationJSON = AristotleDataSetSpecificationJSON 0 Nothing Nothing [] 

data AristotleDataSetSpecificationOut  = 
  AristotleDataSetSpecificationOut { 
            dsddi2o_id :: String 
          , dsddi2o_uuid :: String
          , dsddi2o_statistical_unit :: String
          , dsddi2o_collection_method :: String  
          , dsddi2o_groups :: String 
          , dsddi2o_dssdeinclusion_set :: String
          , dsddi2o_dssclusterinclusion_set :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDataSetSpecificationOut
instance ToNamedRecord AristotleDataSetSpecificationOut
instance DefaultOrdered AristotleDataSetSpecificationOut 

defaultAristotleDataSetSpecificationOut = AristotleDataSetSpecificationOut "" "" "" "" "" "" "" 

adss2adssOut :: AristotleDataSetSpecification -> AristotleDataSetSpecificationOut  
adss2adssOut p = 
       AristotleDataSetSpecificationOut 
          ((show.adss_id) p)  
          (adss_uuid p) 
          (fromMaybe "" (adss_statistical_unit p))   
          (adss_collection_method p) 
          (concatMap show (adss_groups p)) 
          (if (length allddi) < 30000 then allddi 
            else "adss_dssdeinclusion_set too long - see child type for all - 30,000 char truncate = " ++ take 30000 allddi ) 
          (concatMap show (adss_dssclusterinclusion_set p)) 
             where 
                allddi = (concatMap show (adss_dssdeinclusion_set p)) 

-- ---------------------------
-- Testing 
-- Cardinality 
data AristotleDataSetSpecificationCardinality  = 
  AristotleDataSetSpecificationCardinality { 
            adss_groups_max :: Int 
          , adss_dssdeinclusion_set_max :: Int 
          , adss_dssclusterinclusion_set_max :: Int 
          } deriving (Show,Generic) 

defaultAristotleDataSetSpecificationCardinality = AristotleDataSetSpecificationCardinality 0 0 0 

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
adssfold :: AristotleDataSetSpecificationCardinality -> AristotleDataSetSpecification -> AristotleDataSetSpecificationCardinality
adssfold card ap = 
    AristotleDataSetSpecificationCardinality 
        (max (adss_groups_max card) (length (adss_groups ap))) 
        (max (adss_dssdeinclusion_set_max card) (length (adss_dssdeinclusion_set ap))) 
        (max (adss_dssclusterinclusion_set_max card) (length (adss_dssclusterinclusion_set ap)))         

-- -----------------------------
-- Output 

-- extract all primary JSON objects
adssj2oc :: [Either String AristotleDataSetSpecificationJSON] -> [AristotleDataSetSpecification] 
adssj2oc flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
adss2Out :: [AristotleDataSetSpecification] -> [AristotleDataSetSpecificationOut] 
adss2Out jList = map adss2adssOut jList   

-- convert JSON objects into adss_groups dependent list 
adss2gOut :: [AristotleDataSetSpecification] -> [AristotleGroupOut] 
adss2gOut ps = concatMap (\p -> ag2agOut (adss_id p) (adss_uuid p) (adss_groups p)) ps 

-- convert JSON objects into adss_dssdeinclusion_set dependent list 
adss2ddiOut :: [AristotleDataSetSpecification] -> [AristotleDSSDEInclusionOut] 
adss2ddiOut ps = concatMap (\p -> addi2addiOut (adss_id p) (adss_uuid p) (adss_dssdeinclusion_set p)) ps 

-- convert JSON objects into adss_dssclusterinclusion_set dependent list 
adss2dciOut :: [AristotleDataSetSpecification] -> [AristotleDSSClusterInclusionOut] 
adss2dciOut ps = concatMap (\p -> adci2adciOut (adss_id p) (adss_uuid p) (adss_dssclusterinclusion_set p)) ps 

-- All DSS output collected into a single type   
data AristotleOutDSS = 
  AristotleOutDSS { 
            aod_AristotleAnyItemType :: AristotleAnyItemType 
          , aod_AristotleDataSetSpecification :: [AristotleDataSetSpecification]   
          , aod_AristotleProcessResults :: AristotleProcessResults                     
          , aod_AristotleDataSetSpecificationOut :: [AristotleDataSetSpecificationOut] 
          , aod_AristotleDataSetSpecificationGroupOut :: [AristotleGroupOut] 
          , aod_AristotleDataSetSpecificationDSSDEInclusionOut :: [AristotleDSSDEInclusionOut] 
          , aod_AristotleDataSetSpecificationDSSClusterInclusionOut :: [AristotleDSSClusterInclusionOut] 
          } deriving (Show,Generic)  

defaultAristotleOutDSS = AristotleOutDSS DataSetSpecification [] defaultAristotleProcessResults  [] []  [] [] 

populateAristotleOutDSS :: AristotleAnyItemType -> [AristotleDataSetSpecification] -> AristotleProcessResults -> AristotleOutDSS 
populateAristotleOutDSS aait ps apr 
  = AristotleOutDSS aait ps apr (adss2Out ps) (adss2gOut ps) (adss2ddiOut ps) (adss2dciOut ps) 

-- full proc  
getAristotleDSS :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutDSS 
getAristotleDSS aait fetched mts mte 
  = populateAristotleOutDSS aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = adssj2oc parsed 
      ajc = count (safeEither defaultAristotleDataSetSpecificationJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleDataSetSpecificationJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleDSS :: AristotleOutDSS -> String -> IO ()  
writeAristotleDSS aod fp 
  = do 
  if length (aod_AristotleDataSetSpecificationOut aod) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aod_AristotleDataSetSpecificationOut aod))  
  else return ()  

  if length (aod_AristotleDataSetSpecificationGroupOut aod) > 0 then 
    B.writeFile (aait2filename aait fp "Group") (encodeDefaultOrderedByName (aod_AristotleDataSetSpecificationGroupOut aod))  
  else return ()  

  if length (aod_AristotleDataSetSpecificationDSSDEInclusionOut aod) > 0 then 
    B.writeFile (aait2filename aait fp "DSSDEInclusion") (encodeDefaultOrderedByName (aod_AristotleDataSetSpecificationDSSDEInclusionOut aod))  
  else return ()  

  if length (aod_AristotleDataSetSpecificationDSSClusterInclusionOut aod) > 0 then 
    B.writeFile (aait2filename aait fp "DSSClusterInclusion") (encodeDefaultOrderedByName (aod_AristotleDataSetSpecificationDSSClusterInclusionOut aod))  
  else return ()  

      where aait = (aod_AristotleAnyItemType aod) 

-- wrapper for the DSS web request 
web2aod :: W.Options -> Int -> IO AristotleOutDSS 
web2aod opts1 limitpage  = 
  do 
   let aait = DataSetSpecification 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 100)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleDSS aait fetched mts mte)  

-- wrapper for the DSS file read and write  
file2aod adtBS aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((adss2Out.adssj2oc) [(rb2Aristotle adtBS)] ))  
