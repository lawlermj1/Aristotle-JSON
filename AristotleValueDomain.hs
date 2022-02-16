{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleValueDomain
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Value Domain Class.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/

 -}
module AristotleValueDomain  
    (  

      AristotleOutVD( .. ), 
      
      writeAristotleVD, 
      web2aov, 
      file2aov, 

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

-- used only by VD 
data AristotlePermissibleValue = 
  AristotlePermissibleValue   { 
            pv_id :: String
          , pv_value :: String 
          , pv_meaning :: String   
          , pv_value_meaning :: Maybe String   
          , pv_order :: Int 
          , pv_start_date :: Maybe String 
          , pv_end_date :: Maybe String                                                              
           } deriving (Show,Generic) 

instance ToJSON AristotlePermissibleValue where 
    toJSON (AristotlePermissibleValue pv_id pv_value pv_meaning pv_value_meaning pv_order 
        pv_start_date pv_end_date) = 
        object ["id" .= pv_id ,"value" .= pv_value 
               ,"meaning" .= pv_meaning 
               ,"value_meaning" .= pv_value_meaning    
               ,"order" .= pv_order  
               ,"start_date" .= pv_start_date  
               ,"end_date" .= pv_end_date                             
               ]

instance FromJSON AristotlePermissibleValue where 
    parseJSON = withObject "AristotlePermissibleValue" $ \v -> AristotlePermissibleValue 
        <$> v .: "id" 
        <*> v .: "value" 
        <*> v .: "meaning"    
        <*> v .:? "value_meaning"  
        <*> v .: "order" 
        <*> v .:? "start_date" 
        <*> v .:? "end_date" 

defaultAristotlePermissibleValue = AristotlePermissibleValue "" "" "" Nothing 0 Nothing Nothing      

data AristotlePermissibleValueOut  = 
  AristotlePermissibleValueOut { 
            pv2o_concept_id :: String 
          , pv2o_uuid :: String
          , pv2o_id :: String          
          , pv2o_value :: String
          , pv2o_meaning :: String
          , pv2o_value_meaning :: String 
          , pv2o_order :: String 
          , pv2o_start_date :: String      
          , pv2o_end_date :: String                            
           } deriving (Show,Generic) 

instance FromNamedRecord AristotlePermissibleValueOut
instance ToNamedRecord AristotlePermissibleValueOut
instance DefaultOrdered AristotlePermissibleValueOut 

defaultAristotlePermissibleValueOut = AristotlePermissibleValueOut "" "" "" "" "" "" "" "" ""   

apv2apvOut :: Int -> String -> [AristotlePermissibleValue] -> [AristotlePermissibleValueOut] 
apv2apvOut concept uuid ps = 
    map (\p -> AristotlePermissibleValueOut  
                 (show concept) 
                 uuid 
                 (pv_id p)  
                 (pv_value p) 
                 (pv_meaning p)  
                 (fromMaybe "" (pv_value_meaning p)) 
                 (show (pv_order p)) 
                 (fromMaybe "" (pv_start_date p))    
                 (fromMaybe "" (pv_end_date p))                                                   
            ) ps

-- Primary type 
data AristotleValueDomain  = 
  AristotleValueDomain { 
            avd_id :: Int 
          , avd_uuid :: String
          , avd_data_type :: Maybe String   
          , avd_format :: Maybe String  
          , avd_maximum_length :: Maybe Int
          , avd_unit_of_measure :: Maybe String 
          , avd_conceptual_domain :: Maybe String      
          , avd_classification_scheme :: Maybe String              
          , avd_representation_class :: Maybe Int
          , avd_description :: String                      
          , avd_permissiblevalue_set :: [AristotlePermissibleValue]          
          , avd_supplementaryvalue_set :: [AristotlePermissibleValue]
          } deriving (Show,Generic) 

instance ToJSON AristotleValueDomain where 
    toJSON (AristotleValueDomain 
              avd_id avd_uuid 
              avd_data_type avd_format avd_maximum_length 
              avd_unit_of_measure avd_conceptual_domain 
              avd_classification_scheme avd_representation_class 
              avd_description 
              avd_permissiblevalue_set avd_supplementaryvalue_set 
      ) = 
        object ["id" .= avd_id ,"uuid" .= avd_uuid   
               ,"data_type" .= avd_data_type  ,"format" .= avd_format  
               ,"maximum_length" .= avd_maximum_length  ,"unit_of_measure" .= avd_unit_of_measure    
               ,"conceptual_domain" .= avd_conceptual_domain  ,"classification_scheme" .= avd_classification_scheme 
               ,"description" .= avd_description  
               ,"permissiblevalue_set" .= avd_permissiblevalue_set   
               ,"supplementaryvalue_set" .= avd_supplementaryvalue_set  
               ]

instance FromJSON AristotleValueDomain where 
    parseJSON = withObject "AristotleValueDomain" $ \v -> AristotleValueDomain  
        <$> v .: "id" 
        <*> v .: "uuid" 
        <*> v .:? "data_type"
        <*> v .:? "format"
        <*> v .:? "maximum_length" 
        <*> v .:? "unit_of_measure"
        <*> v .:? "conceptual_domain"
        <*> v .:? "classification_scheme"
        <*> v .:? "representation_class"   
        <*> v .: "description" 
        <*> v .: "permissiblevalue_set"    
        <*> v .: "supplementaryvalue_set"  

instance Eq AristotleValueDomain where
  (AristotleValueDomain id1 _ _ _ _ _ _ _ _ _ _ _ ) == 
    (AristotleValueDomain id2 _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleValueDomain where
  (AristotleValueDomain id1 _ _ _ _ _ _ _ _ _ _ _ ) `compare` 
    (AristotleValueDomain id2 _ _ _ _ _ _ _ _ _ _ _ )  
      = id1 `compare` id2  


data AristotleValueDomainJSON =
  AristotleValueDomainJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleValueDomain] 
           } deriving (Show,Generic) 

instance Eq AristotleValueDomainJSON where 
  (AristotleValueDomainJSON c1 _ _ _ ) == (AristotleValueDomainJSON c2 _ _ _ )   
      = c1 == c2  

instance ToJSON AristotleValueDomainJSON 
instance FromJSON AristotleValueDomainJSON 

defaultAristotleValueDomainJSON = AristotleValueDomainJSON 0 Nothing Nothing [] 

data AristotleValueDomainOut  = 
  AristotleValueDomainOut { 
            vd2o_id :: String 
          , vd2o_uuid :: String
          , vd2o_data_type :: String
          , vd2o_format :: String 
          , vd2o_maximum_length :: String 
          , vd2o_unit_of_measure :: String 
          , vd2o_conceptual_domain :: String 
          , vd2o_classification_scheme :: String 
          , vd2o_representation_class :: String 
          , vd2o_description :: String 
          , vd2o_permissiblevalue_set :: String        
          , vd2o_supplementaryvalue_set :: String 
          } deriving (Show,Generic) 

instance FromNamedRecord AristotleValueDomainOut
instance ToNamedRecord AristotleValueDomainOut
instance DefaultOrdered AristotleValueDomainOut 

defaultAristotleValueDomainOut = AristotleValueDomainOut "" "" "" "" "" "" "" "" "" "" "" "" 

avd2avdOut :: AristotleValueDomain -> AristotleValueDomainOut  
avd2avdOut p = 
       AristotleValueDomainOut 
          ((show.avd_id) p)  
          (avd_uuid p) 
          (fromMaybe "" (avd_data_type p))  
          (fromMaybe "" (avd_format p))  
          (show (fromMaybe 0 (avd_maximum_length p)))      
          (fromMaybe "" (avd_unit_of_measure p))               
          (fromMaybe "" (avd_conceptual_domain p)) 
          (fromMaybe "" (avd_classification_scheme p)) 
          (show (fromMaybe 0 (avd_representation_class p)))  
          (avd_description p)  
          (if (length allpvs) < 30000 then allpvs else "avd_permissiblevalue_set too long - see child type for all - 30,000 char truncate = " ++ take 30000 allpvs ) 
          (concatMap show (avd_supplementaryvalue_set p)) 
             where 
                allpvs = (concatMap show (avd_permissiblevalue_set p))    

-- ------------------------
-- Testing 
-- Cardinality 
data AristotleValueDomainCardinality  = 
  AristotleValueDomainCardinality { 
            avd_permissiblevalue_set_max :: Int  
          , avd_supplementaryvalue_set_max :: Int  
          } deriving (Show,Generic) 

defaultAristotleValueDomainCardinality = AristotleValueDomainCardinality 0 0  

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
avdfold :: AristotleValueDomainCardinality -> AristotleValueDomain -> AristotleValueDomainCardinality
avdfold card avd = 
    AristotleValueDomainCardinality 
        (max (avd_permissiblevalue_set_max card) (length (avd_permissiblevalue_set avd))) 
        (max (avd_supplementaryvalue_set_max card) (length (avd_supplementaryvalue_set avd)))   

-- ------------------------
-- Output 
-- extract all primary JSON objects
avdj2avd :: [Either String AristotleValueDomainJSON] -> [AristotleValueDomain] 
avdj2avd flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
avd2Out :: [AristotleValueDomain] -> [AristotleValueDomainOut] 
avd2Out jList = map avd2avdOut jList   

-- convert JSON objects into permissiblevalue_set dependent list 
avd2pvsOut :: [AristotleValueDomain] -> [AristotlePermissibleValueOut] 
avd2pvsOut ps = concatMap (\p -> apv2apvOut (avd_id p) (avd_uuid p) (avd_permissiblevalue_set p)) ps 

-- convert JSON objects into avd_supplementaryvalue_set dependent list 
avd2svsOut :: [AristotleValueDomain] -> [AristotlePermissibleValueOut] 
avd2svsOut ps = concatMap (\p -> apv2apvOut (avd_id p) (avd_uuid p) (avd_supplementaryvalue_set p)) ps 


-- All VD output collected into a single type   
data AristotleOutVD = 
  AristotleOutVD { 
            aov_AristotleAnyItemType :: AristotleAnyItemType 
          , aov_AristotleValueDomain :: [AristotleValueDomain]   
          , aov_AristotleProcessResults :: AristotleProcessResults                     
          , aov_AristotleValueDomainOut :: [AristotleValueDomainOut] 
          , aov_AristotleValueDomainPermissibleValueOut :: [AristotlePermissibleValueOut] 
          , aov_AristotleValueDomainSupplementaryValueOut :: [AristotlePermissibleValueOut] 
          } deriving (Show,Generic)  

defaultAristotleOutVD = AristotleOutVD ValueDomain [] defaultAristotleProcessResults  [] []  []  

populateAristotleOutVD :: AristotleAnyItemType -> [AristotleValueDomain] -> AristotleProcessResults -> AristotleOutVD 
populateAristotleOutVD aait ps apr 
  = AristotleOutVD aait ps apr (avd2Out ps) (avd2pvsOut ps) (avd2svsOut ps)  

-- full proc  
getAristotleVD :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutVD 
getAristotleVD aait fetched mts mte 
  = populateAristotleOutVD aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = avdj2avd parsed 
      ajc = count (safeEither defaultAristotleValueDomainJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleValueDomainJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleVD :: AristotleOutVD -> String -> IO ()  
writeAristotleVD aov fp 
  = do 
  if length (aov_AristotleValueDomainOut aov) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aov_AristotleValueDomainOut aov))  
  else return ()  

  if length (aov_AristotleValueDomainPermissibleValueOut aov) > 0 then 
    B.writeFile (aait2filename aait fp "PermissibleValue") (encodeDefaultOrderedByName (aov_AristotleValueDomainPermissibleValueOut aov))  
  else return ()  

  if length (aov_AristotleValueDomainSupplementaryValueOut aov) > 0 then 
    B.writeFile (aait2filename aait fp "SupplementaryValue") (encodeDefaultOrderedByName (aov_AristotleValueDomainSupplementaryValueOut aov))  
  else return ()  

      where aait = (aov_AristotleAnyItemType aov) 

-- wrapper for the VD web request 
web2aov :: W.Options -> Int -> IO AristotleOutVD 
web2aov opts1 limitpage  = 
  do 
   let aait = ValueDomain 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 100)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleVD aait fetched mts mte)  

-- wrapper for the VD file read and write  
file2aov fVD aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((avd2Out.avdj2avd) [(rb2Aristotle fVD)] ))  
