{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleDataSetSpecification
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Data Set Specification Class. 
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/ 

 -}
module AristotleDataSetSpecification  
    (  
      
      AristotleDataSetSpecification( .. ), 
      AristotleDataSetSpecificationJSON( .. ),       
      AristotleDataSetSpecificationOut( .. ),  

      rb2adssOut, 
      rb2adss,      
      
     ) where
    
import Data.Aeson
import Data.Either 
import Data.List 
import Data.Maybe  
import Data.Time  
import GHC.Generics
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered) 
import Data.ByteString.Lazy (ByteString) 

import CassavaUtils 
import AristotleCommon 


data AristotleDataSetSpecification  = 
  AristotleDataSetSpecification { 
            adss_id :: Int 
          , adss_created :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , adss_modified :: UTCTime 
          , adss_uuid :: String
          , adss_name :: String
          , adss_definition :: String
          , adss_stewardship_organisation :: String  
          , adss_workgroup :: Int
          , adss_version :: String 
          , adss_references :: String 
          , adss_origin_URI :: String
          , adss_origin :: String 
          , adss_comments :: String
          , adss_statistical_unit :: Maybe String
          , adss_collection_method :: String

          , adss_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , adss_groups :: [AristotleGroup] 
          , adss_dssdeinclusion_set :: [AristotleDSSDEInclusion] 
          , adss_dssclusterinclusion_set :: [AristotleDSSClusterInclusion] 
          , adss_slots :: [AristotleSlot] 
          , adss_customvalue_set :: [AristotleCustomValue] 
          , adss_org_records :: [AristotleOrgRecord]           
          , adss_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleDataSetSpecification where 
    toJSON (AristotleDataSetSpecification 
              adss_id adss_created adss_modified adss_uuid adss_name
              adss_definition adss_stewardship_organisation adss_workgroup adss_version 
              adss_references adss_origin_URI adss_origin adss_comments 
              adss_statistical_unit adss_collection_method
              adss_metadatareferencelink_set adss_groups
              adss_dssdeinclusion_set adss_dssclusterinclusion_set 
              adss_slots adss_customvalue_set 
              adss_org_records 
              adss_identifiers
      ) = 
        object ["id" .= adss_id ,"created" .= adss_created  
               ,"modified" .= adss_modified ,"uuid" .= adss_uuid   
               ,"name" .= adss_name ,"definition" .= adss_definition 
               ,"stewardship_organisation" .= adss_stewardship_organisation 
               ,"workgroup" .= adss_workgroup  ,"version" .= adss_version     
               ,"references" .= adss_references  ,"origin_URI" .= adss_origin_URI     
               ,"origin" .= adss_origin  ,"comments" .= adss_comments 
               ,"statistical_unit" .= adss_statistical_unit  ,"collection_method" .= adss_collection_method 

               ,"metadatareferencelink_set" .= adss_metadatareferencelink_set  ,"groups" .= adss_groups 

               ,"dssdeinclusion_set" .= adss_dssdeinclusion_set ,"dssclusterinclusion_set" .= adss_dssclusterinclusion_set
               ,"slots" .= adss_slots ,"customvalue_set" .= adss_customvalue_set  
               ,"org_records" .= adss_org_records 

               ,"identifiers" .= adss_identifiers                                                                     
               ]

instance FromJSON AristotleDataSetSpecification where 
    parseJSON = withObject "AristotleDataSetSpecification" $ \v -> AristotleDataSetSpecification  
        <$> v .: "id" 
        <*> v .: "created" 
        <*> v .: "modified" 
        <*> v .: "uuid" 
        <*> v .: "name" 
        <*> v .: "definition" 
        <*> v .: "stewardship_organisation" 
        <*> v .: "workgroup" 
        <*> v .: "version"
        <*> v .: "references" 
        <*> v .: "origin_URI" 
        <*> v .: "origin" 
        <*> v .: "comments"
        <*> v .:? "statistical_unit"
        <*> v .: "collection_method"

        <*> v .: "metadatareferencelink_set"
        <*> v .: "groups"

        <*> v .: "dssdeinclusion_set"   
        <*> v .: "dssclusterinclusion_set"   
        <*> v .: "slots"        
        <*> v .: "customvalue_set" 

        <*> v .: "org_records" 
                
        <*> v .: "identifiers" 

instance Eq AristotleDataSetSpecification where
  (AristotleDataSetSpecification id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
   (AristotleDataSetSpecification id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleDataSetSpecification where
  (AristotleDataSetSpecification id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
   (AristotleDataSetSpecification id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotleDataSetSpecificationJSON =
  AristotleDataSetSpecificationJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleDataSetSpecification] 
           } deriving (Show,Generic) 

instance Eq AristotleDataSetSpecificationJSON where 
  (AristotleDataSetSpecificationJSON c1 n1 p1 r1 ) == (AristotleDataSetSpecificationJSON c2 n2 p2 r2 )   
      = c1 == c2  
--     = c1 == c2 && n1 == n2 && p1 == p2  && r1 == r2          

instance ToJSON AristotleDataSetSpecificationJSON 
instance FromJSON AristotleDataSetSpecificationJSON 

defaultAristotleDataSetSpecificationJSON = AristotleDataSetSpecificationJSON 0 Nothing Nothing [] 

data AristotleDataSetSpecificationOut  = 
  AristotleDataSetSpecificationOut { 
            dss2o_id :: String 
          , dss2o_created :: String
          , dss2o_modified :: String
          , dss2o_uuid :: String
          , dss2o_name :: String
          , dss2o_definition :: String
          , dss2o_stewardship_organisation :: String  
          , dss2o_workgroup :: String
          , dss2o_version :: String 
          , dss2o_references :: String 
          , dss2o_origin_URI :: String
          , dss2o_origin :: String 
          , dss2o_comments :: String
          , dss2o_statistical_unit :: String
          , dss2o_collection_method :: String          
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDataSetSpecificationOut
instance ToNamedRecord AristotleDataSetSpecificationOut
instance DefaultOrdered AristotleDataSetSpecificationOut 

defaultAristotleDataSetSpecificationOut = AristotleDataSetSpecificationOut "" "" "" "" "" "" "" "" "" "" "" "" ""   

adss2adssOut :: AristotleDataSetSpecification -> AristotleDataSetSpecificationOut  
adss2adssOut p = 
       AristotleDataSetSpecificationOut 
          ((show.adss_id) p)  
          (showUTCTimeDate (adss_created p))           
          (showUTCTimeDate (adss_modified p)) 
          (adss_uuid p) 
          (trim (adss_name p)) 
          (trim (adss_definition p)) 
          (adss_stewardship_organisation p)
          ((show.adss_workgroup) p)
          (adss_version p)
          (trim (adss_references p))
          (adss_origin_URI p)
          (trim (adss_origin p))
          (trim (adss_comments p)) 
          (fromMaybe "" (adss_statistical_unit p))   
          (adss_collection_method p) 

-- ---
-- extract the list 
getAristotleDataSetSpecification :: AristotleDataSetSpecificationJSON -> [AristotleDataSetSpecification] 
getAristotleDataSetSpecification jp = sort (results jp) 

-- convert to out  
allAristotleDataSetSpecificationOut :: AristotleDataSetSpecificationJSON -> [AristotleDataSetSpecificationOut] 
allAristotleDataSetSpecificationOut jp = map adss2adssOut (getAristotleDataSetSpecification jp) 

-- parse 
rb2adss :: ByteString -> Either String AristotleDataSetSpecificationJSON   
rb2adss = (eitherDecode.ctlChar2SpacesBS) 

-- full extraction from byte string to class out
rb2adssOut :: ByteString -> [AristotleDataSetSpecificationOut] 
rb2adssOut rb 
  = allAristotleDataSetSpecificationOut (fromRight defaultAristotleDataSetSpecificationJSON (rb2adss rb)) 
