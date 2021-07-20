{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleDistribution
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Distribution Class.
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/ 

 -}
module AristotleDistribution  
    (  
      
      AristotleDistribution( .. ), 
      AristotleDistributionJSON( .. ),       
      AristotleDistributionOut( .. ),  

      rb2adOut, 
      rb2ad, 
      
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


data AristotleDistribution  = 
  AristotleDistribution { 
            ad_id :: Int 
          , ad_created :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , ad_modified :: UTCTime
          , ad_uuid :: String
          , ad_name :: String
          , ad_definition :: String
          , ad_stewardship_organisation :: String  
          , ad_workgroup :: Int
          , ad_version :: String 
          , ad_references :: String 
          , ad_origin_URI :: String
          , ad_origin :: String 
          , ad_comments :: String
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
          , ad_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , ad_distributiondataelementpath_set :: [AristotleDistributionDataElementPath] 
          , ad_slots :: [AristotleSlot] 
          , ad_customvalue_set :: [AristotleCustomValue] 
          , ad_org_records :: [AristotleOrgRecord]           
          , ad_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleDistribution where 
    toJSON (AristotleDistribution 
              ad_id ad_created ad_modified ad_uuid ad_name
              ad_definition ad_stewardship_organisation ad_workgroup ad_version 
              ad_references ad_origin_URI ad_origin ad_comments 
              ad_release_date ad_updated_date ad_dataset 
              ad_license ad_rights ad_access_URL ad_download_URL  
              ad_byte_size ad_media_type ad_format_type
              ad_metadatareferencelink_set 
              ad_distributiondataelementpath_set 
              ad_slots ad_customvalue_set 
              ad_org_records 
              ad_identifiers
      ) = 
        object ["id" .= ad_id ,"created" .= ad_created  
               ,"modified" .= ad_modified ,"uuid" .= ad_uuid   
               ,"name" .= ad_name ,"definition" .= ad_definition 
               ,"stewardship_organisation" .= ad_stewardship_organisation 
               ,"workgroup" .= ad_workgroup  ,"version" .= ad_version     
               ,"references" .= ad_references  ,"origin_URI" .= ad_origin_URI     
               ,"origin" .= ad_origin  ,"comments" .= ad_comments 
               ,"release_date" .= ad_release_date  ,"updated_date" .= ad_updated_date 
               ,"dataset" .= ad_dataset  ,"license" .= ad_license 
               ,"rights" .= ad_rights  ,"access_URL" .= ad_access_URL 
               ,"download_URL" .= ad_download_URL  ,"byte_size" .= ad_byte_size 
               ,"media_type" .= ad_media_type  ,"format_type" .= ad_format_type 
               ,"metadatareferencelink_set" .= ad_metadatareferencelink_set                
               ,"distributiondataelementpath_set" .= ad_distributiondataelementpath_set  
               ,"slots" .= ad_slots ,"customvalue_set" .= ad_customvalue_set  
               ,"org_records" .= ad_org_records                
               ,"identifiers" .= ad_identifiers                                                                     
               ]

instance FromJSON AristotleDistribution where 
    parseJSON = withObject "AristotleDistribution" $ \v -> AristotleDistribution  
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
        <*> v .: "metadatareferencelink_set"   
        <*> v .: "distributiondataelementpath_set"  
        <*> v .: "slots"        
        <*> v .: "customvalue_set" 
        <*> v .: "org_records"         
        <*> v .: "identifiers" 

instance Eq AristotleDistribution where
  (AristotleDistribution id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
   (AristotleDistribution id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleDistribution where
  (AristotleDistribution id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
   (AristotleDistribution id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotleDistributionJSON =
  AristotleDistributionJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleDistribution] 
           } deriving (Show,Generic) 

instance Eq AristotleDistributionJSON where 
  (AristotleDistributionJSON c1 n1 p1 r1 ) == (AristotleDistributionJSON c2 n2 p2 r2 )   
      = c1 == c2  

instance ToJSON AristotleDistributionJSON 
instance FromJSON AristotleDistributionJSON 

defaultAristotleDistributionJSON = AristotleDistributionJSON 0 Nothing Nothing [] 

data AristotleDistributionOut  = 
  AristotleDistributionOut { 
            d2o_id :: String 
          , d2o_created :: String
          , d2o_modified :: String
          , d2o_uuid :: String
          , d2o_name :: String
          , d2o_definition :: String
          , d2o_stewardship_organisation :: String  
          , d2o_workgroup :: String
          , d2o_version :: String 
          , d2o_references :: String 
          , d2o_origin_URI :: String
          , d2o_origin :: String 
          , d2o_comments :: String
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
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleDistributionOut
instance ToNamedRecord AristotleDistributionOut
instance DefaultOrdered AristotleDistributionOut 

defaultAristotleDistributionOut = AristotleDistributionOut "" "" "" "" "" "" "" "" "" "" "" "" ""   

ad2adOut :: AristotleDistribution -> AristotleDistributionOut  
ad2adOut p = 
       AristotleDistributionOut 
          ((show.ad_id) p)  
          (showUTCTimeDate (ad_created p))          
          (showUTCTimeDate (ad_modified p))  
          (ad_uuid p) 
          (trim (ad_name p)) 
          (trim (ad_definition p)) 
          (ad_stewardship_organisation p)
          ((show.ad_workgroup) p)
          (ad_version p)
          (trim (ad_references p))
          (ad_origin_URI p)
          (trim (ad_origin p))
          (trim (ad_comments p)) 
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
-- 

-- extract the list 
getAristotleDistribution :: AristotleDistributionJSON -> [AristotleDistribution] 
getAristotleDistribution jp = sort (results jp) 

-- convert to out  
allAristotleDistributionOut :: AristotleDistributionJSON -> [AristotleDistributionOut] 
allAristotleDistributionOut jp = map ad2adOut (getAristotleDistribution jp) 

-- parse 
rb2ad :: ByteString -> Either String AristotleDistributionJSON   
rb2ad = (eitherDecode.ctlChar2SpacesBS) 

-- full extraction from byte string to class out
rb2adOut :: ByteString -> [AristotleDistributionOut] 
rb2adOut rb 
  = allAristotleDistributionOut (fromRight defaultAristotleDistributionJSON (rb2ad rb)) 


