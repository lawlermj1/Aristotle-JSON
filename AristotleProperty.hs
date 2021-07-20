{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleProperty
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Property Class.
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/ 

 -}
module AristotleProperty  
    (  
      
      AristotleProperty( .. ), 
      AristotlePropertyJSON( .. ),       
      AristotlePropertyOut( .. ),  

      rb2apOut, 
      rb2ap, 
      
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


data AristotleProperty  = 
  AristotleProperty { 
            ap_id :: Int 
          , ap_created :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , ap_modified :: UTCTime 
          , ap_uuid :: String
          , ap_name :: String
          , ap_definition :: String
          , ap_stewardship_organisation :: String  
          , ap_workgroup :: Int
          , ap_version :: String 
          , ap_references :: String 
          , ap_origin_URI :: String
          , ap_origin :: String 
          , ap_comments :: String
          , ap_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , ap_slots :: [AristotleSlot] 
          , ap_customvalue_set :: [AristotleCustomValue] 
          , ap_org_records :: [AristotleOrgRecord]           
          , ap_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleProperty where 
    toJSON (AristotleProperty 
              ap_id ap_created ap_modified ap_uuid ap_name
              ap_definition ap_stewardship_organisation ap_workgroup ap_version 
              ap_references ap_origin_URI ap_origin ap_comments 
              ap_metadatareferencelink_set ap_slots               
              ap_customvalue_set 
              ap_org_records
              ap_identifiers
      ) = 
        object ["id" .= ap_id ,"created" .= ap_created  
               ,"modified" .= ap_modified ,"uuid" .= ap_uuid   
               ,"name" .= ap_name ,"definition" .= ap_definition 
               ,"stewardship_organisation" .= ap_stewardship_organisation 
               ,"workgroup" .= ap_workgroup  ,"version" .= ap_version     
               ,"references" .= ap_references  ,"origin_URI" .= ap_origin_URI     
               ,"origin" .= ap_origin  ,"comments" .= ap_comments    
               ,"metadatareferencelink_set" .= ap_metadatareferencelink_set  
               ,"slots" .= ap_slots                  
               ,"customvalue_set" .= ap_customvalue_set  
               ,"org_records" .= ap_org_records  
               ,"identifiers" .= ap_identifiers                                                                     
               ]

instance FromJSON AristotleProperty where 
    parseJSON = withObject "AristotleProperty" $ \v -> AristotleProperty  
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
        <*> v .: "metadatareferencelink_set" 
        <*> v .: "slots"
        <*> v .: "customvalue_set" 
        <*> v .: "org_records"         
        <*> v .: "identifiers" 

instance Eq AristotleProperty where
  (AristotleProperty id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
    (AristotleProperty id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleProperty where
  (AristotleProperty id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
    (AristotleProperty id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotlePropertyJSON =
  AristotlePropertyJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleProperty] 
           } deriving (Show,Generic) 

instance Eq AristotlePropertyJSON where 
  (AristotlePropertyJSON c1 n1 p1 r1 ) == (AristotlePropertyJSON c2 n2 p2 r2 )   
      = c1 == c2  
--     = c1 == c2 && n1 == n2 && p1 == p2  && r1 == r2          

instance ToJSON AristotlePropertyJSON 
instance FromJSON AristotlePropertyJSON 

defaultAristotlePropertyJSON = AristotlePropertyJSON 0 Nothing Nothing [] 

data AristotlePropertyOut  = 
  AristotlePropertyOut { 
            p2o_id :: String 
          , p2o_created :: String
          , p2o_modified :: String
          , p2o_uuid :: String
          , p2o_name :: String
          , p2o_definition :: String
          , p2o_stewardship_organisation :: String  
          , p2o_workgroup :: String
          , p2o_version :: String 
          , p2o_references :: String 
          , p2o_origin_URI :: String
          , p2o_origin :: String 
          , p2o_comments :: String
           } deriving (Show,Generic) 

instance FromNamedRecord AristotlePropertyOut
instance ToNamedRecord AristotlePropertyOut
instance DefaultOrdered AristotlePropertyOut 

defaultAristotlePropertyOut = AristotlePropertyOut "" "" "" "" "" "" "" "" "" "" "" "" "" 

ap2apOut :: AristotleProperty -> AristotlePropertyOut  
ap2apOut p = 
       AristotlePropertyOut 
          ((show.ap_id) p)  
          (showUTCTimeDate (ap_created p))         
          (showUTCTimeDate (ap_modified p)) 
          (ap_uuid p) 
          (trim (ap_name p)) 
          (trim (ap_definition p)) 
          (ap_stewardship_organisation p)
          ((show.ap_workgroup) p)
          (ap_version p)
          (trim (ap_references p))
          (trim (ap_origin_URI p)) 
          (ap_origin p)
          (trim (ap_comments p))  


-- extract the list 
getAristotleProperty :: AristotlePropertyJSON -> [AristotleProperty] 
getAristotleProperty jp = sort (results jp) 

-- convert to out  
allAristotlePropertyOut :: AristotlePropertyJSON -> [AristotlePropertyOut] 
allAristotlePropertyOut jp = map ap2apOut (getAristotleProperty jp) 

-- parse 
rb2ap :: ByteString -> Either String AristotlePropertyJSON   
rb2ap = (eitherDecode.ctlChar2SpacesBS) 

-- full extraction from byte string to class out
rb2apOut :: ByteString -> [AristotlePropertyOut] 
rb2apOut rb 
  = allAristotlePropertyOut (fromRight defaultAristotlePropertyJSON (rb2ap rb)) 

