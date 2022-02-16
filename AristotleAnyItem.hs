{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleAnyItem
   Description : This contains specific JSON and CSV parsing functions for the generic or 
               boilerplate Aristotle Any Item Class, and boilerplate child JSON objects. 
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/

   This can be used for all items. 
   This will reduce the amount of maintained code. 

   AnyItem is complete - has identical fields - for 
    DataType
    ObjectClass
    Property
    DataElementConcept
    DataElement 

   Separate unique fields for these from the boilerplate? 
    Relation
    ObjectClassSpecialisation
    DataSetSpecification
    ValueDomain
    Distribution

-- for common shared or child JSONs 
-- correct structure defined in JSON models on site at
-- https://aristotle.cloud/api/v4/ 
-- so no need for dummies added to discover parse errors and correct structure
-- These were not used at all in the SA data, so were untested. 

-- So far, distribution is the only use of AristotleMetadatarReferenceLink.  
-- Also, AristotleIdentifier always has a cardinality of 1, so an out was not needed. 
-- But this is created anyway.      

Flattened ar_identifiers :: [AristotleIdentifier] 

Create child exports for multiple values for - cardinality > 2 
ar_customvalue_set :: [AristotleCustomValue] 

Use of unsafe functions head and last fixed with defaults 

 -}

module AristotleAnyItem  
    (  
      
      AristotleProcessResults( .. ), 
      AristotleOutAll( .. ),  
      AristotleAnyItemName( .. ), 

      concatAristotleOutAll, 
      web2aoa, 
      writeAristotle,
      anyItemType2Name, 

     ) where
    
import Data.Aeson
import Data.Either 
import Data.List 
import Data.Maybe  
import Data.Time  
import Data.Data 
import GHC.Generics
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered, encodeDefaultOrderedByName, parseField, toField) 
import qualified Data.ByteString.Lazy as BL  
import qualified Network.Wreq as W 

import CassavaUtils 
import AristotleCommon  

-- --------------
-- Child table types 
-- Shared JSON product types 
data AristotleMetadatarReferenceLink = 
  AristotleMetadatarReferenceLink   { 
            mrl_description :: String
          , mrl_reference :: Int            
          , mrl_id :: Int 
          , mrl_order :: Int                      
           } deriving (Show,Generic) 

instance ToJSON AristotleMetadatarReferenceLink where 
    toJSON (AristotleMetadatarReferenceLink mrl_description mrl_reference mrl_id mrl_order) = 
        object [
                "description" .= mrl_description 
               ,"reference" .= mrl_reference         
               ,"id" .= mrl_id 
               ,"order" .= mrl_order 
               ]

instance FromJSON AristotleMetadatarReferenceLink where 
    parseJSON = withObject "AristotleMetadatarReferenceLink" $ \v -> AristotleMetadatarReferenceLink 
        <$> v .: "description" 
        <*> v .: "reference" 
        <*> v .: "id"  
        <*> v .: "order" 

defaultAristotleMetadatarReferenceLink = AristotleMetadatarReferenceLink "" 0 0 0        

data AristotleMetadatarReferenceLinkOut  = 
  AristotleMetadatarReferenceLinkOut { 
            mdl2o_AnyItemType :: String   
          , mdl2o_concept_id :: String 
          , mdl2o_uuid :: String
          , mdl2o_description :: String 
          , mdl2o_reference :: String 
          , mdl2o_id :: String 
          , mdl2o_order :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleMetadatarReferenceLinkOut
instance ToNamedRecord AristotleMetadatarReferenceLinkOut
instance DefaultOrdered AristotleMetadatarReferenceLinkOut 

defaultAristotleMetadatarReferenceLinkOut = AristotleMetadatarReferenceLinkOut "" "" "" "" "" "" ""  

amdl2amdlOut :: AristotleAnyItemType -> Int -> String -> [AristotleMetadatarReferenceLink] -> [AristotleMetadatarReferenceLinkOut] 
amdl2amdlOut aait concept uuid ps = 
    map (\p -> AristotleMetadatarReferenceLinkOut 
                 (show aait) 
                 (show concept) 
                 uuid 
                 (mrl_description p) 
                 (show (mrl_reference p))  
                 (show (mrl_id p))                    
                 (show (mrl_order p))                    
            ) ps

-- ----------------------
data AristotleSlot = 
  AristotleSlot   { 
            as_id :: Int
          , as_name :: String 
          , as_type :: String 
          , as_value :: String   
          , as_order :: Int 
          , as_permission :: Int           
           } deriving (Show,Generic) 

instance ToJSON AristotleSlot where 
    toJSON (AristotleSlot as_id as_name as_type as_value as_order as_permission) = 
        object ["id" .= as_id ,"name" .= as_name  
               ,"type" .= as_type ,"value" .= as_value 
               ,"order" .= as_order  ,"permission" .= as_permission        
               ]

instance FromJSON AristotleSlot where 
    parseJSON = withObject "AristotleSlot" $ \v -> AristotleSlot 
        <$> v .: "id" 
        <*> v .: "name" 
        <*> v .: "type"         
        <*> v .: "value" 
        <*> v .: "order" 
        <*> v .: "permission" 

defaultAristotleSlot = AristotleSlot 0 "" "" "" 0 0          

data AristotleSlotOut  = 
  AristotleSlotOut { 
            s2o_AnyItemType :: String   
          , s2o_concept_id :: String 
          , s2o_uuid :: String
          , s2o_id :: String          
          , s2o_name :: String
          , s2o_type :: String
          , s2o_value :: String 
          , s2o_order :: String 
          , s2o_permission :: String                     
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleSlotOut
instance ToNamedRecord AristotleSlotOut
instance DefaultOrdered AristotleSlotOut 

defaultAristotleSlotOut = AristotleSlotOut "" "" "" "" "" "" "" "" "" 

as2asOut :: AristotleAnyItemType -> Int -> String -> [AristotleSlot] -> [AristotleSlotOut] 
as2asOut aait concept uuid ps = 
    map (\p -> AristotleSlotOut  
                 (show aait) 
                 (show concept) 
                 uuid 
                 (show (as_id p))   
                 (as_name p) 
                 (as_type p)  
                 (as_value p) 
                 (show (as_order p))   
                 (show (as_permission p))                    
            ) ps

-- ----------------------
data AristotleCustomValue = 
  AristotleCustomValue   { 
            cv_id :: Int
          , cv_name :: String 
          , cv_field ::  Int          
          , cv_content :: String                        
           } deriving (Show,Generic) 

instance ToJSON AristotleCustomValue where 
    toJSON (AristotleCustomValue cv_id cv_name cv_field cv_content ) = 
        object ["id" .= cv_id ,"name" .= cv_name 
               ,"field" .= cv_field ,"content" .= cv_content 
               ]

instance FromJSON AristotleCustomValue where 
    parseJSON = withObject "AristotleCustomValue" $ \v -> AristotleCustomValue 
        <$> v .: "id" 
        <*> v .: "name" 
        <*> v .: "field"                 
        <*> v .: "content" 

defaultAristotleCustomValue = AristotleCustomValue 0 "" 0 ""    

data AristotleCustomValueOut  = 
  AristotleCustomValueOut { 
            cv2o_AnyItemType :: String   
          , cv2o_concept_id :: String 
          , cv2o_uuid :: String
          , cv2o_id :: String          
          , cv2o_name :: String
          , cv2o_field :: String
          , cv2o_content :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleCustomValueOut
instance ToNamedRecord AristotleCustomValueOut
instance DefaultOrdered AristotleCustomValueOut 

defaultAristotleCustomValueOut = AristotleCustomValueOut "" "" "" "" "" "" ""   

acvs2acvsOut :: AristotleAnyItemType -> Int -> String -> [AristotleCustomValue] -> [AristotleCustomValueOut] 
acvs2acvsOut aait concept uuid ps = 
    map (\p -> AristotleCustomValueOut
                 (show aait) 
                 (show concept) 
                 uuid 
                 ((show.cv_id) p)  
                 (cv_name p) 
                 ((show.cv_field) p)  
                 (cv_content p) 
            ) ps

-- ----------------------
-- {"id":108,"organization_record":44,"type":"r","order":0}
data AristotleOrgRecord = 
  AristotleOrgRecord   { 
            or_id :: Int
          , or_organization_record :: Int 
          , or_type :: String          
          , or_order :: Int                             
           } deriving (Show,Generic) 

instance ToJSON AristotleOrgRecord where 
    toJSON (AristotleOrgRecord or_id or_organization_record or_type or_order ) = 
        object ["id" .= or_id ,"organization_record" .= or_organization_record 
               ,"type" .= or_type ,"order" .= or_order 
               ]

instance FromJSON AristotleOrgRecord where 
    parseJSON = withObject "AristotleOrgRecord" $ \v -> AristotleOrgRecord 
        <$> v .: "id" 
        <*> v .: "organization_record" 
        <*> v .: "type"   
        <*> v .: "order"    

defaultAristotleOrgRecord = AristotleOrgRecord 0 0 "" 0          
        
data AristotleOrgRecordOut  = 
  AristotleOrgRecordOut { 
            or2o_AnyItemType :: String   
          , or2o_concept_id :: String 
          , or2o_uuid :: String
          , or2o_id :: String          
          , or2o_organization_record :: String
          , or2o_type :: String
          , or2o_order :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleOrgRecordOut
instance ToNamedRecord AristotleOrgRecordOut
instance DefaultOrdered AristotleOrgRecordOut 

defaultAristotleOrgRecordOut = AristotleOrgRecordOut "" "" "" "" "" "" ""   

aor2aorOut :: AristotleAnyItemType -> Int -> String -> [AristotleOrgRecord] -> [AristotleOrgRecordOut] 
aor2aorOut aait concept uuid ps = 
    map (\p -> AristotleOrgRecordOut  
                 (show aait) 
                 (show concept) 
                 uuid 
                 ((show.or_id) p)  
                 ((show.or_organization_record) p) 
                 (or_type p)  
                 ((show.or_order) p) 
            ) ps

-- ----------------------
data AristotleIdentifier = 
  AristotleIdentifier   { 
            ai_id :: String
          , ai_namespace :: Int 
          , ai_identifier :: String          
          , ai_version :: String                        
          , ai_order :: Int 
           } deriving (Show,Generic) 

instance ToJSON AristotleIdentifier where 
    toJSON (AristotleIdentifier ai_id ai_namespace ai_identifier ai_version ai_order) = 
        object ["id" .= ai_id ,"namespace" .= ai_namespace  
               ,"identifier" .= ai_identifier ,"version" .= ai_version   
               ,"order" .= ai_order        
               ]

instance FromJSON AristotleIdentifier where 
    parseJSON = withObject "AristotleIdentifier" $ \v -> AristotleIdentifier 
        <$> v .: "id" 
        <*> v .: "namespace" 
        <*> v .: "identifier"                 
        <*> v .: "version" 
        <*> v .: "order"   

defaultAristotleIdentifier = AristotleIdentifier "" 0 "" "" 0      

data AristotleIdentifierOut  = 
  AristotleIdentifierOut { 
            i2o_AnyItemType :: String   
          , i2o_concept_id :: String 
          , i2o_uuid :: String
          , i2o_id :: String 
          , i2o_namespace :: String 
          , i2o_identifier :: String           
          , i2o_version :: String 
          , i2o_order :: String           
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleIdentifierOut
instance ToNamedRecord AristotleIdentifierOut
instance DefaultOrdered AristotleIdentifierOut 

defaultAristotleIdentifierOut = AristotleIdentifierOut "" "" "" "" "" "" "" ""  

ai2aiOut :: AristotleAnyItemType -> Int -> String -> [AristotleIdentifier] -> [AristotleIdentifierOut] 
ai2aiOut aait concept uuid ps = 
    map (\p -> AristotleIdentifierOut  
                 (show aait) 
                 (show concept) 
                 uuid 
                 (ai_id p) 
                 (show (ai_namespace p))  
                 (ai_identifier p)  
                 (ai_version p)                                    
                 (show (ai_order p))                    
            ) ps


-- ----------------------
-- Parsing and Input 
data AristotleAnyItem  = 
  AristotleAnyItem { 
            aai_id :: Int 
          , aai_created :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",          
          , aai_modified :: UTCTime
          , aai_uuid :: String
          , aai_name :: String
          , aai_definition :: String
          , aai_stewardship_organisation :: String  
          , aai_workgroup :: Maybe Int
          , aai_version :: String 
          , aai_references :: String 
          , aai_origin_URI :: String
          , aai_origin :: String 
          , aai_comments :: String
          , aai_metadatareferencelink_set :: [AristotleMetadatarReferenceLink] 
          , aai_slots :: [AristotleSlot] 
          , aai_customvalue_set :: [AristotleCustomValue] 
          , aai_org_records :: [AristotleOrgRecord]           
          , aai_identifiers :: [AristotleIdentifier]   
          } deriving (Show,Generic) 

instance ToJSON AristotleAnyItem where 
    toJSON (AristotleAnyItem 
              aai_id aai_created aai_modified aai_uuid aai_name
              aai_definition aai_stewardship_organisation aai_workgroup aai_version 
              aai_references aai_origin_URI aai_origin aai_comments 
              aai_metadatareferencelink_set 
              aai_slots aai_customvalue_set 
              aai_org_records
              aai_identifiers
      ) = 
        object ["id" .= aai_id ,"created" .= aai_created  
               ,"modified" .= aai_modified ,"uuid" .= aai_uuid   
               ,"name" .= aai_name ,"definition" .= aai_definition 
               ,"stewardship_organisation" .= aai_stewardship_organisation 
               ,"workgroup" .= aai_workgroup  ,"version" .= aai_version     
               ,"references" .= aai_references  ,"origin_URI" .= aai_origin_URI     
               ,"origin" .= aai_origin  ,"comments" .= aai_comments 
               ,"metadatareferencelink_set" .= aai_metadatareferencelink_set                 
               ,"slots" .= aai_slots ,"customvalue_set" .= aai_customvalue_set  
               ,"org_records" .= aai_org_records 
               ,"identifiers" .= aai_identifiers                                                                     
               ]

instance FromJSON AristotleAnyItem where 
    parseJSON = withObject "AristotleAnyItem" $ \v -> AristotleAnyItem  
        <$> v .: "id" 
        <*> v .: "created" 
        <*> v .: "modified" 
        <*> v .: "uuid" 
        <*> v .: "name" 
        <*> v .: "definition" 
        <*> v .: "stewardship_organisation" 
        <*> v .:? "workgroup" 
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

instance Eq AristotleAnyItem where
  (AristotleAnyItem id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) == 
   (AristotleAnyItem id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleAnyItem where
  (AristotleAnyItem id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
   (AristotleAnyItem id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  

data AristotleAnyItemJSON =
  AristotleAnyItemJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleAnyItem] 
           } deriving (Show,Generic) 

instance Eq AristotleAnyItemJSON where 
  (AristotleAnyItemJSON c1 _ _ _ ) == (AristotleAnyItemJSON c2 _ _ _ )   
      = c1 == c2 

instance ToJSON AristotleAnyItemJSON 
instance FromJSON AristotleAnyItemJSON 

defaultAristotleAnyItemJSON = AristotleAnyItemJSON 0 Nothing Nothing [] 

getIds :: AristotleAnyItemJSON -> [Int] 
getIds aaij = map aai_id (results aaij) 

-- container for AristotleAnyItemType and results for use in process + output 
data AristotleAnyItemParse = 
  AristotleAnyItemParse {
            aaip_AnyItemType :: AristotleAnyItemType 
          , aaip_AristotleAnyItemList :: [AristotleAnyItem] 
           } deriving (Show,Generic) 

defaultAristotleAnyItemParse = AristotleAnyItemParse AnyItem [] 

-- ----------------------
-- Process audit + Test Properties 
-- Test Property  
data AristotleTestProperty  = 
  AristotleTestProperty {  
            aai_metadatareferencelink_set_max :: Int 
          , aai_slots_max :: Int 
          , aai_customvalue_set_max :: Int 
          , aai_org_records_max :: Int 
          , aai_identifiers_max :: Int 
          , aai_metadatareferencelink_set_varchar_max :: Int 
          , aai_slots_varchar_max :: Int 
          , aai_customvalue_set_varchar_max :: Int 
          , aai_org_records_varchar_max :: Int 
          , aai_identifiers_varchar_max :: Int 
          } deriving (Show,Generic) 

defaultAristotleTestProperty = AristotleTestProperty 0 0 0 0 0 0 0 0 0 0 

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
aaifold :: AristotleTestProperty -> AristotleAnyItem -> AristotleTestProperty 
aaifold atp aai = 
    AristotleTestProperty 
        (max (aai_metadatareferencelink_set_max atp) (length (aai_metadatareferencelink_set aai))) 
        (max (aai_slots_max atp) (length (aai_slots aai))) 
        (max (aai_customvalue_set_max atp) (length (aai_customvalue_set aai))) 
        (max (aai_org_records_max atp) (length (aai_org_records aai))) 
        (max (aai_identifiers_max atp) (length (aai_identifiers aai))) 
        (max (aai_metadatareferencelink_set_varchar_max atp) (length (concatMap show (aai_metadatareferencelink_set aai))))  
        (max (aai_slots_varchar_max atp) (length (concatMap show (aai_slots aai))))  
        (max (aai_customvalue_set_varchar_max atp) (length (concatMap show (aai_customvalue_set aai))))  
        (max (aai_org_records_varchar_max atp) (length (concatMap show (aai_org_records aai))))  
        (max (aai_identifiers_varchar_max atp) (length (concatMap show (aai_identifiers aai))))  


-- ----------------------
-- Process audit and tests
-- Process status  
writeAristotleProcessResults :: AristotleAnyItemType -> String -> String -> [Either String AristotleAnyItemJSON] 
  -> AristotleAnyItemParse -> AristotleProcessResults 
writeAristotleProcessResults aait mts mte parsed (AristotleAnyItemParse _ ps) = 
  AristotleProcessResults 
    aait (aait2http aait 100) mts mte 
    (show (nub (map checkParse parsed)))  
    (prop_parse_ok parsed)  
    ajc 
    (length ps) 
    (prop_count_to_parsed ajc ps) 
      where ajc = (count (safeEither defaultAristotleAnyItemJSON parsed))       

-- ----------------------
-- Output 
data AristotleAnyItemOut  = 
  AristotleAnyItemOut { 
            ai2o_AnyItemType :: String     
          , ai2o_id :: String 
          , ai2o_created :: String
          , ai2o_modified :: String
          , ai2o_uuid :: String
          , ai2o_name :: String
          , ai2o_definition :: String
          , ai2o_stewardship_organisation :: String  
          , ai2o_workgroup :: String
          , ai2o_version :: String 
          , ai2o_references :: String 
          , ai2o_origin_URI :: String
          , ai2o_origin :: String 
          , ai2o_comments :: String

          , ai2o_metadatareferencelink_set :: String
          , ai2o_slots :: String
          , ai2o_customvalue_set :: String
          , ai2o_org_records :: String         
          , ai2o_identifiers :: String  
-- flattening of ai2o_identifiers. Cardinality assumed to be 1 
          , ai2o_ai_id :: String
          , ai2o_ai_namespace :: String 
          , ai2o_ai_identifier :: String          
          , ai2o_ai_version :: String                        
          , ai2o_ai_order :: String              
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleAnyItemOut
instance ToNamedRecord AristotleAnyItemOut
instance DefaultOrdered AristotleAnyItemOut 

defaultAristotleAnyItemOut = AristotleAnyItemOut "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""    

-- create out record  
aai2aaiOut :: Int -> AristotleAnyItemType -> AristotleAnyItem -> AristotleAnyItemOut  
aai2aaiOut cl et p = 
       AristotleAnyItemOut 
          (show et)  
          ((show.aai_id) p)  
          (showUTCTimeDate (aai_created p))           
          (showUTCTimeDate (aai_modified p))  
          (aai_uuid p) 
          (trim (aai_name p))
          (trim (aai_definition p))
          (aai_stewardship_organisation p)
          (show (fromMaybe 0 (aai_workgroup p))) 
          (aai_version p)
          (trim (aai_references p))
          (aai_origin_URI p)
          (trim (aai_origin p))
          (trim (aai_comments p))  
          (if (length allmdl) < cl then allmdl 
             else "! aai_metadatareferencelink_set too many chars - see child type for all - 30,000 char truncate = " ++ take cl allmdl ) 
          (if (length alls) < cl then alls 
             else "! aai_slots too many chars - see child type for all - 30,000 char truncate = " ++ take cl alls )
          (if (length allcvs) < cl then allcvs 
             else "! aai_customvalue_set too many chars - see child type for all - 30,000 char truncate = " ++ take cl allcvs )
          (if (length allor) < cl then allor 
             else "! aai_org_records too many chars - see child type for all - 30,000 char truncate = " ++ take cl allor ) 
          (if (length alli) < cl then alli 
             else "! aai_identifiers too many chars - see child type for all - 30,000 char truncate = " ++ take cl alli )  
          (ai_id aip0) 
          (show (ai_namespace aip0))  
          (ai_identifier aip0)     
          (ai_version aip0)                    
          (show (ai_order aip0)) 
             where 
                aip = (aai_identifiers p) 
                aip0 = if length aip == 1 then head aip else defaultAristotleIdentifier 
                allmdl = (concatMap show (aai_metadatareferencelink_set p))                  
                alls = (concatMap show (aai_slots p))   
                allcvs = (concatMap show (aai_customvalue_set p))   
                allor = (concatMap show (aai_org_records p))   
                alli = (concatMap show (aai_identifiers p))   


-- extract all primary Aristotle JSON objects using the results field 
aaij2aai :: [Either String AristotleAnyItemJSON] -> [AristotleAnyItem] 
aaij2aai flist = concatMap results (rights flist) 

-- place parsed and type into container 
aai2aaip :: AristotleAnyItemType -> [AristotleAnyItem] -> AristotleAnyItemParse
aai2aaip aait as = AristotleAnyItemParse aait as 

-- convert primary JSON objects into out list 
-- 30000 char limit for CVS - may change for DB 
aai2Out :: AristotleAnyItemParse -> [AristotleAnyItemOut] 
aai2Out (AristotleAnyItemParse aait ps) = map (aai2aaiOut 30000 aait) ps 

-- convert JSON objects into aai_metadatareferencelink_set dependent list 
aai2mdlOut :: AristotleAnyItemParse -> [AristotleMetadatarReferenceLinkOut] 
aai2mdlOut (AristotleAnyItemParse aait ps) = concatMap (\p -> amdl2amdlOut aait (aai_id p) (aai_uuid p) (aai_metadatareferencelink_set p)) ps 

-- convert JSON objects into aai_slots dependent list 
aai2ssOut :: AristotleAnyItemParse -> [AristotleSlotOut] 
aai2ssOut (AristotleAnyItemParse aait ps) = concatMap (\p -> as2asOut aait (aai_id p) (aai_uuid p) (aai_slots p)) ps 

-- convert JSON objects into custom value dependent list 
aai2cvsOut :: AristotleAnyItemParse -> [AristotleCustomValueOut] 
aai2cvsOut (AristotleAnyItemParse aait ps) = concatMap (\p -> acvs2acvsOut aait (aai_id p) (aai_uuid p) (aai_customvalue_set p)) ps 

-- convert JSON objects into aai_org_records dependent list 
aai2orOut :: AristotleAnyItemParse -> [AristotleOrgRecordOut] 
aai2orOut (AristotleAnyItemParse aait ps) = concatMap (\p -> aor2aorOut aait (aai_id p) (aai_uuid p) (aai_org_records p)) ps 

-- convert JSON objects into aai_identifiers dependent list 
aai2iOut :: AristotleAnyItemParse -> [AristotleIdentifierOut] 
aai2iOut (AristotleAnyItemParse aait ps) = concatMap (\p -> ai2aiOut aait (aai_id p) (aai_uuid p) (aai_identifiers p)) ps 

-- All boilerplate output collected into a single type   
data AristotleOutAll = 
  AristotleOutAll { 
            aoa_AristotleAnyItemType :: AristotleAnyItemType 
          , aoa_AristotleAnyItemParse :: AristotleAnyItemParse   
          , aoa_AristotleProcessResults :: AristotleProcessResults  
          , aoa_AristotleAnyItemOut :: [AristotleAnyItemOut] 
          , aoa_AristotleMetadatarReferenceLinkOut :: [AristotleMetadatarReferenceLinkOut] 
          , aoa_AristotleSlotOut :: [AristotleSlotOut] 
          , aoa_AristotleCustomValueOut :: [AristotleCustomValueOut] 
          , aoa_AristotleOrgRecordOut :: [AristotleOrgRecordOut] 
          , aoa_AristotleIdentifierOut :: [AristotleIdentifierOut] 
          , aoa_AristotleTestProperty :: AristotleTestProperty  
          , aoa_AristotleParseResult :: [ParseResult]                 
          } deriving (Show,Generic)  

defaultAristotleOutAll = AristotleOutAll AnyItem defaultAristotleAnyItemParse defaultAristotleProcessResults  
                         [] [] [] [] [] [] defaultAristotleTestProperty []

populateAristotleOutAll :: AristotleAnyItemParse -> AristotleProcessResults -> [FetchResult] -> AristotleOutAll 
populateAristotleOutAll aaip apr frs 
  = AristotleOutAll (aaip_AnyItemType aaip) aaip apr (aai2Out aaip) (aai2mdlOut aaip) (aai2ssOut aaip) 
       (aai2cvsOut aaip) (aai2orOut aaip) (aai2iOut aaip) 
       (foldl aaifold defaultAristotleTestProperty (aaip_AristotleAnyItemList aaip))  
       (parseList getIds frs )

concatAristotleOutAll :: [AristotleOutAll] -> AristotleOutAll 
concatAristotleOutAll as = 
  defaultAristotleOutAll { 
     aoa_AristotleAnyItemOut = concat (map aoa_AristotleAnyItemOut as) 
   , aoa_AristotleMetadatarReferenceLinkOut = concat (map aoa_AristotleMetadatarReferenceLinkOut as) 
   , aoa_AristotleSlotOut = concat (map aoa_AristotleSlotOut as) 
   , aoa_AristotleCustomValueOut = concat (map aoa_AristotleCustomValueOut as) 
   , aoa_AristotleOrgRecordOut = concat (map aoa_AristotleOrgRecordOut as) 
   , aoa_AristotleIdentifierOut = concat (map aoa_AristotleIdentifierOut as)    
}

-- Just the name 
data AristotleAnyItemName  = 
  AristotleAnyItemName { 
            ai2on_AnyItemType :: String    
          , ai2on_name :: String       
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleAnyItemName
instance ToNamedRecord AristotleAnyItemName
instance DefaultOrdered AristotleAnyItemName 

defaultAristotleAnyItemName = AristotleAnyItemName "" "" 

-- gets name only 
anyItemType2Name :: AristotleAnyItemOut -> AristotleAnyItemName 
anyItemType2Name ar = AristotleAnyItemName (ai2o_AnyItemType ar) (ai2o_name ar ) 


-- full proc  
getAristotleAnyItem :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutAll 
getAristotleAnyItem aait fetched mts mte 
  = populateAristotleOutAll collected apr fetched 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = aai2aaip aait (aaij2aai parsed) 
      apr = writeAristotleProcessResults aait mts mte parsed collected  


-- writes the AnyItem Aristotle to up to 6 separate CSV files 
-- it writes out a file if the count > 0  
writeAristotle :: AristotleOutAll -> String -> IO ()  
writeAristotle aoa fp 
  = do 
  if length (aoa_AristotleAnyItemOut aoa) > 0 then 
    BL.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aoa_AristotleAnyItemOut aoa))  
  else return ()   

  if length (aoa_AristotleMetadatarReferenceLinkOut aoa) > 0 then 
    BL.writeFile (aait2filename aait fp "MetadatarReferenceLink") (encodeDefaultOrderedByName (aoa_AristotleMetadatarReferenceLinkOut aoa))  
  else return ()   

  if length (aoa_AristotleSlotOut aoa) > 0 then 
    BL.writeFile (aait2filename aait fp "Slot") (encodeDefaultOrderedByName (aoa_AristotleSlotOut aoa))  
  else return ()   

  if length (aoa_AristotleCustomValueOut aoa) > 0 then 
    BL.writeFile (aait2filename aait fp "CustomValue") (encodeDefaultOrderedByName (aoa_AristotleCustomValueOut aoa))  
  else return ()   

  if length (aoa_AristotleOrgRecordOut aoa) > 0 then 
    BL.writeFile (aait2filename aait fp "OrgRecord") (encodeDefaultOrderedByName (aoa_AristotleOrgRecordOut aoa))  
  else return ()    

  if length (aoa_AristotleIdentifierOut aoa) > 0 then 
    BL.writeFile (aait2filename aait fp "Identifier") (encodeDefaultOrderedByName (aoa_AristotleIdentifierOut aoa))  
  else return ()    
    where aait = (aoa_AristotleAnyItemType aoa) 

-- wrapper for the boilerplate request 
-- reduce pagesize to 5 for finding parse errors 
-- increase pagesize to 100 for normal extraction 
web2aoa :: W.Options -> Int -> Int -> AristotleAnyItemType -> IO AristotleOutAll 
web2aoa opts1 limitpage pagesize aait = 
  do 
-- fetching      100 
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait pagesize)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleAnyItem aait fetched mts mte)  
