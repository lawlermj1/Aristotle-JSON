{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleIssue
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Issue.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/ 

   Inconsistent JSON Model Typing bug 
   "Error in $.results[1]['workgroup_id']: parsing Int failed, expected Number, but encountered String",
   So, the parsing of this will be skipped. 

   JSON.parse JSON value inconsistent with json model expected Number, but encountered String Issue workgroup_id  51722 a number  Blank String  Use 0 or -1 to hide valid Workgroup Ids.
   See
   https://zzz.aristotlecloud.io/item/51722/issues?isopen=false

   Need to raise with Aristotle. 
   The data has been corrected in the manual file, so this can be used. 

   Use this data to extract UUIDs for Steward Org. 

 -}
module AristotleIssue  
    (  
      
      AristotleOutI( .. ), 
      
      writeAristotleI, 
      web2aoi, 
      file2aoi, 

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

-- used only by Issue  
data AristotleLabelName  = 
  AristotleLabelName { 
            aln_label :: String 
          , aln_pk :: Int
          } deriving (Show,Generic) 

instance ToJSON AristotleLabelName where 
    toJSON (AristotleLabelName aln_label aln_pk  ) = 
        object ["label" .= aln_label ,"pk" .= aln_pk ]

instance FromJSON AristotleLabelName where 
    parseJSON = withObject "AristotleLabelName" $ \v -> AristotleLabelName  
        <$> v .: "label" 
        <*> v .: "pk" 

data AristotleLabelNameOut  = 
  AristotleLabelNameOut { 
            ln2o_concept_id :: String 
          , ln2o_uuid :: String 
          , ln2o_label :: String 
          , ln2o_pk :: String           
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleLabelNameOut
instance ToNamedRecord AristotleLabelNameOut
instance DefaultOrdered AristotleLabelNameOut 

defaultAristotleLabelNameOut = AristotleLabelNameOut "" "" "" "" 

ai2alnOut :: Int -> String -> [AristotleLabelName] -> [AristotleLabelNameOut] 
ai2alnOut concept uuid ps = 
    map (\p -> AristotleLabelNameOut  
                 (show concept) 
                 uuid 
                 (aln_label p)  
                 (show (aln_pk p))      
            ) ps

-- Primary type 
data AristotleIssue  = 
  AristotleIssue { 
            ais_pk :: Int 
          , ais_name :: String
          , ais_description :: String
          , ais_item :: Int  
          , ais_isopen :: Bool                    
          , ais_proposal_field :: String  
-- enum =  name, definition, references, origin, comments 
          , ais_proposal_value :: String            
          , ais_labels :: [Int]
          , ais_label_names :: [AristotleLabelName] 
          , ais_created :: UTCTime           
          , ais_submitter_display_name :: String 
          , ais_submitter_id :: Int 
          , ais_item_statuses_html :: String 
          , ais_stewardship_organisation_name :: String
          , ais_stewardship_organisation_uuid :: String 
          , ais_workgroup_name :: String 
          , ais_workgroup_id :: Int      
          , ais_bumped_date :: UTCTime 
--            "created": "2020-11-12T15:35:40.990312+11:00",  
          , ais_number_of_comments :: Int           
          } deriving (Show,Generic) 

instance ToJSON AristotleIssue where 
    toJSON (AristotleIssue ais_pk ais_name  
           ais_description ais_item  
           ais_isopen ais_proposal_field  
           ais_proposal_value         
           ais_labels 
           ais_label_names  
           ais_created          
           ais_submitter_display_name 
           ais_submitter_id  
           ais_item_statuses_html  
           ais_stewardship_organisation_name  
           ais_stewardship_organisation_uuid  
           ais_workgroup_name ais_workgroup_id  
           ais_bumped_date  
           ais_number_of_comments
      ) = 
        object ["pk" .= ais_pk ,"name" .= ais_name  
               ,"description" .= ais_description ,"item" .= ais_item   
               ,"isopen" .= ais_isopen ,"proposal_field" .= ais_proposal_field 
               ,"proposal_value" .= ais_proposal_value 
               ,"labels" .= ais_labels  
               ,"label_names" .= ais_label_names     
               ,"created" .= ais_created 
               ,"submitter_display_name" .= ais_submitter_display_name  
               ,"submitter_id" .= ais_submitter_id 
               ,"item_statuses_html" .= ais_item_statuses_html    
               ,"stewardship_organisation_name" .= ais_stewardship_organisation_name                             
               ,"stewardship_organisation_uuid" .= ais_stewardship_organisation_uuid 
               ,"workgroup_name" .= ais_workgroup_name ,"workgroup_id" .= ais_workgroup_id 
               ,"bumped_date" .= ais_bumped_date     
               ,"number_of_comments" .= ais_number_of_comments                                                                                 
               ]

instance FromJSON AristotleIssue where 
    parseJSON = withObject "AristotleIssue" $ \v -> AristotleIssue  
        <$> v .: "pk" 
        <*> v .: "name" 
        <*> v .: "description" 
        <*> v .: "item"         
        <*> v .: "isopen"         
        <*> v .: "proposal_field"    
        <*> v .: "proposal_value"   
        <*> v .: "labels"          
        <*> v .: "label_names" 
        <*> v .: "created" 
        <*> v .: "submitter_display_name" 
        <*> v .: "submitter_id" 
        <*> v .: "item_statuses_html" 
        <*> v .: "stewardship_organisation_name" 
        <*> v .: "stewardship_organisation_uuid"         
        <*> v .: "workgroup_name"  
        <*> v .: "workgroup_id"  
        <*> v .: "bumped_date"  
        <*> v .: "number_of_comments"                  

instance Eq AristotleIssue where
  (AristotleIssue id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ) == 
   (AristotleIssue id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 == id2  

instance Ord AristotleIssue where
  (AristotleIssue id1 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) `compare` 
   (AristotleIssue id2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)  
      = id1 `compare` id2  


data AristotleIssueJSON =
  AristotleIssueJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleIssue] 
           } deriving (Show,Generic) 

instance Eq AristotleIssueJSON where 
  (AristotleIssueJSON c1 _ _ _  ) == (AristotleIssueJSON c2 _ _ _ )   
      = c1 == c2 

instance ToJSON AristotleIssueJSON 
instance FromJSON AristotleIssueJSON 

defaultAristotleIssueJSON = AristotleIssueJSON 0 Nothing Nothing [] 

data AristotleIssueOut  = 
  AristotleIssueOut { 
            is2o_pk :: String
          , is2o_name :: String
          , is2o_description :: String
          , is2o_item :: String
          , is2o_isopen :: String
          , is2o_proposal_field :: String
          , is2o_proposal_value :: String
          , is2o_labels :: String
          , is2o_label_names :: String
          , is2o_created :: String
          , is2o_submitter_display_name :: String
          , is2o_submitter_id :: String
          , is2o_item_statuses_html :: String
          , is2o_stewardship_organisation_name :: String
          , is2o_stewardship_organisation_uuid :: String
          , is2o_workgroup_name :: String
          , is2o_workgroup_id :: String
          , is2o_bumped_date :: String
          , is2o_number_of_comments :: String

           } deriving (Show,Generic) 

instance FromNamedRecord AristotleIssueOut
instance ToNamedRecord AristotleIssueOut
instance DefaultOrdered AristotleIssueOut 

defaultAristotleIssueOut = AristotleIssueOut "" "" "" "" "" "" "" "" "" "" "" "" "" "" 

ai2aiOut :: AristotleIssue -> AristotleIssueOut  
ai2aiOut p = 
       AristotleIssueOut 
          ((show.ais_pk) p)  
          (ais_name  p)
          (ais_description  p)
          ((show.ais_item) p)  
          ((show.ais_isopen) p)  
          (ais_proposal_field  p)
          (ais_proposal_value  p)
          (concatMap show (ais_labels p)) 
          (concatMap show (ais_label_names p)) 
          (showUTCTimeDate (ais_created p))           
          (ais_submitter_display_name  p)
          (show (ais_submitter_id  p)) 
          (ais_item_statuses_html  p)
          (ais_stewardship_organisation_name  p)
          (ais_stewardship_organisation_uuid  p)
          (ais_workgroup_name  p)
          (show (ais_workgroup_id  p)) 
          (showUTCTimeDate (ais_bumped_date  p))           
          (show (ais_number_of_comments  p)) 

-- ------------------------
-- Output 
-- extract all primary JSON objects
aij2ai :: [Either String AristotleIssueJSON] -> [AristotleIssue] 
aij2ai flist = concatMap results (rights flist) 

-- convert primary JSON objects into out list 
ai2Out :: [AristotleIssue] -> [AristotleIssueOut] 
ai2Out jList = map ai2aiOut jList   

-- convert JSON objects into label_names dependent list 
ai2lnOut :: [AristotleIssue] -> [AristotleLabelNameOut] 
ai2lnOut ps = concatMap (\p -> ai2alnOut (ais_item p) (show (ais_pk p)) (ais_label_names p)) ps 

-- All Issue output collected into a single type   
data AristotleOutI = 
  AristotleOutI { 
            aoi_AristotleAnyItemType :: AristotleAnyItemType 
          , aoi_AristotleIssue :: [AristotleIssue]   
          , aoi_AristotleProcessResults :: AristotleProcessResults 
          , aoi_AristotleIssueOut :: [AristotleIssueOut] 
          , aoi_AristotleIssueLabelNameOut :: [AristotleLabelNameOut] 
          } deriving (Show,Generic)  

defaultAristotleOutI = AristotleOutI Issue [] defaultAristotleProcessResults  [] [] 

populateAristotleOutI :: AristotleAnyItemType -> [AristotleIssue] -> AristotleProcessResults -> AristotleOutI 
populateAristotleOutI aait ps apr 
  = AristotleOutI aait ps apr (ai2Out ps) (ai2lnOut ps) 

-- full proc  
getAristotleI :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutI 
getAristotleI aait fetched mts mte 
  = populateAristotleOutI aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = aij2ai parsed 
      ajc = count (safeEither defaultAristotleIssueJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleIssueJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleI :: AristotleOutI -> String -> IO ()  
writeAristotleI aoi fp 
  = do 
  if length (aoi_AristotleIssueOut aoi) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aoi_AristotleIssueOut aoi))  
  else return ()  

  if length (aoi_AristotleIssueLabelNameOut aoi) > 0 then 
    B.writeFile (aait2filename aait fp "LabelName") (encodeDefaultOrderedByName (aoi_AristotleIssueLabelNameOut aoi))  
  else return ()  

      where aait = (aoi_AristotleAnyItemType aoi) 

-- wrapper for the I web request 
web2aoi :: W.Options -> Int -> IO AristotleOutI 
web2aoi opts1 limitpage  = 
  do 
   let aait = Issue 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 0)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleI aait fetched mts mte)  

-- wrapper for the I file read and write  
file2aoi fI aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((ai2Out.aij2ai) [(rb2Aristotle fI)] ))  
      

