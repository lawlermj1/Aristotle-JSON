{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
   Module      : AristotleRelation
   Description : This contains specific JSON and CSV parsing functions for the Aristotle Relation.  
   Copyright   : ( c ) Matthew Lawler 2021  
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/

---
Done 
AristotleRelationRole also has a fixed cardinality of 2. 
Flattened these 
          , ar_relationrole_set :: [AristotleRelationRole] 

Use of unsafe functions head and last fixed with defaults 

---
Not done 
Test for 
  length ar_relationrole_set <= 2  

 -}
module AristotleRelation  
    (  
      
      AristotleOutR( .. ), 
      
      writeAristotleR, 
      web2aor, 
      file2aor, 

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


-- used ONLY in Relation 
data AristotleRelationRole = 
  AristotleRelationRole   { 
            arr_id :: String
          , arr_name :: String 
          , arr_definition :: String          
          , arr_multiplicity :: Int                        
          , arr_ordinal :: Int 
           } deriving (Show,Generic) 

instance ToJSON AristotleRelationRole where 
    toJSON (AristotleRelationRole arr_id arr_name arr_definition arr_multiplicity arr_ordinal) = 
        object ["id" .= arr_id ,"name" .= arr_name  
               ,"definition" .= arr_definition ,"multiplicity" .= arr_multiplicity   
               ,"ordinal" .= arr_ordinal        
               ]

instance FromJSON AristotleRelationRole where 
    parseJSON = withObject "AristotleRelationRole" $ \v -> AristotleRelationRole 
        <$> v .: "id" 
        <*> v .: "name" 
        <*> v .: "definition"                 
        <*> v .: "multiplicity" 
        <*> v .: "ordinal"   

defaultAristotleRelationRole = AristotleRelationRole "" "" "" 0 0 

data AristotleRelationRoleOut  = 
  AristotleRelationRoleOut { 
            rr2o_concept_id :: String 
          , rr2o_uuid :: String
          , rr2o_arr_id :: String 
          , rr2o_arr_name :: String 
          , rr2o_arr_definition :: String          
          , rr2o_arr_multiplicity :: String                        
          , rr2o_arr_ordinal :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleRelationRoleOut
instance ToNamedRecord AristotleRelationRoleOut
instance DefaultOrdered AristotleRelationRoleOut 

defaultAristotleRelationRoleOut = AristotleRelationRoleOut "" "" "" "" "" "" ""

arr2arrOut :: Int -> String -> [AristotleRelationRole] -> [AristotleRelationRoleOut] 
arr2arrOut concept uuid ps = 
    map (\p -> AristotleRelationRoleOut  
                 (show concept) 
                 uuid 
                 (arr_id p)  
                 (arr_name p) 
                 (arr_definition p)  
                 (show (arr_multiplicity p)) 
                 (show (arr_ordinal p))                                                  
            ) ps 

-- Primary Type 
data AristotleRelation  = 
  AristotleRelation { 
            ar_id :: Int 
          , ar_uuid :: String 
          , ar_relationrole_set :: [AristotleRelationRole]  
          } deriving (Show,Generic) 

instance ToJSON AristotleRelation where 
    toJSON (AristotleRelation ar_id ar_uuid ar_relationrole_set ) = 
        object ["id" .= ar_id ,"uuid" .= ar_uuid 
               ,"relationrole_set" .= ar_relationrole_set ]

instance FromJSON AristotleRelation where 
    parseJSON = withObject "AristotleRelation" $ \v -> AristotleRelation  
        <$> v .: "id" 
        <*> v .: "uuid" 
        <*> v .: "relationrole_set"  

instance Eq AristotleRelation where
  (AristotleRelation id1 _ _ ) == (AristotleRelation id2 _ _ ) = id1 == id2  

instance Ord AristotleRelation where
  (AristotleRelation id1 _ _ ) `compare` (AristotleRelation id2 _ _ ) = id1 `compare` id2  


data AristotleRelationJSON =
  AristotleRelationJSON { 
            count :: Int
          , next :: Maybe String    
          , prev :: Maybe String                   
          , results :: [AristotleRelation] 
           } deriving (Show,Generic) 

instance Eq AristotleRelationJSON where 
  (AristotleRelationJSON c1 _ _ _ ) == (AristotleRelationJSON c2 _ _ _ )   
      = c1 == c2 

instance ToJSON AristotleRelationJSON 
instance FromJSON AristotleRelationJSON 

defaultAristotleRelationJSON = AristotleRelationJSON 0 Nothing Nothing [] 

data AristotleRelationOut  = 
  AristotleRelationOut { 
            r2o_id :: String 
          , r2o_uuid :: String 
          , r2o_relationrole_set :: String  
-- flattening of r2o_relationrole_set. Cardinality assumed to be 1 or 2                 
          , r2o_0_arr_id :: String 
          , r2o_0_arr_name :: String 
          , r2o_0_arr_definition :: String          
          , r2o_0_arr_multiplicity :: String                        
          , r2o_0_arr_ordinal :: String 
          , r2o_1_arr_id :: String 
          , r2o_1_arr_name :: String 
          , r2o_1_arr_definition :: String 
          , r2o_1_arr_multiplicity :: String 
          , r2o_1_arr_ordinal :: String 
           } deriving (Show,Generic) 

instance FromNamedRecord AristotleRelationOut
instance ToNamedRecord AristotleRelationOut
instance DefaultOrdered AristotleRelationOut 

defaultAristotleRelationOut = AristotleRelationOut "" "" "" "" "" "" "" "" "" "" "" "" "" 

ar2arOut :: AristotleRelation -> AristotleRelationOut  
ar2arOut p = 
       AristotleRelationOut 
          ((show.ar_id) p)  
          (ar_uuid p) 
          (concatMap show (ar_relationrole_set p))   
          (arr_id arr0) 
          (arr_name arr0)           
          (arr_definition arr0)   
          (show (arr_multiplicity arr0))  
          (show (arr_ordinal arr0))    
          (arr_id arr1) 
          (arr_name arr1)           
          (arr_definition arr1)   
          (show (arr_multiplicity arr1))  
          (show (arr_ordinal arr1))    
             where 
-- ordinal 0 and 1 
                arr = (ar_relationrole_set p) 
                arr0 = if length arr >= 1 then head arr else defaultAristotleRelationRole 
                arr1 = if length arr == 2 then last arr else defaultAristotleRelationRole 

-- ------------------------
-- Testing 
-- Cardinality 
data AristotleRelationCardinality  = 
  AristotleRelationCardinality { 
            ar_relationrole_set_max :: Int  
          } deriving (Show,Generic) 

defaultAristotleRelationCardinality = AristotleRelationCardinality 0  

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
arfold :: AristotleRelationCardinality -> AristotleRelation -> AristotleRelationCardinality
arfold card ar = 
    AristotleRelationCardinality 
        (max (ar_relationrole_set_max card) (length (ar_relationrole_set ar))) 

-- ------------------------
-- Output 
-- extract all primary JSON objects
arj2ar :: [Either String AristotleRelationJSON] -> [AristotleRelation] 
arj2ar flist = concatMap results (rights flist) 

-- convert to out  
ar2Out :: [AristotleRelation] -> [AristotleRelationOut] 
ar2Out jlist = map ar2arOut jlist 

-- convert JSON objects into ar_relationrole_set dependent list 
ar2rrOut :: [AristotleRelation] -> [AristotleRelationRoleOut] 
ar2rrOut ps = concatMap (\p -> arr2arrOut (ar_id p) (ar_uuid p) (ar_relationrole_set p)) ps 


-- All relation output collected into a single type   
data AristotleOutR = 
  AristotleOutR { 
            aor_AristotleAnyItemType :: AristotleAnyItemType 
          , aor_AristotleRelation :: [AristotleRelation]   
          , aor_AristotleProcessResults :: AristotleProcessResults                     
          , aor_AristotleRelationOut :: [AristotleRelationOut] 
          , aor_AristotleRelationRelationRoleOut :: [AristotleRelationRoleOut] 
          } deriving (Show,Generic)  

defaultAristotleOutR = AristotleOutR Relation [] defaultAristotleProcessResults  [] [] 

populateAristotleOutR :: AristotleAnyItemType -> [AristotleRelation] -> AristotleProcessResults -> AristotleOutR 
populateAristotleOutR aait ps apr 
  = AristotleOutR aait ps apr (ar2Out ps) (ar2rrOut ps) 

-- full proc  
getAristotleR :: AristotleAnyItemType -> [FetchResult] -> String -> String -> AristotleOutR 
getAristotleR aait fetched mts mte 
  = populateAristotleOutR aait collected apr 
    where 
      parsed = map (rb2Aristotle.fetchedBody) fetched 
      collected = arj2ar parsed 
      ajc = count (safeEither defaultAristotleRelationJSON parsed) 
      apr = writeAPR aait mts mte ajc defaultAristotleRelationJSON parsed collected  

-- it writes out a file if the count > 0  
writeAristotleR :: AristotleOutR -> String -> IO ()  
writeAristotleR aor fp 
  = do 
  if length (aor_AristotleRelationOut aor) > 0 then 
    B.writeFile (aait2filename aait fp "") (encodeDefaultOrderedByName (aor_AristotleRelationOut aor))  
  else return ()  

  if length (aor_AristotleRelationRelationRoleOut aor) > 0 then 
    B.writeFile (aait2filename aait fp "RelationRole") (encodeDefaultOrderedByName (aor_AristotleRelationRelationRoleOut aor))  
  else return ()  

      where aait = (aor_AristotleAnyItemType aor) 

-- wrapper for the R web request 
web2aor :: W.Options -> Int -> IO AristotleOutR 
web2aor opts1 limitpage  = 
  do 
   let aait = Relation 
-- fetching      
   mts <- messageTime "Start fetching: " 
   fetched <- crawl opts1 limitpage (Just (aait2http aait 100)) []  
   mte <- messageTime "End fetching:  " 
-- return  
   return (getAristotleR aait fetched mts mte)  

-- wrapper for the R file read and write  
file2aor fR aait = 
  do 
    B.writeFile (aait2filename aait "fileAristotle" "") (encodeDefaultOrderedByName ((ar2Out.arj2ar) [(rb2Aristotle fR)] ))  
      
