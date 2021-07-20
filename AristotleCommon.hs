{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

{- |
   Module      : AristotleCommon 
   Description : Common Wreq functions and Aristotle JSON object types  
   Copyright   : ( c ) Matthew Lawler 2021 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

   crawl is a fold function that gathers together all Aristotle object pages until next page is null. 
   checkparse exposes parse errors. 

   Sites: 
   https://artyom.me/aeson 
   https://dss.aristotlecloud.io/api/v4/ 
   http://www.serpentine.com/wreq/tutorial.html

   Capturing http errors  
   https://stackoverflow.com/questions/34285433/wreq-stop-404s-throwing-exceptions 

   print opts1 
-- -> Options { manager = Left _, proxy = Nothing, auth = Just (OAuth2Token "xyzabc"), 
--  headers = [("User-Agent","haskell wreq-0.5.3.3")], params = [], redirects = 10, cookies = Just (CJ {expose = []}) }

 -}
module AristotleCommon 
    (  

      AristotleMetadatarReferenceLink( .. ), 
      AristotleGroup( .. ), 
      AristotleOrgRecord( .. ), 

      AristotleAllNextJSON( .. ),  
      AristotleCustomValue( .. ),   
      AristotleIdentifier( .. ),  
      AristotleSlot( .. ),  
      AristotleDistributionDataElementPath( .. ), 
      AristotleDSSDEInclusion( .. ), 
      AristotleDSSClusterInclusion( .. ), 

      FetchResult( .. ), 

      opts2, 
      crawl, 
      checkParse, 

      prop_inverse_B_to_JSON_to_B, 
      prop_inverse_JSON_to_B_to_JSON, 

     ) where
    
import Data.Maybe 
import Data.Either 
import GHC.Generics 
import qualified Data.ByteString.Lazy as B 

import Control.Lens ( set, (^.), (?~), (&) ) 
import Data.Aeson (eitherDecode, encode, withObject, object, (.:), (.=), (.:?),  
                   ToJSON(..), FromJSON(..) ) 
import Network.Wreq (checkResponse, defaults, auth, oauth2Token, getWith, 
                     responseStatus, statusCode, responseBody, 
                     Options ) 

import CassavaUtils 

-- For wreq - Aristotle web access 

opts2 = (set checkResponse (Just $ \_ _ -> return ()) defaults) :: Options 

data AristotleAllNextJSON =
  AristotleAllNextJSON { 
           next :: Maybe String    
           } deriving (Show,Generic) 

instance Eq AristotleAllNextJSON where 
  (AristotleAllNextJSON n1 ) == (AristotleAllNextJSON n2 )   
      = n1 == n2  

instance ToJSON AristotleAllNextJSON 
instance FromJSON AristotleAllNextJSON 

defaultAristotleObjectClassJSON = AristotleAllNextJSON Nothing 

data FetchResult
  = Fetched { fetchedBody :: B.ByteString, nextUrl :: Maybe String } 
  | FetchError String 
  deriving (Show)

fetch :: Options -> String -> IO FetchResult 
fetch opts url = do 
    gotten <- getWith opts url  
    if gotten ^. responseStatus . statusCode == 200 
    then return $ Fetched (gotten ^. responseBody) (getNext (rb2Next (gotten ^. responseBody)))  
    else return $ FetchError $ "http statusCode: " ++ show (gotten ^. responseStatus . statusCode) ++ " url: " ++ url 

rb2Next :: B.ByteString -> Either String AristotleAllNextJSON 
rb2Next rb = (eitherDecode.ctlChar2SpacesBS) rb 

getNext :: Either String AristotleAllNextJSON -> Maybe String
getNext esa = 
     case esa of 
     Left err -> Nothing 
     Right json -> (next json)   

crawl :: Options -> Int -> Maybe String -> [FetchResult] -> IO [FetchResult] 
crawl opts mp murl fs 
  | isNothing murl || mp <= 0 = return fs 
  | otherwise  = do 
     fetchResult <- fetch opts (fromMaybe "" murl) 
     case fetchResult of 
       FetchError mes -> return fs  
       Fetched body murlNext -> do 
         print $ "read: " ++ (fromMaybe "" murl)  
         crawl opts (mp - 1) murlNext (fs ++ [fetchResult]) 

checkParse :: (FromJSON a) => Either String a -> String 
checkParse esa = 
     case esa of 
     Left err -> err  
     Right _ -> "Parse Successful" 

-- for common shared JSONs 
-- dummies added to discover parse errors and discover correct structure 
-- These were not used at all in the SA data, so were untested. 

data AristotleMetadatarReferenceLink = 
  AristotleMetadatarReferenceLink   { 
            mrl_id :: Int
          , mrl_name :: Int 
          , mrl_dummy :: Int                        
           } deriving (Show,Generic) 

instance ToJSON AristotleMetadatarReferenceLink where 
    toJSON (AristotleMetadatarReferenceLink mrl_id mrl_name mrl_dummy ) = 
        object ["id" .= mrl_id ,"name" .= mrl_name 
               ,"dummy" .= mrl_dummy 
               ]

instance FromJSON AristotleMetadatarReferenceLink where 
    parseJSON = withObject "AristotleMetadatarReferenceLink" $ \v -> AristotleMetadatarReferenceLink 
        <$> v .: "id" 
        <*> v .: "name" 
        <*> v .: "dummy"   

data AristotleGroup = 
  AristotleGroup   { 
            g_id :: Int
          , g_name :: Int 
          , g_dummy :: Int                        
           } deriving (Show,Generic) 

instance ToJSON AristotleGroup where 
    toJSON (AristotleGroup g_id g_name g_dummy ) = 
        object ["id" .= g_id ,"name" .= g_name 
               ,"dummy" .= g_dummy 
               ]

instance FromJSON AristotleGroup where 
    parseJSON = withObject "AristotleGroup" $ \v -> AristotleGroup 
        <$> v .: "id" 
        <*> v .: "name" 
        <*> v .: "dummy"    

-- end dummies 

-- These were all used in some JSON types.  

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


data AristotleIdentifier = 
  AristotleIdentifier   { 
            ai_id :: String
          , ai_namespace :: Int 
          , ai_identifier ::  String          
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


data AristotleSlot = 
  AristotleSlot   { 
            as_id :: Int
          , as_name :: String 
          , as_value ::  String   
          , as_order :: Int 
          , as_permission :: Int           
           } deriving (Show,Generic) 

instance ToJSON AristotleSlot where 
    toJSON (AristotleSlot as_id as_name as_value as_order as_permission) = 
        object ["id" .= as_id ,"name" .= as_name  
               ,"value" .= as_value 
               ,"order" .= as_order  ,"permission" .= as_permission        
               ]

instance FromJSON AristotleSlot where 
    parseJSON = withObject "AristotleSlot" $ \v -> AristotleSlot 
        <$> v .: "id" 
        <*> v .: "name" 
        <*> v .: "value" 
        <*> v .: "order" 
        <*> v .: "permission" 


data AristotleDistributionDataElementPath = 
  AristotleDistributionDataElementPath   { 
            addep_id :: String
          , addep_logical_path :: String 
          , addep_order :: Int 
          , addep_specific_information :: String
          , addep_data_element :: Maybe Int             
--          , addep_specialisation_classes :: []                      
           } deriving (Show,Generic) 

instance ToJSON AristotleDistributionDataElementPath where 
    toJSON (AristotleDistributionDataElementPath 
            addep_id addep_logical_path addep_order addep_specific_information addep_data_element) = 
        object ["id" .= addep_id ,"logical_path" .= addep_logical_path  
               ,"order" .= addep_order  ,"specific_information" .= addep_specific_information
               ,"data_element" .= addep_data_element                 
--               ,"specialisation_classes" .= addep_specialisation_classes        
               ]

instance FromJSON AristotleDistributionDataElementPath where 
    parseJSON = withObject "AristotleDistributionDataElementPath" $ \v -> AristotleDistributionDataElementPath 
        <$> v .: "id" 
        <*> v .: "logical_path" 
        <*> v .: "order" 
        <*> v .: "specific_information" 
        <*> v .:? "data_element"         
--        <*> v .: "specialisation_classes"     


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

--    property function composed with inverse function  
--prop_inverse_B_to_JSON_to_B :: (ToJSON a, FromJSON a) => a -> B.ByteString -> B.ByteString -> Bool 
--prop_inverse_B_to_JSON_to_B :: B.ByteString -> B.ByteString -> Bool 
--prop_inverse_B_to_JSON_to_B a defaultB b = encode (fromRight defaultB (eitherDecode b)) == b  

prop_inverse_B_to_JSON_to_B :: (ToJSON B.ByteString, FromJSON B.ByteString) => B.ByteString -> B.ByteString -> Bool 
prop_inverse_B_to_JSON_to_B defaultB b = encode (fromRight defaultB (eitherDecode b)) == b  

prop_inverse_JSON_to_B_to_JSON :: (FromJSON a, ToJSON a, Eq a) => a -> a -> Bool 
prop_inverse_JSON_to_B_to_JSON defaultA a = fromRight defaultA (eitherDecode (encode a)) == a 

