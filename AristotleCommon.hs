{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable, FlexibleContexts #-}

{- |
   Module      : AristotleCommon 
   Description : Common Wreq functions and 
   Copyright   : ( c ) Matthew Lawler 2021 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com 

   crawl is a fold function that gathers together all Aristotle object pages until next page is null. 
   checkparse exposes parse errors. 

   Sites: 
   https://artyom.me/aeson 
   https://aristotle.cloud/api/v4/
   http://www.serpentine.com/wreq/tutorial.html

   Capturing http errors  
   https://stackoverflow.com/questions/34285433/wreq-stop-404s-throwing-exceptions 

   print opts1 
-- -> Options { manager = Left _, proxy = Nothing, auth = Just (OAuth2Token "xyzabc"), 
--  headers = [("User-Agent","haskell wreq-0.5.3.3")], params = [], redirects = 10, cookies = Just (CJ {expose = []}) }

   need to make permanent 
:set -package uuid-types

   need to 
   1. map over all uuids + handle no permissions error 
   2. extract type from hrFinalRequest

 -}
module AristotleCommon 
    (  

      AristotleAllNextJSON( .. ), 
      FetchResult( .. ), 
      AristotleAnyItemType( .. ), 
      AristotleProcessResults( .. ), 
      ParseResult( .. ), 

      opts2, 
      crawl, 
      checkParse, 
      aait2http, 
      rb2Aristotle, 
      safeEither, 
      writeAPR, 
      prop_count_to_parsed, 
      prop_parse_ok, 
      aait2filename, 
      defaultAristotleProcessResults, 
      parseList, 

      fetchUUID, 

     ) where
    
import Data.Maybe 
import Data.Either 
import Data.List 
import GHC.Generics 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.ByteString.Internal as BI  
import qualified Control.Monad as CM  

import Control.Lens ( (^.), set  ) 
-- (^.) :: s -> Getting a s a -> a 
-- set :: ASetter s t a b -> b -> s -> t

-- makeLenses
-- (&) :: a -> (a -> b) -> b 
-- (?~) :: ASetter s t a (Maybe b) -> b -> s -> t
-- view

import Data.Aeson (eitherDecode, encode, withObject, object, (.:), (.=), (.:?),  
                   ToJSON(..), FromJSON(..) ) 
import Network.Wreq (checkResponse, defaults, auth, oauth2Token, getWith, 
                     responseStatus, statusCode, responseBody, 
                     customMethodWith, hrFinalRequest, hrRedirects, 
                     Options, HistoriedResponse  ) 
-- customHistoriedMethodWith :: String -> Options -> String -> IO (HistoriedResponse ByteString) 
-- (m a) -> Lens (m a) r 
-- makeLenses :: Name -> DecsQ 
-- hrFinalRequest :: Lens' (HistoriedResponse body) Request 
--  , responseHeader, 
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered, encodeDefaultOrderedByName, parseField, toField) 
import Data.Data 
import Data.UUID.Types 
import qualified Network.HTTP.Client as N 

import CassavaUtils 

-- For wreq - Aristotle web access 

opts2 = (set checkResponse (Just $ \_ _ -> return ()) defaults) :: Options 

data AristotleAllNextJSON =
  AristotleAllNextJSON { 
           next :: Maybe String    
           } deriving (Show,Generic) 

instance Eq AristotleAllNextJSON where 
  (AristotleAllNextJSON n1 ) == (AristotleAllNextJSON n2 ) = n1 == n2  

instance ToJSON AristotleAllNextJSON 
instance FromJSON AristotleAllNextJSON 

defaultAristotleObjectClassJSON = AristotleAllNextJSON Nothing 

data FetchResult
  = Fetched { fetchedBody :: BL.ByteString, 
              nextUrl :: Maybe String } 
  | FetchError String 
  deriving (Show)

fetch :: Options -> String -> IO FetchResult 
fetch opts url = do 
    gotten <- getWith opts url  
    if gotten ^. responseStatus . statusCode == 200 
    then return $ Fetched (gotten ^. responseBody) (getNext (rb2Next (gotten ^. responseBody)))  
    else return $ FetchError $ "http statusCode: " ++ show (gotten ^. responseStatus . statusCode) ++ " url: " ++ url 

rb2Next :: BL.ByteString -> Either String AristotleAllNextJSON 
rb2Next rb = (eitherDecode.ctlChar2SpacesBS) rb 

getNext :: Either String AristotleAllNextJSON -> Maybe String
getNext esa = 
     case esa of 
     Left err -> Nothing 
     Right json -> (next json)   

-- type for a single fetch only  
data FetchSingle
  = FetchSingleBody { 
           fetchSingleBody :: BL.ByteString 
--         , fetchFinalRequest :: BI.ByteString          
--         , fetchFinalRequest :: N.Request 
         } 
  | FetchSingleError String 
  deriving (Show)

-- get final request after all redirects from host, as this shows json type 
-- customMethodWith "PATCH" opts "http://httpbin.org/patch"  
-- use hrFinalRequest somehow!?!? 
fetchSingle :: Options -> String -> IO FetchSingle 
fetchSingle opts url = do 
--    gotten <- getWith opts url  
    gotten <- customMethodWith "GET" opts url 
    if gotten ^. responseStatus . statusCode == 200 
    then return $ FetchSingleBody (gotten ^. responseBody) 
--    (N.path ((makeLenses gotten) ^. hrFinalRequest))        
--    then return $ FetchSingleBody (gotten ^. responseBody) (gotten ^. hrRedirects) 
    else return $ FetchSingleError $ "http statusCode: " ++ show (gotten ^. responseStatus . statusCode) ++ " url: " ++ url 

-- fetch a single page using UUID 
fetchUUID :: Options -> String -> IO FetchSingle  
fetchUUID opts uuid 
  = do 
     fetchResult <- fetchSingle opts (uuid2http uuid) 
     print $ "read: " ++ (uuid2http uuid) 
     case fetchResult of 
       FetchSingleBody body -> print $ " body: " ++ show body  
       FetchSingleError mes -> print $ "error message: " ++ mes          
     return fetchResult 
  
-- fetch all pages from a single seed page web request 
crawl :: Options -> Int -> Maybe String -> [FetchResult] -> IO [FetchResult] 
crawl opts mp murl fs 
  | mp > 0 && isJust murl   
    = do 
     fetchResult <- fetch opts (fromMaybe "" murl) 
     case fetchResult of 
       FetchError mes -> return fs  
       Fetched body murlNext -> do 
         print $ "read: " ++ (fromMaybe "" murl)  
         crawl opts (mp - 1) murlNext (fs ++ [fetchResult]) 
  | otherwise = return fs 

-- "http://aristotle.cloudo/api/v4/metadata/ffff7f36-dc6d-11ea-a913-0a6933dafb30"
-- -> 
-- "https://aristotle.cloud/api/v4/metadata/dataelement/ffff7f36-dc6d-11ea-a913-0a6933dafb30" 
-- the response http adds the AnyItem type. How to capture this?  
uuid2http :: String -> String 
uuid2http uuid = "http://dss.aristotlecloud.io/api/v4/metadata/" ++ uuid 

-- "http://aristotle.cloudo/api/v4/concept/14710"
-- -> 
-- "http://aristotle.cloud/api/v4/concept/14710/"
-- ignore added forward slash 
concept2http :: Int -> String
concept2http concept = "http://dss.aristotlecloud.io/api/v4/concept/" ++ show concept 

-- checks for parse errors 
checkParse :: (FromJSON a) => Either String a -> String 
checkParse esa = 
     case esa of 
     Left err -> err  
     Right _ -> "Parse Successful" 

-- capture error 
data ParseResult  
  = ParseResult { 
          parsedIds :: [Int] 
        , resultMsg :: String 
        , unparsed :: BL.ByteString 
          } deriving (Show,Generic) 

-- parse ByteString into Either JSON 
--rb2Aristotle :: BL.ByteString -> Either String AristotleAnyItemJSON  
rb2Aristotle :: (FromJSON a, ToJSON a) => BL.ByteString -> Either String a    
rb2Aristotle = (eitherDecode.ctlChar2SpacesBS) 

-- prev with error  
parseOne :: (FromJSON a, ToJSON a) => (a -> [Int]) -> FetchResult -> ParseResult 
parseOne a2Ids fr 
  = case (rb2Aristotle bs) of 
     Left err -> ParseResult [] err bs   
     Right okA -> ParseResult (a2Ids okA) "Parse Successful" BL.empty 
     where bs = (fetchedBody fr) 

parseFail :: ParseResult -> Bool 
parseFail pr = (resultMsg pr) /= "Parse Successful" 

-- get detailed error results 
-- filter :: (a -> Bool) -> [a] -> [a]
parseList :: (FromJSON a, ToJSON a) => (a -> [Int]) -> [FetchResult] -> [ParseResult] 
parseList a2Ids frs = filter parseFail (map (parseOne a2Ids) frs) 

-- safe either lists  
safeEither :: a -> [Either String a] -> a  
safeEither a ps 
   | length ps == 0 = a 
   | otherwise = fromRight a (head ps) 

-- --------------
-- Primary Sum or Enum types 
-- These are types that use the AnyItem boilerplate to capture all or most of their JSON fields. 
data AristotleAnyItemType 
    = AnyItem | DataElement | DataElementConcept | DataType | ObjectClass | Property | DataSetSpecification 
    | Distribution | ObjectClassSpecialisation | Relation | ValueDomain 
-- added back in to make common functions work     
    | AnyConcept | ConceptDelta | Issue | Link
       deriving (  Eq, Ord, Typeable, Show, Read, Data ) 

-- converting http string back to type 
string2AnyItem :: String -> AristotleAnyItemType 
string2AnyItem s 
  | s == "anyitem" = AnyItem
  | s == "dataelement" = DataElement
  | s == "dataelementconcept" = DataElementConcept
  | s == "datatype" = DataType
  | s == "objectclass" = ObjectClass
  | s == "property" = Property
  | s == "datasetspecification" = DataSetSpecification
  | s == "distribution" = Distribution
  | s == "objectclassspecialisation" = ObjectClassSpecialisation
  | s == "relation" = Relation
  | s == "valuedomain" = ValueDomain
  | s == "anyconcept" = AnyConcept
  | s == "conceptdelta" = ConceptDelta
  | s == "issue" = Issue
  | s == "link" = Link 
  | otherwise = AnyItem 

instance FromField AristotleAnyItemType where
 parseField bs
  | s == "anyitem" = pure AnyItem
  | s == "dataelement" = pure DataElement
  | s == "dataelementconcept" = pure DataElementConcept
  | s == "datatype" = pure DataType
  | s == "objectclass" = pure ObjectClass
  | s == "property" = pure Property
  | s == "datasetspecification" = pure DataSetSpecification
  | s == "distribution" = pure Distribution
  | s == "objectclassspecialisation" = pure ObjectClassSpecialisation
  | s == "relation" = pure Relation
  | s == "valuedomain" = pure ValueDomain
  | s == "anyconcept" = pure AnyConcept
  | s == "conceptdelta" = pure ConceptDelta
  | s == "issue" = pure Issue
  | s == "link" = pure Link         
  | otherwise = CM.mzero 
    where s = bs2s bs 

-- converts string to bytestring 
instance ToField AristotleAnyItemType where      
   toField a = (s2bs.show) a   

-- Boilerplate only = AnyItem | DataElement | DataElementConcept | DataType | ObjectClass | Property   

-- Part BP =  DataSetSpecification | Distribution | ObjectClassSpecialisation | Relation | ValueDomain 
-- These have additional JSON fields.  

-- These are oither types that do not follow the AnyItem boilerplate. 
-- data AristotleAnyOtherType = AnyConcept | ConceptDelta | Issue | Link deriving (  Eq, Ord, Typeable, Show, Read, Data )  

-- build http get request string 
-- size up to 100 available for metadata gets 
-- Other gets are restricted to a fixed 20 JSON objects per get  
aait2http :: AristotleAnyItemType -> Int -> String 
aait2http aait pagesize 
-- AnyConcept needs to be rewritten 
  | aait == AnyConcept = hdr ++ "concept/14710/" 
  | aait == ConceptDelta = hdr ++ "metadata_info/latest_action/?page=1" 
  | aait == Issue = hdr ++ "issues/?page=1" 
  | aait == Link = hdr ++ "links/?page=1" 
  | otherwise = hdr ++ "metadata/" ++ (lc aait) ++ "?page=1&page_size=" ++ show pagesize 
    where hdr = "http://dss.aristotlecloud.io/api/v4/" 

-- compares input count to parsed types 
--prop_count_to_parsed :: Int -> [AristotleAnyItem] -> Bool 
prop_count_to_parsed :: Int -> [a] -> Bool 
prop_count_to_parsed j ps = j == length ps 

-- compares input count to parsed types   
prop_parse_ok :: (FromJSON a) => [Either String a] -> Bool 
prop_parse_ok parsed = nub (map checkParse parsed)  == ["Parse Successful"] 

-- Unused so far 
--    property function composed with inverse function  
--prop_inverse_B_to_JSON_to_B :: (ToJSON a, FromJSON a) => a -> BL.ByteString -> BL.ByteString -> Bool 
--prop_inverse_B_to_JSON_to_B :: BL.ByteString -> BL.ByteString -> Bool 
--prop_inverse_B_to_JSON_to_B a defaultB b = encode (fromRight defaultB (eitherDecode b)) == b  

prop_inverse_B_to_JSON_to_B :: (ToJSON BL.ByteString, FromJSON BL.ByteString) => BL.ByteString -> BL.ByteString -> Bool 
prop_inverse_B_to_JSON_to_B defaultB b = encode (fromRight defaultB (eitherDecode b)) == b  

prop_inverse_JSON_to_B_to_JSON :: (FromJSON a, ToJSON a, Eq a) => a -> a -> Bool 
prop_inverse_JSON_to_B_to_JSON defaultA a = fromRight defaultA (eitherDecode (encode a)) == a 

-- Process status  
data AristotleProcessResults = 
  AristotleProcessResults { 
            apr_AristotleAnyItemType :: AristotleAnyItemType 
          , apr_aait2http :: String 
          , apr_fetch_time_start :: String 
          , apr_fetch_time_end :: String           
          , apr_checkParse :: String    
          , apr_prop_parse_ok :: Bool 
          , apr_JSON_count :: Int 
          , apr_parse_count :: Int 
          , apr_prop_count_to_parsed :: Bool 
--          , apr_parse_results :: [ParseResult a] 
          } deriving (Show,Generic)  

defaultAristotleProcessResults = AristotleProcessResults AnyItem "" "" "" "" False 0 0 False 

-- --      where ajc = (count (aocsj2aocsHead defJSON pl)) [FetchResult] -> frs
writeAPR :: (FromJSON a, ToJSON a, FromJSON b) => AristotleAnyItemType -> String -> String -> Int 
  -> a -> [Either String a] -> [b] -> AristotleProcessResults
writeAPR aait mts mte ajc defJSON pl ps  = 
  AristotleProcessResults 
    aait (aait2http aait 100) mts mte 
    (show (nub (map checkParse pl)))  
    (prop_parse_ok pl)  
    ajc 
    (length ps) 
    (prop_count_to_parsed ajc ps) 
--    (parseList defJSON frs) 

-- build filename eg  "fullAristotleDataTypeCustomValue.csv" 
-- example fp is "fullAristotle"
-- example for child is "CustomValue" 
aait2filename :: AristotleAnyItemType -> String -> String -> String 
aait2filename aait fp child = fp ++ show aait ++ child ++ ".csv" 
