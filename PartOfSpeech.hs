{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}

{- |
   Module      : PartOfSpeech
   Description : This module defines parts of speech. 
   Copyright   : ( c ) Matthew Lawler 2021   
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This is based on the gf: Grammatical Framework module. 
   See
   https://hackage.haskell.org/package/gf

   Acronym added as a separate Part of Speech, which is non standard in GF, 
   but incredibly useful, when parsing, due to the semantic density. 
   Also added to help with parsing are Year and Letter. 
   All numbers are covered by Numeral. 

   Not a complete parsing solution. 

 -}
 
module PartOfSpeech 
    (  
      W2POS( .. ), 
      Words2POS( .. ), 
      Name2Namespace( .. ), 
      Space2Words( .. ), 
      Space2Nouns( .. ), 
      Noun2Space( .. ), 
      SpaceNoun2SpaceNoun( .. ), 

      cw2pSortAll,  
      sortWords2POS, 
      word2Map, 
      toMapW2POS, 
      singleWords2POS, 
      n2nsOutAll, 

      n2ws4s2wOutAll, 
      n2ns4n2ws, 
      defaultName2Namespace, 

      n2ws4s2ws,
      s2wfsOut, 
      s2w4s2nOut, 
      s2n4n2sOut, 

      sn2sn1, 
      sn2snIntra, 
      sn2snInter, 

     ) where

import Data.Csv

import qualified Data.List as L  
import qualified Data.Ord as O  
import qualified Data.Vector as V 
import qualified Data.Map.Strict as M  
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as DD 
import qualified Data.Maybe as DM 
import qualified Debug.Trace as DT 
import qualified Control.Monad as CM  
import qualified GHC.Generics as GG 

import CassavaUtils 

data POSType 
  = Acronym | ProperName | Adjective1Place | Adjective2Place | AdverbAdj | Pronoun | Quantifier 
  | Determiner | Conjunction | Subjunction | Preposition | Noun | NounRelational | AdverbNum 
  | Numeral | AdverbVerb | Verb1Place | Verb2Place | Verb3Place | VerbAdjective | VerbNPAP 
  | VerbNPV | VerbSentence | VerbVP | Year | Letter | Expansion | Misspell | Unknown 
    deriving ( Eq, Ord, DD.Typeable, Show, Read, Enum ) 

-- Ord is needed when when resolving duplicates, such as date, which can be a verb, noun or adjective. 
-- The derived Ord instance will give the correct order, based on constructor order. 
-- The constructor order is: 
-- Acronym 
-- Proper Name; note that this can be a name like Paris, or the expansion of Acronym 
-- Adjectives 
-- Noun modifiers like Preposition, Conjunction, Determiner etc 
-- Nouns 
-- Adverbs 
-- Verbs 
-- The goal of parsing here it to separate properties (adjective) from objects (nouns). 
-- As adjectives often are based on nouns, and adjectives are less common, the parser will assume that word is an adjective. 
-- Later, full use of the GF will eliminate this bug. 
-- Acronyms are always phrases, which have the highest parse priority, as these are precise namespace terms. 
-- Verbs are last as these do not overlap with nouns, and most analysis focuses on nouns, rather than verbs. 

-- Acronym : Definition = an abbreviation initial letters ; Example = nasa for NASA
-- ProperName : Definition = proper name ; Example = Paris
-- Adjective1Place : Definition = one-place adjective ; Example = warm
-- Adjective2Place : Definition = two-place adjective ; Example = divisible
-- AdverbAdj : Definition = adjective-modifying adverb ; Example = very
-- Pronoun : Definition = personal pronoun ; Example = she
-- Quantifier : Definition = quantifier ('nucleus' of Det) ; Example = this/these
-- Determiner : Definition = determiner phrase ; Example = those seven
-- Conjunction : Definition = conjunction ; Example = and
-- Subjunction : Definition = subjunction ; Example = if
-- Preposition : Definition = preposition, or just case ; Example = in
-- Noun : Definition = common noun ; Example = house
-- NounRelational : Definition = relational noun ; Example = son
-- AdverbNum : Definition = numeral-modifying adverb ; Example = more than
-- Numeral : Definition = cardinal or ordinal in words ; Example = five/fifth
-- AdverbVerb : Definition = adverb directly attached to verb ; Example = always
-- Verb1Place : Definition = one-place verb ; Example = sleep
-- Verb2Place : Definition = two-place verb ; Example = love
-- Verb3Place : Definition = three-place verb ; Example = show
-- VerbAdjective : Definition = adjective-complement verb ; Example = look
-- VerbNPAP : Definition = verb with NP and AP complement ; Example = paint
-- VerbNPV : Definition = verb with NP and V complement ; Example = cause
-- VerbSentence : Definition = sentence-complement verb ; Example = claim
-- VerbVP : Definition = verb-phrase-complement verb ; Example = want
-- Year : Definition = unit of time ; Example = 1995
-- Letter : Definition = part of alphabet ; Example = z
-- Expansion : Definition = words used in acronym ; Example = FT: Full Time
-- Misspell : Definition = incorrect letters ; Example = chiild
-- Unknown : Definition = unrecognised ; Example = 


-- Note the function to convert bytestring to string 
instance FromField POSType where
    parseField bs
        | s == "Acronym" = pure Acronym
        | s == "ProperName" = pure ProperName
        | s == "Adjective1Place" = pure Adjective1Place
        | s == "Adjective2Place" = pure Adjective2Place
        | s == "AdverbAdj" = pure AdverbAdj
        | s == "Pronoun" = pure Pronoun
        | s == "Quantifier" = pure Quantifier
        | s == "Determiner" = pure Determiner
        | s == "Conjunction" = pure Conjunction
        | s == "Subjunction" = pure Subjunction
        | s == "Preposition" = pure Preposition
        | s == "Noun" = pure Noun
        | s == "NounRelational" = pure NounRelational
        | s == "AdverbNum" = pure AdverbNum
        | s == "Numeral" = pure Numeral
        | s == "AdverbVerb" = pure AdverbVerb
        | s == "Verb1Place" = pure Verb1Place
        | s == "Verb2Place" = pure Verb2Place
        | s == "Verb3Place" = pure Verb3Place
        | s == "VerbAdjective" = pure VerbAdjective
        | s == "VerbNPAP" = pure VerbNPAP
        | s == "VerbNPV" = pure VerbNPV
        | s == "VerbSentence" = pure VerbSentence
        | s == "VerbVP" = pure VerbVP
        | s == "Year" = pure Year
        | s == "Letter" = pure Letter
        | s == "Misspell" = pure Misspell 
        | s == "Expansion" = pure Expansion 
        | otherwise = pure Unknown          
--        | otherwise = CM.mzero 
        where s = bs2s bs 

-- converts string to bytestring 
instance ToField POSType where      
   toField a = (s2bs.show) a 

-- --------------   
data SystemName 
    = Aristotle | Sparx 
      deriving ( Eq, Ord, DD.Typeable, Show, Read, Enum )    

instance FromField SystemName where
    parseField bs
        | s == "Aristotle" = pure Aristotle
        | s == "Sparx" = pure Sparx
        | otherwise = CM.mzero 
        where s = bs2s bs 

-- converts string to bytestring 
instance ToField SystemName where      
   toField a = (s2bs.show) a 

-- -----------------------
-- POS input type 
-- POSType placed first as the words only have an arbitrary limit - that is, it could be expanded. 
-- normal GF phrases extend up to 4 words.
-- but Acronym expansion can be as many as 10 words. 
data W2POS = W2POS
    { pOSType :: !POSType 
    , word1   :: !String
    , word2   :: String
    , word3   :: String
    , word4   :: String
    , word5   :: String
    , word6   :: String
    , word7   :: String
    , word8   :: String
    , word9   :: String
    , wordA   :: String
    } deriving (Show, GG.Generic) 

instance FromNamedRecord W2POS
instance ToNamedRecord W2POS
instance DefaultOrdered W2POS                

-- POS internal type 
data Words2POS = Words2POS
    { partOfSpeech :: POSType 
    , numberWords :: Int 
    , word1CharLen :: Int 
    , rankW2P :: Int 
    , wordsinPhrase :: [String] 
    } deriving (Show, GG.Generic) 

instance FromNamedRecord Words2POS 
instance ToNamedRecord Words2POS
instance DefaultOrdered Words2POS   

defaultWords2POS = Words2POS Unknown 0 0 0 [] 

-- never going to do this, and this will trigger an error if used 
instance FromField Words2POS where
    parseField s = CM.mzero 

-- just for testing 
instance ToField Words2POS where 
    toField w2p 
      = s2bs (show (partOfSpeech w2p) ++ " : " ++ 
              show (numberWords w2p) ++ ", " ++ 
              show (word1CharLen w2p) ++ ", " ++ 
              show (rankW2P w2p) ++ " : " ++ 
              L.intercalate ", " (wordsinPhrase w2p) ) 

-- never going to do this, and this will trigger an error if used 
instance FromField [Words2POS]  where 
    parseField s = CM.mzero 

-- awful, but it works when needed for dumping output for unit testing 
instance ToField [Words2POS]  where 
    toField ss = s2bs (L.intercalate ", " (L.map show ss))  

instance Eq Words2POS where
  (Words2POS _ _ r1 _ _ ) == (Words2POS _ _ r2 _ _ ) = r1 == r2  

instance Ord Words2POS where
  (Words2POS _ _ r1 _ _ ) `compare` (Words2POS _ _ r2 _ _ ) = r1 `compare` r2 

-- convert input into POS working internal type 
cw2p :: W2POS -> Words2POS
cw2p w = Words2POS (partOfSpeech wo) (numberWords wo) (word1CharLen wo) (rankW2P2I wo) ws 
          where 
-- collect words into a list, and set all to lower case, and remove invalid chars            
            ws = map noquotes (map (lc.(filter (not.isBracket))) (filter (/= "") [word1 w, word2 w, word3 w, word4 w, word5 w, word6 w, word7 w, word8 w, word9 w, wordA w]))
-- build an initial w2p so that ranking can be done 
            wo = Words2POS (pOSType w) (length ws) (length (word1 w)) 0 []  

-- rank is NOT an id 
-- determine ranking order for parse choice 
-- max number of total words = 60K  so = 100,000 * number of words in a phrase = 10 
-- max characters in a word = 30 so = 1000 * number of chars 
-- ordering from POSType, with Acronym = 0 and VerbVP = 23 (reverse order - 100)
-- no need for uniqueness Fold == test after sorting in order
rankW2P2I :: Words2POS -> Int 
rankW2P2I w = -1 * ((numberWords w)  * 100000 + (word1CharLen w) * 1000 + (100 - (fromEnum (partOfSpeech w))))  

-- sort to resolve duplicate POS types for best parsing 
-- sort by numberWords, then charLen, then POSType order  
-- comparing :: Ord a => (b -> a) -> b -> b -> Ordering
sortWords2POS :: [W2POS] -> [Words2POS] 
sortWords2POS ws  
  = L.sortBy ( O.comparing rankW2P ) (map cw2p ws) 

-- Assumed inout is sorted, then simplify Words2POS to a single words and rmdups 
-- rmdupkvs :: (Ord k, Ord v) => [(k,v)] -> [(k,v)] 
singleWords2POS :: [Words2POS] -> [(String,Words2POS)]  
singleWords2POS ws = rmdupkvs wwkvs  
        where 
-- creates a KV tuple            
          wwkvs = map (\w -> ((noquotes (head (wordsinPhrase w)),w))) ww 
-- only single words other filter duplicates?         
          ww = filter (\w -> numberWords w == 1) ws  

-- M.fromAscList :: Eq k => [(k, a)] -> Map k a 
-- sort in alphabetic order as M.Map seems to do a B-Tree for lookups 
-- constructs simple Map on POS with first word as key 
word2Map :: [Words2POS] -> M.Map String Words2POS  
word2Map ws  
    = M.fromAscList wls  
        where 
          wls = L.sortBy ( O.comparing fst ) (map (\w -> (noquotes (head (wordsinPhrase w)), w )) ws)   

-- toList :: Map k a -> [(k, a)]
toMapW2POS :: M.Map String Words2POS -> [MapW2POS]  
toMapW2POS kas = map (\(k,a) -> MapW2POS k a) (M.toList kas) 

data MapW2POS = MapW2POS
    { mapKey :: String
    , mapValue :: Words2POS
    } deriving (Show, GG.Generic) 

instance FromNamedRecord MapW2POS
instance ToNamedRecord MapW2POS
instance DefaultOrdered MapW2POS                   

-- POS external type 
data Words2POSOut = Words2POSOut
    { partOfSpeechOut :: POSType 
    , numberWordsOut :: Int 
    , word1CharLenOut :: Int 
    , rankW2POut :: Int 
    , wordsinPhraseOut :: String 
    } deriving (Show, GG.Generic) 

instance FromNamedRecord Words2POSOut
instance ToNamedRecord Words2POSOut
instance DefaultOrdered Words2POSOut     

-- convert to output 
-- add commas to the concat 
cw2pOut :: Words2POS -> Words2POSOut
cw2pOut w = Words2POSOut (partOfSpeech w) (numberWords w) (word1CharLen w) (rankW2P2I w) (L.intercalate ", " (wordsinPhrase w)) 

-- convert primary CSV into out type 
cw2pOutAll :: [W2POS] -> [Words2POSOut] 
cw2pOutAll ws = map (cw2pOut.cw2p) ws 

-- convert primary CSV into out type 
cw2pSortAll :: [W2POS] -> [Words2POSOut] 
cw2pSortAll ws = map cw2pOut (sortWords2POS ws)

-- -----------------------
-- definedName input type 
data Name2Namespace = Name2Namespace
    { definedName :: String 
    , definedSpace :: String 
    , definedIn :: SystemName   
--    { definedName :: !String 
--    , definedSpace :: !String 
--    , definedIn :: !SystemName      
    } deriving (Show, GG.Generic) 

instance FromNamedRecord Name2Namespace
instance ToNamedRecord Name2Namespace
instance DefaultOrdered Name2Namespace   

defaultName2Namespace = Name2Namespace "" "" Aristotle 

-- definedName internal type 
data Name2Words = Name2Words
    { n2wName :: String 
-- ! unused? contains parse from name     
    , n2wNamePOS :: [Words2POS] 
    , n2wWords :: [String]  
-- contains parse from words          
    , n2wWordsPOS :: [Words2POS] 
    , n2wSpace :: String 
    , n2wSystemName :: SystemName 
    } deriving (Show, GG.Generic) 

instance FromNamedRecord Name2Words
instance ToNamedRecord Name2Words
instance DefaultOrdered Name2Words 

-- matching a definedName using single words only - not phrases 
-- lookup :: Ord k => k -> Map k a -> Maybe a 
-- catMaybes :: [Maybe a] -> [a]
-- filter :: (a -> Bool) -> [a] -> [a]
nm2ws :: M.Map String Words2POS -> Name2Namespace -> Name2Words 
nm2ws wlu n2ns 
--    | DT.trace ( "nm2ws: ocws = " ++ concatMap (++ ", ") ocws ) False = undefined 
--    | otherwise 
    = Name2Words ocn [] ocws (DM.catMaybes ocwsPos) (definedSpace n2ns) (definedIn n2ns) 
      where 
        ocn = definedName n2ns 
        ocws = map noquotes (words (lc (filter (not.isBracket) ocn)))   
-- matches only if there is a single word in ocws!?!?! 
        ocwsPos = map (\w -> M.lookup w wlu) ocws 

-- definedName external type 
data Name2WordsOut = Name2WordsOut
    { n2wNameOut :: String 
-- contains parse from name     
    , n2wNamePOSOut :: String 
    , n2wWordsOut :: String 
-- contains parse from words          
    , n2wWordsPOSOut :: String 
    , n2wSpaceOut :: String 
    } deriving (Show, GG.Generic) 

instance FromNamedRecord Name2WordsOut
instance ToNamedRecord Name2WordsOut
instance DefaultOrdered Name2WordsOut   

-- stringify the type 
nm2wsOut :: Name2Words -> Name2WordsOut 
nm2wsOut o = Name2WordsOut (n2wName o) "" (L.intercalate ", " (n2wWords o)) (concatMap show (n2wWordsPOS o)) (n2wSpace o) 

nm2wsOutAll :: [Name2Words] -> [Name2WordsOut] 
nm2wsOutAll os = map nm2wsOut os 

n2ns4n2ws :: M.Map String Words2POS -> [Name2Namespace] -> [Name2Words] 
n2ns4n2ws wlu n2nsL = map (\o -> nm2ws wlu o) n2nsL  

n2nsOutAll :: M.Map String Words2POS -> [Name2Namespace] -> [Name2WordsOut] 
n2nsOutAll wlu n2nsL = nm2wsOutAll (map (\o -> nm2ws wlu o) n2nsL) 

-- --------------------------
-- Internal collect words into a Space  
data Space2Words = Space2Words
    {  s2wSystemName :: SystemName   
    ,  s2wSpace :: String 
-- words by POS type  
    , s2wNouns :: [String] 
    , s2wAdjectives :: [String]     
    , s2wAcronyms :: [String] 
    , s2wProperNames :: [String]     
    , s2wOther :: [String] 
    , s2wWords :: [String]     
-- words in POS type    
    , s2wWords2POS :: [Words2POS]     
    } deriving (Eq, Ord, Show, GG.Generic) 

defaultSpace2Words = Space2Words Sparx "" [] [] [] [] [] [] [] 

instance FromNamedRecord Space2Words
instance ToNamedRecord Space2Words
instance DefaultOrdered Space2Words   

-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]   
n2ws4s2w :: Name2Words -> Space2Words 
n2ws4s2w n2w 
  = Space2Words 
      (n2wSystemName n2w) 
      (noquotes (n2wSpace n2w))  
      (concatMap wordsinPhrase (filter (\w -> ((partOfSpeech w) == Noun || (partOfSpeech w) == NounRelational || (partOfSpeech w) == Acronym)) w2p))
      (concatMap wordsinPhrase (filter (\w -> ((partOfSpeech w) == Adjective1Place || (partOfSpeech w) == Adjective2Place || (partOfSpeech w) == AdverbAdj )) w2p)) 
      (concatMap wordsinPhrase (filter (\w -> ((partOfSpeech w) == Acronym )) w2p))
      (concatMap wordsinPhrase (filter (\w -> ((partOfSpeech w) == ProperName)) w2p))      
      (concatMap wordsinPhrase (filter (\w -> ((partOfSpeech w) /= Noun && (partOfSpeech w) /= NounRelational && (partOfSpeech w) /= Acronym && (partOfSpeech w) /= Adjective1Place 
         && (partOfSpeech w) /= Adjective2Place && (partOfSpeech w) /= AdverbAdj && (partOfSpeech w) /= Acronym && (partOfSpeech w) /= ProperName )) w2p))  
      (map noquotes (n2wWords n2w))               
      w2p            
      where w2p = (n2wWordsPOS n2w) 

-- tuple up 
s2w4kv :: Space2Words -> (String,Space2Words)
s2w4kv s2w = (s2wSpace s2w,s2w) 

-- tuple and sort 
n2ws4s2wAll :: [Name2Words] -> [(String,[Space2Words])] 
n2ws4s2wAll n2ws 
  = sortFoldlTupleAny "" defaultSpace2Words ts 
    where ts = map (s2w4kv.n2ws4s2w) n2ws 

s2wSingle :: (String,[Space2Words]) -> Space2Words 
s2wSingle (k,v) 
  = Space2Words 
      (head (map s2wSystemName v)) 
      k 
      (L.nub (concatMap s2wNouns v)) 
      (L.nub (concatMap s2wAdjectives v)) 
      (L.nub (concatMap s2wAcronyms v)) 
      (L.nub (concatMap s2wProperNames v))             
      (L.nub (concatMap s2wOther v))   
      (L.nub (concatMap s2wWords v))        
      (L.nub (concatMap s2wWords2POS v))             

data Space2WordsOut = Space2WordsOut
    { s2wSystemNameOut :: String 
    , s2wSpaceOut :: String 
    , s2wNounsOut :: String
    , s2wAdjectivesOut :: String  
    , s2wAcronymsOut :: String  
    , s2wProperNamesOut :: String     
    , s2wOtherOut :: String
    , s2wWordsOut :: String     
    , s2wWords2POSOut :: String     
    } deriving (Show, GG.Generic) 

instance FromNamedRecord Space2WordsOut
instance ToNamedRecord Space2WordsOut
instance DefaultOrdered Space2WordsOut     

s2wOut :: Space2Words -> Space2WordsOut 
s2wOut s 
  = Space2WordsOut
    (show (s2wSystemName s)) 
    (s2wSpace s)     
    (L.intercalate ", " (s2wNouns s)) 
    (L.intercalate ", " (s2wAdjectives s)) 
    (L.intercalate ", " (s2wAcronyms s))   
    (L.intercalate ", " (s2wProperNames s))         
    (L.intercalate ", " (s2wOther s)) 
    (L.intercalate ", " (s2wWords s))      
    (L.intercalate ", " (map show (s2wWords2POS s)))        

s2wOutAll :: [Space2Words] -> [Space2WordsOut] 
s2wOutAll ss = map s2wOut ss  

--n2ws4s2wAll :: [Name2Words] -> [(String,[Space2Words])] 
--s2wSingle :: (String,[Space2Words]) -> Space2Words 

-- combining it all 
n2ws4s2ws :: [Name2Words] -> [Space2Words] 
n2ws4s2ws n2ws 
  = map s2wSingle (n2ws4s2wAll n2ws) 

-- combining it all 
n2ws4s2wOutAll :: [Name2Words] -> [Space2WordsOut] 
n2ws4s2wOutAll n2ws 
  = map s2wOut (map s2wSingle (n2ws4s2wAll n2ws)) 

-- Frequency 
-- freq :: (Eq a, Ord a) => [a] -> [(a,Int)] 
data Space2WordsFreq = Space2WordsFreq 
    { s2wfSystemName :: SystemName   
--    , s2wfSpace :: String 
    , s2wfNouns :: [(String,Int)] 
    , s2wfAdjectives :: [(String,Int)]     
    , s2wfAcronyms :: [(String,Int)] 
    , s2wfProperNames :: [(String,Int)]  
    , s2wfOther :: [(String,Int)] 
    , s2wfWords :: [(String,Int)] 
    } deriving (Eq, Ord, Show, GG.Generic) 

instance FromNamedRecord Space2WordsFreq
instance ToNamedRecord Space2WordsFreq
instance DefaultOrdered Space2WordsFreq      

defaultSpace2WordsFreq = Space2WordsFreq Aristotle [] [] [] [] [] [] 

-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b] 
s2w4s2wf :: SystemName -> [Space2Words] -> Space2WordsFreq 
s2w4s2wf sn s2ws 
  = Space2WordsFreq 
      sn 
--      s2wSpace (head s2wsSn) 
      (freq (concatMap s2wNouns s2wsSn)) 
      (freq (concatMap s2wAdjectives s2wsSn)) 
      (freq (concatMap s2wAcronyms s2wsSn)) 
      (freq (concatMap s2wProperNames s2wsSn)) 
      (freq (concatMap s2wOther s2wsSn))      
      (freq (concatMap s2wWords s2wsSn)) 
        where s2wsSn = filter (\s -> (s2wSystemName s) == sn) s2ws 

-- output 
data Space2WordsFreqOut = Space2WordsFreqOut 
    { s2wfSystemNameOut :: SystemName   
--    , s2wfSpaceOut :: String  
    , s2wfWordsOut :: String   
    , s2wfWordsFreqOut :: Int   
    , s2wfPOSType :: POSType 
    } deriving (Eq, Ord, Show, GG.Generic)

instance FromNamedRecord Space2WordsFreqOut
instance ToNamedRecord Space2WordsFreqOut
instance DefaultOrdered Space2WordsFreqOut   

defaultSpace2WordsFreqOut = Space2WordsFreqOut Aristotle "" 0 Unknown    

-- just gets the full words 
-- lookup :: Ord k => k -> Map k a -> Maybe a 
-- use fromMaybe Unknown for default 
s2wfOut :: M.Map String Words2POS -> Space2WordsFreq -> [Space2WordsFreqOut]  
s2wfOut wlu s2wf 
  = map (\(s,i) -> Space2WordsFreqOut (s2wfSystemName s2wf) s i  
                   (partOfSpeech (DM.fromMaybe defaultWords2POS (M.lookup s wlu)))) (s2wfWords s2wf) 

-- two instances for each system 
s2wfs :: [Space2Words] -> [Space2WordsFreq]
s2wfs s2w 
  = [s2w4s2wf Sparx s2w, s2w4s2wf Aristotle s2w]  

-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b] 
s2wfsOut :: M.Map String Words2POS -> [Space2Words] -> [Space2WordsFreqOut]
s2wfsOut wlu s2w 
  = concatMap (s2wfOut wlu) (s2wfs s2w) 

-- M.fromAscList :: Eq k => [(k, a)] -> Map k a 
-- sort in alphabetic order as M.Map seems to do a B-Tree for lookups 
-- constructs simple Map on POS with first word as key 
-- k = (system, word)
wordFreq2Map :: [Space2WordsFreqOut] -> M.Map (SystemName, String) Space2WordsFreqOut   
wordFreq2Map ws  
  = M.fromAscList wls  
    where wls = L.sortBy ( O.comparing fst ) (map (\w -> ((s2wfSystemNameOut w, noquotes (s2wfWordsOut w)), w )) ws)   

-- ------------------------
-- Relate a Space to Nouns 
data Space2Nouns = Space2Nouns
    { s2nSystemName :: SystemName   
    , s2nSpace :: String 
    , s2nCommonNouns :: [String]     
    , s2nUniqueNouns :: [String]
    , s2nNouns :: [String]      
    } deriving (Eq, Ord, Show, GG.Generic) 

instance FromNamedRecord Space2Nouns
instance ToNamedRecord Space2Nouns
instance DefaultOrdered Space2Nouns      

defaultSpace2Nouns = Space2Nouns Sparx "" [] []  

--s2w4s2n :: M.Map (SystemName, String) Space2WordsFreqOut -> Space2Words -> Space2Nouns 
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- invariant length nouns = length unique nouns + length common nouns  
s2w4s2n :: [Space2WordsFreqOut] -> Space2Words -> Space2Nouns 
s2w4s2n s2wfs s2w 
  = Space2Nouns 
     (s2wSystemName s2w) (s2wSpace s2w) 
-- multiple namespaces 
     (map s2wfWordsOut (filter 
        (\s -> (eqSystem (s2wfSystemNameOut s) && isNoun (s2wfPOSType s)  
            && (s2wfWordsFreqOut s) > 1) && elem (s2wfWordsOut s) s2wn) s2wfs ))      
-- filter by system, and noun POStype and only one namespace + using space nouns 
     (map s2wfWordsOut (filter 
        (\s -> (eqSystem (s2wfSystemNameOut s) && isNoun (s2wfPOSType s) 
            && (s2wfWordsFreqOut s) == 1) && elem (s2wfWordsOut s) s2wn) s2wfs ))   
     s2wn      
       where 
         eqSystem s = (s2wSystemName s2w) == s  
         isNoun w = elem w [Noun, NounRelational, Acronym] 
         s2wn = (s2wNouns s2w)   

-- 
s2w4s2nOut :: [Space2WordsFreqOut] -> [Space2Words] -> [Space2Nouns]   
s2w4s2nOut s2wfs s2ws = map (s2w4s2n s2wfs) s2ws 

-- ------------------------
-- Relate a Noun to Spaces  
data Noun2Space = Noun2Space
    { n2sSystemName :: SystemName   
    , n2sSpace :: String 
    , n2sNoun :: String     
    , n2sIsItCommon :: Bool 
    } deriving (Eq, Ord, Show, GG.Generic) 

instance FromNamedRecord Noun2Space
instance ToNamedRecord Noun2Space
instance DefaultOrdered Noun2Space      

defaultNoun2Space = Noun2Space Sparx "" "" False 

s2n4n2s :: Space2Nouns -> [Noun2Space] 
s2n4n2s s2n 
  = map (\n -> Noun2Space sn sp n True) (s2nCommonNouns s2n) 
    ++ map (\n -> Noun2Space sn sp n False) (s2nUniqueNouns s2n) 
    where 
      sn = (s2nSystemName s2n) 
      sp = (s2nSpace s2n) 

s2n4n2sOut :: [Space2Nouns] -> [Noun2Space] 
s2n4n2sOut s2ns = concatMap s2n4n2s s2ns 

-- ------------------------
-- Relate a Noun across Spaces and System 
data SpaceNoun2SpaceNoun = SpaceNoun2SpaceNoun 
    { sn2snSystemName1 :: SystemName   
    , sn2snSpace1 :: String 
    , sn2snNoun1 :: String 
    , sn2snSystemName2 :: SystemName  
    , sn2snSpace2 :: String     
    , sn2snNoun2 :: String          
    } deriving (Eq, Ord, Show, GG.Generic) 

instance FromNamedRecord SpaceNoun2SpaceNoun
instance ToNamedRecord SpaceNoun2SpaceNoun
instance DefaultOrdered SpaceNoun2SpaceNoun 

defaultSpaceNoun2SpaceNoun = SpaceNoun2SpaceNoun Sparx "" "" Sparx "" "" 

-- do an intial cart product on common nouns only 
-- cartProd :: [a] -> [b] -> [(a, b)]   (cartProd cn2ss cn2ss )  
--take2 :: [a] -> [(a,a)] -> [(a,a)]  
-- halfCartProd :: [a] -> [(a, a)] -> [(a, a)]
sn2sn1 :: [Noun2Space] -> [SpaceNoun2SpaceNoun] 
sn2sn1 n2ss 
  = map (\(a,b) -> 
      SpaceNoun2SpaceNoun 
        (n2sSystemName a) (n2sSpace a) (n2sNoun a) 
        (n2sSystemName b) (n2sSpace b) (n2sNoun b)         
         ) (halfCartProd (filter n2sIsItCommon n2ss) [])   

-- find all connections within a system via filtering 
-- system and noun same, but the space must be different 
-- sorts, then use evenOddSplit to take one of each pair of connections 
sn2snIntra :: [SpaceNoun2SpaceNoun] -> [SpaceNoun2SpaceNoun] 
sn2snIntra sn2sns 
  = filter (\s -> (sn2snSystemName1 s) == (sn2snSystemName2 s) && (sn2snSpace1 s) /= (sn2snSpace2 s) && (sn2snNoun1 s) == (sn2snNoun2 s)) sn2sns

--  = fst (evenOddSplit intSort) 
--    where 
--      int = filter (\s -> (sn2snSystemName1 s) == (sn2snSystemName2 s) && (sn2snSpace1 s) /= (sn2snSpace2 s) && (sn2snNoun1 s) == (sn2snNoun2 s)) sn2sns
--      intSort = L.sortBy ( O.comparing (\s -> (sn2snNoun1 s) ++ (sn2snSpace1 s)) ) int    

-- find all connections across system via filtering 
sn2snInter :: [SpaceNoun2SpaceNoun] -> [SpaceNoun2SpaceNoun] 
sn2snInter sn2sns 
  = filter (\s -> (sn2snSystemName1 s) == Aristotle && (sn2snSystemName2 s) == Sparx && (sn2snNoun1 s) == (sn2snNoun2 s)) sn2sns 

-- ------------------------
-- Test 
-- average length of parsing 
-- That is Average = (length n2wWordsPOS) / n2wWords should be between 110 and 160. 
-- If less, then there is a parsing error, and additional words are needed. 
