								
# Parsing Aristotle JSON files using Haskell 								
								
								
## Purpose 								
								
These modules use Haskell to read the Aristole API, parse the JSON and export the information into CSVs. 								
								
## Background 								
								
Aristotle Metadata Registry is commercial Metadata Registry software based on the ISO/IEC 11179 standard. 								
								
The API is defined at the following site: https://aristotle.cloud/api/v4/								
						
The JSON parser was built in Haskell 9 using the wreq, lens, words8, aeson and cassava packages. This software has only been used by myself. It is only run intermittently when a new export was needed. Therefore, the IO is very simple. There is also no Cabal install. 
The total JSON Parsing hs code base is 4,500 LOC ignoring the PartOfSpeech module.  								
								
Any errors or misapplication of Haskell are my own. 								
								
## Design - Custom Modules 								
								
The following includes hs modules, as well as input json files, sample output csv files, text files and word files.  								
								
File	| Ord	| Lines	| FT	| For
-----------------------	| --	| -----	| ----	| -----------------------
Aristotle Data Model.docx	| 1	| 230	| docx	| This explains the data model structure behind the Aristotle objects. Read this before using this code. 
Aristotle.Main.AnyConcept.hs	| 2	| 70	| hs	| This is the main module for Aristotle Any Concept objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.AnyItem.hs	| 3	| 140	| hs	| This is the main module for Aristotle Any Item objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.AnyItem.OCP.hs	| 4	| 160	| hs	| This is the main module for Aristotle Any Item (Object Class Property) objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.ConceptDelta.hs	| 5	| 70	| hs	| This is the main module for Aristotle Concept Delta objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.DataSetSpecification.hs	| 6	| 70	| hs	| This is the main module for Aristotle Data Set Specification objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.Distribution.hs	| 7	| 70	| hs	| This is the main module for Aristotle Distribution objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.Issue.hs	| 8	| 70	| hs	| This is the main module for Aristotle Issue objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.Link.hs	| 9	| 70	| hs	| This is the main module for Aristotle Link objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.ObjectClassSpecialisation.hs	| 10	| 70	| hs	| This is the main module for Aristotle Object Class Specialisation objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.Relation.hs	| 11	| 70	| hs	| This is the main module for Aristotle Relation objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.ValueDomain.hs	| 12	| 70	| hs	| This is the main module for Aristotle Value DOmain objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
AristotleAnyConcept.hs	| 13	| 220	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Any Concept objects. 
AristotleAnyItem.hs	| 14	| 750	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Any Item objects. 
AristotleCommon.hs	| 15	| 380	| hs	| This module defines common Wreq functions, and common or reused Aristotle JSON objects. Crawl is a fold function that gathers together all Aristotle object pages until next page is null.  Checkparse exposes JSON parser errors. 
AristotleConceptDelta.hs	| 16	| 260	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Concept Delta objects. 
AristotleDataSetSpecification.hs	| 17	| 220	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Data Set Specification objects. 
AristotleDistribution.hs	| 18	| 250	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Distribution objects. 
AristotleIssue.hs	| 19	| 330	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Issue objects. 
AristotleLink.hs	| 20	| 220	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Link objects. 
AristotleObjectClassSpecialisation.hs	| 21	| 250	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Object Class Specialisation objects. 
AristotleRelation.hs	| 22	| 290	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Relation objects. 
AristotleValueDomain.hs	| 23	| 230	| hs	| This module contains specific JSON and CSV parsing functions for Aristotle Value Domain objects. 
CassavaUtils.hs	| 24	| 170	| hs	| This module defines common CSV, String, Word8 and Bytestring functions. 
PartOfSpeech.hs	| 25	| 700	| hs	| This module defines reusable Parts of Speech functions. It is a crude, initial implementation.
anyconcept.page001.Fixed.json	| 26	| 20	| json	| This is an sample JSON file for Aristotle Any Concept objects, which has been fixed to help parsing. 
anyitem.page001.Fixed.json	| 27	| 60	| json	| This is an sample JSON file for Aristotle Any Item objects, which has been fixed to help parsing. 
conceptdelta.page001.Fixed.json	| 28	| 130	| json	| This is an sample JSON file for Aristotle Concept Delta objects, which has been fixed to help parsing. 
dataElement.page001.Fixed.json	| 29	| 1200	| json	| This is an sample JSON file for Aristotle Data Element objects, which has been fixed to help parsing. 
dataelementconcept.page001.Fixed.json	| 30	| 920	| json	| This is an sample JSON file for Aristotle Data Element Concept objects, which has been fixed to help parsing. 
datasetspecification.page001.Fixed.json	| 31	| 1140	| json	| This is an sample JSON file for Aristotle Data Set Specification objects, which has been fixed to help parsing. 
datatype.page001.Fixed.json	| 32	| 630	| json	| This is an sample JSON file for Aristotle Data Type objects, which has been fixed to help parsing. 
distribution.page001.Fixed.json	| 33	| 460	| json	| This is an sample JSON file for Aristotle Distribution objects, which has been fixed to help parsing. 
issue.page001.Fixed.json	| 34	| 4700	| json	| This is an sample JSON file for Aristotle Issue objects, which has been fixed to help parsing. 
link.page001.Fixed.json	| 35	| 410	| json	| This is an sample JSON file for Aristotle Link objects, which has been fixed to help parsing. 
objectclass.page001.Fixed.json	| 36	| 250	| json	| This is an sample JSON file for Aristotle Object Class objects, which has been fixed to help parsing. 
objectclassSpecialisation.page001.Fixed.json	| 37	| 3300	| json	| This is an sample JSON file for Aristotle Object Class Specialisation objects, which has been fixed to help parsing. 
property.page001.Fixed.json	| 38	| 790	| json	| This is an sample JSON file for Aristotle Property objects, which has been fixed to help parsing. 
relation.page001.Fixed.json	| 39	| 180	| json	| This is an sample JSON file for Aristotle Relation objects, which has been fixed to help parsing. 
valuedomain.page001.Fixed.json	| 40	| 3530	| json	| This is an sample JSON file for Aristotle Value Domain objects, which has been fixed to help parsing. 
Z Install.docx	| 41	| 100	| docx	| This is a list of Install steps for Haskell and pacman.
Z Uninstall HP Wolf Security.docx	| 42	| 1	| docx	| This is why HP wolf was uninstalled
Z Aristotle_API.ps1	| 43	| 100	| ps1	| This was a half hearted attempt to use powershell to read the API. TLDR. 
0 Bugs Haskell.txt	| 44	| 200	| txt	| Describes various bugs or misunderstandings overcome during parsing the JSON.
token.txt	| 45	| 1	| txt	| Contains Aristotle supplied API token. This is not checked in for obvious security reasons. 
Z Install.txt	| 46	| 1900	| txt	| This is the results of the install steps above. 
