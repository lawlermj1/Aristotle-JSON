								
# Parsing Aristotle JSON files using Haskell 								
								
								
## Purpose 								
								
These modules use Haskell to read the Aristole API, parse the JSON and export the information into CSVs. 								
								
## Background 								
								
Aristotle Metadata Registry is commercial Metadata Registry software based on the ISO/IEC 11179 standard. 								
								
The API is defined at the following site: 								
 dss.aristotlecloud.io/api/v4/ 								
								
The JSON parser was built in Haskell 9 using the wreq, lens, words8, aeson and cassava packages. This software has only been used by myself. It is only run intermittently when a new export was needed. Therefore, the IO is very simple. There is also no Cabal install.  								
								
Any errors or misapplication of Haskell are my own. 								
								
## Design - Custom Modules 								
								
The following includes hs modules, as well as input json files, sample output csv files, text files and word files.  								
								
File	| 	Ord	| 	Lines	| 	FT	| 	For
-----------------------	| 	--	| 	-----	| 	----	| 	-----------------------
Aristotle.Main.DataElement.hs	| 	1	| 	70	| 	hs	| 	This is the main module for Aristotle Data Element objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.DataElementConcept.hs	| 	2	| 	70	| 	hs	| 	This is the main module for Aristotle Data Element Concept objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.DataSetSpecification.hs	| 	3	| 	70	| 	hs	| 	This is the main module for Aristotle Data Set Specification objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.Distribution.hs	| 	4	| 	70	| 	hs	| 	This is the main module for Aristotle Distribution objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.ObjectClass.hs	| 	5	| 	70	| 	hs	| 	This is the main module for Aristotle Object Class objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.Property.hs	| 	6	| 	70	| 	hs	| 	This is the main module for Aristotle Property objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
Aristotle.Main.ValueDomain.hs	| 	7	| 	70	| 	hs	| 	This is the main module for Aristotle Value DOmain objects. It reads the API, parses the JSON into haskell types, and exports into csv a file.
AristotleCommon.hs	| 	8	| 	380	| 	hs	| 	This module defines common Wreq functions, and common or reused Aristotle JSON objects. Crawl is a fold function that gathers together all Aristotle object pages until next page is null.  Checkparse exposes JSON parser errors. 
AristotleDataElement.hs	| 	9	| 	200	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Data Element objects. 
AristotleDataElementConcept.hs	| 	10	| 	200	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Data Element Concept objects. 
AristotleDataSetSpecification.hs	| 	11	| 	220	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Data Set Specification objects. 
AristotleDistribution.hs	| 	12	| 	250	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Distribution objects. 
AristotleObjectClass.hs	| 	13	| 	190	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Object Class objects. 
AristotleProperty.hs	| 	14	| 	190	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Property objects. 
AristotleValueDomain.hs	| 	15	| 	230	| 	hs	| 	This module contains specific JSON and CSV parsing functions for Aristotle Value Domain objects. 
CassavaUtils.hs	| 	16	| 	170	| 	hs	| 	This module defines common CSV, String, Word8 and Bytestring functions. 
dataElement.page001.Fixed.json	| 	17	| 	1200	| 	json	| 	This is an sample JSON file for Aristotle Data Element objects, which has been fixed to help parsing. 
dataElement.page001.json	| 	18	| 	1400	| 	json	| 	This is an sample JSON file for Aristotle Data Element objects, without change. 
dataElement.page001.txt	| 	19	| 	1420	| 	txt	| 	This is an sample text file taken from the Aristotle Data Element web page.  
dataelementconcept.page001.Fixed.json	| 	20	| 	920	| 	json	| 	This is an sample JSON file for Aristotle Data Element Concept objects, which has been fixed to help parsing. 
dataelementconcept.page001.json	| 	21	| 	960	| 	json	| 	This is an sample JSON file for Aristotle Data Element Concept objects, without change. 
dataelementconcept.page001.txt	| 	22	| 	990	| 	txt	| 	This is an sample text file taken from the Aristotle Data Element Concept web page.  
datasetspecification.page001.Fixed.json	| 	23	| 	1140	| 	json	| 	This is an sample JSON file for Aristotle Data Set Specification objects, which has been fixed to help parsing. 
datasetspecification.page001.json	| 	24	| 	1	| 	json	| 	This is an sample JSON file for Aristotle Data Set Specification objects, without change. 
distribution.page001.Fixed.json	| 	25	| 	460	| 	json	| 	This is an sample JSON file for Aristotle Distribution objects, which has been fixed to help parsing. 
distribution.page001.json	| 	26	| 	1	| 	json	| 	This is an sample JSON file for Aristotle Distribution objects, without change. 
fileAristotleDataSetSpecification.csv	| 	27	| 	1	| 	csv	| 	Sample csv output from a file
fullAristotleDataSetSpecification.csv	| 	28	| 	260	| 	csv	| 	Sample csv output from the API
objectclass.page001.Fixed.json	| 	29	| 	250	| 	json	| 	This is an sample JSON file for Aristotle Object Class objects, which has been fixed to help parsing. 
objectclass.page001.json	| 	30	| 	1	| 	json	| 	This is an sample JSON file for Aristotle Object Class objects, without change. 
property.page001.Fixed.json	| 	31	| 	790	| 	json	| 	This is an sample JSON file for Aristotle Property objects, which has been fixed to help parsing. 
property.page001.json	| 	32	| 	850	| 	json	| 	This is an sample JSON file for Aristotle Property objects, without change. 
property.page001.txt	| 	33	| 	880	| 	txt	| 	This is an sample text file taken from the Aristotle Property web page.  
valuedomain.page001.Fixed.json	| 	34	| 	3530	| 	json	| 	This is an sample JSON file for Aristotle Value Domain objects, which has been fixed to help parsing. 
valuedomain.page001.json	| 	35	| 	3580	| 	json	| 	This is an sample JSON file for Aristotle Value Domain objects, without change. 
valuedomain.page001.txt	| 	36	| 	3600	| 	txt	| 	This is an sample text file taken from the Aristotle Value Domain web page.  
Z Aristotle_API.ps1	| 	37	| 	100	| 	ps1	| 	This was a half hearted attempt to use powershell to read the API. TLDR. 
Z bugs.txt	| 	38	| 	160	| 	txt	| 	This contains the approach, ghci commands, and bugs encountered, with fixes. 
Z Install.docx	| 	39	| 	100	| 	docx	| 	This is a list of Install steps for Haskell and pacman.
Z Install.txt	| 	40	| 	1900	| 	txt	| 	This is the results of the install steps above. 
Z Uninstall HP Wolf Security.docx	| 	41	| 	1	| 	docx	| 	This is why HP wolf was uninstalled
token.txt	| 	42	| 	1	| 	txt	| 	Contains Aristotle supplied API token. This is not checked in for obvious security reasons. 
