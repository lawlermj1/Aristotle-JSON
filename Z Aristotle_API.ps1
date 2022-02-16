
# Invoke-RestMethod ok for finding the wbe apge, but it parses the JSON into a PS Object. 
# see:
https://adamtheautomator.com/powershell-json/

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="4bea3227b30d93865d796e0c1660435d23be05f7"} -Uri "http://aristotle.cloud/api/v4/metadata/dataelement"

#->
#count next                                                            previous results                                               
#----- ----                                                            -------- -------                                               
#  112 http://aristotle.cloud/api/v4/metadata/dataelement?page=2          {@{id=191; created=2018-08-10T14:07:55.681528+10:00...


Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="4bea3227b30d93865d796e0c1660435d23be05f7"} -Uri "http://aristotle.cloud/api/v4/metadata/dataelementconcept"

#-> http://aristotle.cloud/api/v4/metadata/dataelementconcept?page=2


Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/property"

#-> http://aristotle.cloud/api/v4/metadata/property?page=2 

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/valuedomain"

#-> http://aristotle.cloud/api/v4/metadata/valuedomain?page=2

#----------------------------------
# Now we get problems 

cd C:\Users\lawle\Documents\Aristotle 

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" 

# can only get 10???

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/distribution"

# can only get 4???

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/datasetspecification"

# can only get 9???

# some dead ends 

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" | Out-File objectclass.page001.txt

# writes out top level JSON tags - need to get results tag 

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" | Out-String | Out-File objectclass.page001.txt 

# no change 

Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" | Out-String | ConvertFrom-Json 

# still gets ConvertFrom-Json : Invalid JSON primitive: count. 

$url = Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass"

[System.Net.WebClient]::new().DownloadString($url) | ConvertFrom-Json

# NOPE! 

Invoke-WebRequest -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" 

# -> Invoke-WebRequest : The response content cannot be parsed because the Internet Explorer engine is not available, or Internet Explorer's first-launch configuration is not complete.

$OC = Invoke-RestMethod -Method Get -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" 

$OC.results | Out-File objectclass.page001.txt

# Now a F## PS Object!!! No longer JSON 

#----------------------------------
# How to get the results tag from the output??? 

# The Invoke-WebRequest command performs a similar function by sending HTTP verbs to Web services but does not have the parsing ability that Invoke-RestMethod does.

Invoke-WebRequest -Method 'GET'  -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" 

# -> Invoke-WebRequest : The response content cannot be parsed because the Internet Explorer engine is not available, or Internet Explorer's first-launch configuration is not complete. Specify the UseBasicParsing parameter and try again. 

Invoke-WebRequest -Method 'GET'  -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" -UseBasicParsing

# -> works

$OC = Invoke-WebRequest -Method 'GET'  -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/objectclass" -UseBasicParsing

$OC.Content | Out-File objectclass.page001.txt

$DN = Invoke-WebRequest -Method 'GET'  -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/distribution" -UseBasicParsing

$DN.Content | Out-File distribution.page001.json

$DSS = Invoke-WebRequest -Method 'GET'  -ContentType application/json -Headers @{"auth_token"="f01758794ed749168bb38d7231ac23cb845a0d8f"} -Uri "http://aristotle.cloud/api/v4/metadata/datasetspecification" -UseBasicParsing

$DSS.Content | Out-File datasetspecification.page001.json

