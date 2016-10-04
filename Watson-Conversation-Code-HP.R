######################################################
### IBM Watson - Code Snippet ---  R CONVERSATION CODE Backbone for Harry Potter #STEM Lab - DAY 3
### Experimental Code. R Interface for IBM Watson Services
### DOCS: http://www.ibm.com/watson/developercloud/doc/conversation/
### Before you begin you will need (1) An IBM Bluemix demo account (2) CONVERSATION Service tile created on bluemix with API credentials and 
###  (3) Create a basic conversation with Tooling provided in bluemix - see https://dreamtolearn.com/ryan/r_journey_to_watson/39 for examples
#####################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(splitstackshape) # for the text to colummns 
library(reshape2)
require(jsonlite) # for FROM JSON
#library(rjson)

######### Housekeeping And Authentication
setwd("/Users/ryan/Documents/Project_Harry_Potter") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. 

url_CONV="https://gateway.watsonplatform.net/conversation/api/v1"
version="?version=2016-07-11"
username_CONV # check we got it from KEYs.R file in same directory - looks like this - "e63f524d-9999-9999-9999-e6b1d4e99b87"
password_CONV # check we got it from KEYs.R file in same directory - looks like this - "ABCD0EggCXYZ"
workspace_CONV # check we got it from KEYs.R file - looks like this - "2ded4293-9999-9999-9999-4c8b1289be81" <- **** YOU NEED TO PULL THIS FROM BROWSER URL WHEN YOU TEST CONVO IN TOOLING - I THINK ONLY PLACE TO GET RIGHT NO

###########  FUNCTION DECLARATIONS ################

# WORKS AT TERMINAL # curl -X POST -u 7221eeb0-5e6c-4187-xxxx-fc66747ee08d:nkOxxxxxMuIB -H "Content-Type:application/json" -d  "{\"input\": {\"text\": \"\"}}" "https://gateway.watsonplatform.net/conversation/api/v1/workspaces/81203da7-xxxx-xxxx-bcc1-0d3519d0b944/message?version=2016-07-11"

## FUNCTION DECLARE - INITIATE CONVERSATION WITH SERVICE
conversation.init <- function()  
    {
    raw_response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                 authenticate(username_CONV,password_CONV),
                 add_headers("Content-Type"="application/json"),
                 body = '{ "input": { "text":""},
                           "system":{ "dialog_stack":["root"]},
                           "dialog_turn_counter":1,
                           "dialog_request_counter":1
                         }',
                 encode = "json"
                 )
                return(content(raw_response, "text", encoding = "UTF-8"))
}



## FUNCTION DECLARE - GO DEEPER INTO CONVERSATION
conversation.chat <- function(dialog,conversation_id,dialog_stack_node)  #### LEVEL 1 ENGAGE
{
  the_body <-  paste('{"input":{"text":"',
                     dialog,
                     '"},"context":{"conversation_id":"',
                     conversation_id,
                     '","system":{"dialog_stack":["',
                     dialog_stack_node,
                     '"],',
                     '"dialog_turn_counter":1,"dialog_request_counter":1}}}',
                     sep="")
  
  raw_response <- POST(url=paste(url_CONV,"/workspaces/",workspace_CONV,"/message",version,sep=""),
                       authenticate(username_CONV,password_CONV),
                       add_headers("Content-Type"="application/json"),
                       body = the_body
  )
  return(content(raw_response, "text", encoding = "UTF-8"))
}

########### END OF FUNCTION DECLARE


 
########### MAIN CODE
########### MAIN CODE
########### MAIN CODE


####### WARNING -####### WARNING - THIS CODE IS VERY BRITTLE - WILL GET YOU STARTED - BUT BREAKS UNDER MANY CONDITIONS !

# ** START HERE ** INITIALIZATION ANDGET HOOKS 
response <- conversation.init()
temp <- fromJSON(response)
text <- temp$output$text
text  <- gsub("\\n", " - ", text)
print(text)
conv_id <-temp$context$conversation_id
dial_stack <- temp$context$system$dialog_stack
conv_id
dial_stack

 
# ** LOOP THE CONVERSATION
repeat{
  chat <- readline(prompt="Enter response: ")
  response <- conversation.chat(chat,conv_id,dial_stack)
  #print(response)
  temp <- fromJSON(response)
  #temp
  text <- temp$output$text
  text  <- gsub("\\n", " ", text)
  text  <- gsub("'", "", text) ## apostrophes wreck the 'say' feature as
  print(text)
  #system(paste("say ",text[1]))
  watson.TTS.execute(base_url_TTS,URLencode(paste(text[1])),"en-GB_KateVoice","hp.wav")
  conv_id <-temp$context$conversation_id
  dial_stack <- temp$context$system$dialog_stack
  if((conv_id=='NA')||(chat == "")){
    print("end")
    break
    }
}

# watson.TTS.execute(base_url_TTS,"SORTING HAT","en-GB_KateVoice","hp.wav")
# closeAllConnections()

