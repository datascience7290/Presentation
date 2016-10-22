######################################################
### Experimental Code.  Experimental R Interface for IBM Watson Services -
### Focus: IBM Watson Tone Analyzer is a service that helps people comprehend, consume, and revise the language tone of their writing for more efficient communications 
##
######################################################

library(RCurl) # to talk to Watson - REST APIS # install.packages("RCurl") # if the package is not already installed
library(httr) # comms
library(XML)  # comms and data
library(data.table) # data shaping
library(reshape2) # data shaping
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(png) # for the presenting of images


###Setting Working Directory
setwd("/Users/shivamgoel/desktop/R Assignment")


### GET - Authentication
getURL("https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19&text=hello",userpwd = "1e4f3ac7-17a1-4b21-9be7-e8efbc2a20c3:NIs5EJmwfTbU" ) 

### Function to process output from API and table
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data <- data[-c(1), ] # remove dud first row
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),2)
  setnames(data,c("trait","signal"))
  return(data)
}


### FUNCTION to post data to Tone Analyzer and return results
process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19",
                   authenticate("1e4f3ac7-17a1-4b21-9be7-e8efbc2a20c3","NIs5EJmwfTbU"),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"), 
                   body=text )
  
  response_text <- content(response, "text", encoding = "UTF-8")  # or encoding = "ISO-8859-1"
  abc <- tidyResponse(response_text)
  return(abc)
}


process_data_to_tone("I am a not good person. I am a good person") # not perfect, but good enough for now!
#query <- "I am super scared and nervous about bees"
#process_data_to_tone(query)
View(analysis)
# trait signal
# 1              anger   0.01
# 2            disgust   0.02
# 3               fear   0.99
# 4                joy   0.03
# 5            sadness   0.02
# 6         analytical   0.00
# 7          confident   0.00
# 8          tentative   0.00
# 9           openness   0.21
# 10 conscientiousness   0.22
# 11      extraversion   0.76
# 12     agreeableness   0.37
# 13       neuroticism   0.99










graphics.off()
