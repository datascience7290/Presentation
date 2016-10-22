library(RCurl)
library(httr)
library(XML)
library(data.table)
library(reshape2)
library(dplyr)
library(plyr)
library(stringr)

## LANGUAGE Translation & Identification - CREDENTIALS from SERVICE
setwd("/Users/hinagandhi/desktop/")  
url="https://gateway.watsonplatform.net/language-translator/api"
username="fc997a64-239b-40f8-9b9f-88b0e73177f1"
password="OrjFfTdtsJlO"
username_password = paste(username,":",password,sep="")


# FUNCTION DECLARE: LIST LINGOS - What languages does watson speak? # quick check - LIST OF AVAILABLE LANGUAGES = Identifies the language of the input text
watson.language.list <- function()
{
  response <- getURL(paste(url,"/v2/identifiable_languages",sep=""),userpwd = username_password)
  response <- as.data.frame(strsplit(as.character(response),"\"language\":\""))
  response <- data.frame(do.call('rbind', strsplit(as.character(response[,1]),'\"',fixed=TRUE)))
  response <- subset(response, select=c("X1", "X5"))
  response <- response[-c(1), ] # remove first row
  return(response)
}

#OK lets Test:
language_lookup <- watson.language.list()
language_lookup
options(warn=-1)


###### FUNCTION - ANalyze TEXT/DOC RECEIVED and return language with confidence
watson.language.analyze <- function(text_to_analyze)
{
  response <- (POST(url=paste(url,"/v2/identify",sep=""),
                    authenticate(username,password),
                    add_headers("Content-Type"="text/plain","charset"="utf-8" ),
                    body = text_to_analyze
  ))
  response <- content(response, "text")
  response <- as.data.frame(strsplit(as.character(response),"\"language\":\""))
  response <- data.frame(do.call('rbind', strsplit(as.character(response[,1]),'\"',fixed=TRUE)))
  response <- subset(response, select=c("X1", "X4"))
  response <- response[-c(1), ] # remove first row
  response$X4 <- gsub("[{error: no user with username ',:' was found}]","",response$X4)
  response$X4 <- round(as.numeric(response$X4),4)
  head(response,5)
  response <- (merge(response, language_lookup, by = 'X1'))
  response <- response[order(-response$X4), ]  
  rownames(response) <- NULL
  colnames(response) <- c("ln","confidence","language")
  return(response)
}


## TESTING - Parlez vous francais?
TEXT <- "bonjour!  comment ca va?"
TEXT <- "Örülök, hogy a városban élő"
TEXT <- "ويسرني أن تكون البرمجة"
TEXT <- "私は都市に住んでいることを嬉しく思います"
TEXT <- "我很高興能住在城市"
TEXT <- "நான் நகரத்தில் வாழும் வேண்டும் மகிழ்ச்சி அடைகிறேன்"
TEXT <- "میں شہر میں رہنے کے لئے خوش ہوں"
TEXT <- "Ես գոհ եմ, որ ապրում է քաղաքի"
TEXT <- "Я радий жити в місті"
TEXT <- "Би хотод амьдарч байгаадаа баяртай байна"
TEXT <- "Είμαι στην ευχάριστη θέση να ζουν στην πόλη"
language_analysis <- watson.language.analyze(TEXT)
head(language_analysis)


#######################
# reading csv file with one column for languages
data <- read.csv("language_samples_raw.csv", header = FALSE) # no header, one single CSV column # need more work to handle the chinese,japanese characters
data <- data.frame(data)
data
length <- dim(data)[1] # get height of the list, so we can size loop correctly.
data$output_language <- ""
data$output_confidence <- ""

for (i in 1:length)
{
  text <- paste(data$V1[i])
  language_analysis <- watson.language.analyze(text)
  temp <- as.data.frame(language_analysis[1,])
  print(paste(i," | Language:",temp$language, " | Confidence:",temp$confidence," |  text analyzed:",text))
  #print(output) #show in terminal
  data$output_language[i] <- paste(temp$language)
  data$output_confidence[i] <- temp$confidence
}
head(data)
write.csv(data, file = "language_post_analysis.csv")

#=========


### FUNCTION - TRANSLATE AN ENGLISH SOURCE INTO REQUESTED LANGUAGE
##### FUNCTION - ANalyze TEXT/DOC RECEIVED and return language with confidence
watson.language.translate1 <- function(text_input,language_to)
{
  response <- (POST(url=paste(url,"/v2/translate",sep=""),
                    authenticate(username,password),
                    body = list(source = "en",
                                target = language_to,
                                text = text_input
                    )
  ))
  return(response)
}

# model_id": en-es-conversational.  Options: Conversational News Patent

# Translation is available among Arabic, English, French, Portuguese, and Spanish (KOrean & Chinese depend on region)
translate_this <- "For gardens to flourish, they need good soil and regular water"
translate_to <- "fr" # less options here - en, fr,es (spanish), ar (arabic), pt (portugese)
reply <- watson.language.translate1(translate_this,translate_to)


##### FUNCTION - ANalyze TEXT/DOC RECEIVED and return language with confidence
watson.language.translate.all <- function(text_input)
{
  #test[1] = paste(content(watson.language.translate1("The IBM Watson Language Translation service converts text input in one language into a destination language for the end user using background from domain-specifc models. Translation is available among Arabic, English, French, Portuguese, and Spanish","fr"),"text"),"fr")
  test = content(watson.language.translate1(text_input,"pt"))
  #test[2] = paste(content(watson.language.translate1(text_input,"es"),"text"),"es")
  #test[3] = paste(content(watson.language.translate1(text_input,"pt"),"text"),"pt")
  #test[4] = paste(content(watson.language.translate1(text_input,"ar"),"text"),"ar")
  test <- data.frame(test)
  return(test)
}
# calling translate.all function for each line of csv
for (i in 1:length)
{
  text <- paste(data$V1[i])
  language_analysis <- watson.language.translate.all(text)
  temp <- as.data.frame(language_analysis[1,])
  print(paste(i," | Translation:",temp$translation, " | Word Count:",temp$word_count))
  data$output_translation[i] <- paste(temp$translation)
  data$output_word_count[i] <- temp$word_count
}
View(data)
write.csv(data,"language_translation.csv")