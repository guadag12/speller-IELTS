#install.packages("RttsDevelop", repos="http://R-Forge.R-project.org")
#devtools::install_github("muschellij2/text2speech")
#install.packages("googleLanguageR")
#install.packages("randomNames")
# install.packages("remotes")
#remotes::install_github("slu-openGIS/postmastr")
#install.packages("generator")

######################################
############ LIBRARIES ###############
#####################################

library(RttsDevelop)
library(randomNames)
library(generator)
library(postmastr)
######################################
############ GENERATOR ##############
#####################################
sushi1
names <- as.data.frame(randomNames(n= 50,
            which.names="both",
            name.order="last.first",
            name.sep=", ",
            sample.with.replacement=FALSE,
            return.complete.data=FALSE))
celphone <- as.data.frame(r_phone_numbers(50, use_parentheses = TRUE, use_hyphens = TRUE))
credit_card <- as.data.frame(r_credit_card_numbers(50))
email <- as.data.frame(r_email_addresses(50))
dates <- as.data.frame(r_date_of_births(50))
names2 <- as.data.frame(r_full_names(50))
addresses <- read_csv("C:/Users/guada/Downloads/addresses.csv")
addresses <- addresses[-c(51:100), ]

data <- cbind(names,names2, addresses, celphone, dates,  email, credit_card)
names(data)[1] <- "principal_member"
names(data)[2] <- "optional_member"
names(data)[3] <- "address"
names(data)[4] <- "postal_code"
names(data)[5] <- "celphone"
names(data)[6] <- "date_birth"
names(data)[7] <- "email"
names(data)[8] <- "credit_card_number"

data$principal_member <- gsub(pattern = ",", replacement = "", data$principal_member)
data$principal_member <- strsplit( data$principal_member, "")[[1]]

h <- "The name is "
i <- data$principal_member[1]
j <- "Do you need me to spell it out?"
k <- 1
for(i in data$principal_member){
  if(k < length(data$principal_member)){
    content <-append(paste0(h, i, " ", " ", " ", " " j) ,  
      strsplit( i, "")[[1]])
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("D:/Guada/TOEFL 2021/dictator/dictator/names/names_", k, ".wav"))
    k =k+1
  }
  else{}
  }

content <- "A common mistake that people make when trying to design something completely foolproof is to underestimate the ingenuity of complete fools."
speakers = c("Bruce")
tts_ITRI(col,
         destfile = paste0("audio_tts_", x, ".mp3"))
lapply(speakers, function(x) tts_ITRI(content, speaker = x,
                                      destfile = paste0("audio_tts_", x, ".mp3")))

library(text2speech)
library(googleLanguageR)
tts_auth("google", key_or_json_file = "sonorous-antler-318800-28a12c8221e8.json")
gl_talk("Would you like a cup of tea?", gender = "FEMALE", languageCode = "en-GB")
gl_talk("This is my audio player") %>% gl_talk_player(html = paste0("file:///D:/Guada/TOEFL%202021/dictator/dictator/", player, ".html"))