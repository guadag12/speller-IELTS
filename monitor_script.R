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
#install.packages(c("RttsDevelop", "randomNames", "postmastr", "generator"))
#remotes::install_github("slu-openGIS/postmastr")
#install.packages("googleLanguageR")
library(googleLanguageR)
library(RttsDevelop)
library(randomNames)
library(generator)
library(readr)
library(postmastr)
library(text2speech)
library(googleLanguageR)
library(stringi)
library(stringr)
library(splitstackshape)

# Generate Random Entities ------------------------------------------------

# sushi1
# names <- as.data.frame(randomNames(n= 50,
#             which.names="both",
#             name.order="last.first",
#             name.sep=", ",
#             sample.with.replacement=FALSE,
#             return.complete.data=FALSE))
# celphone <- as.data.frame(r_phone_numbers(50, use_parentheses = TRUE, use_hyphens = TRUE))
# credit_card <- as.data.frame(r_credit_card_numbers(50))
# email <- as.data.frame(r_email_addresses(50))
# dates <- as.data.frame(r_date_of_births(50))
# names2 <- as.data.frame(r_full_names(50))
# addresses <- read_csv("addresses.csv")
# addresses <- addresses[-c(51:100), ]
# money <- as.data.frame(paste0(round(runif(50, min=0, max=100), 2)," dollars"))
# numbers <- as.data.frame(round(runif(50, min=0, max=50000), 0))
# 
# 
# data <- cbind(names, names2, addresses[,1], celphone, dates,  email, credit_card, money, numbers)
# names(data)[1] <- "principal_member"
# names(data)[2] <- "optional_member"
# names(data)[3] <- "address"
# names(data)[4] <- "celphone"
# names(data)[5] <- "date_birth"
# names(data)[6] <- "email"
# names(data)[7] <- "credit_card_number"
# names(data)[8] <- "money"
# names(data)[9] <- "numbers"
# 
# data$principal_member <- gsub(pattern = ",", replacement = "", data$principal_member)
#data$principal_member <- strsplit( data$principal_member, "")[[1]]
#saveRDS(data, "data.rds")


# Read RDS & Access Key ---------------------------------------------------
data <- readRDS("data.rds")


tts_auth("google", key_or_json_file = "/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/sonorous-antler-.json")

# Optional member ---------------------------------------------------------

h <- "The name is "
i <- data$optional_member[1]
j <- "Do you need me to spell it out?"
k <- 1
for(i in data$optional_member){
  if(k < length(data$optional_member)){
    content <-append(paste0(h, i, " ", " ", j),  strsplit( i, "")[[1]])
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/names_second/names_s_", k, ".wav"))
    k =k+1
  }
  else{}
}



# Address ---------------------------------------------------------

h <- "The address is "
i <- data$address[1]
j <- "Do you need me to spell it out?"
k <- 1

addresses$number <- as.numeric(str_extract(addresses$Address, "[0-9]+"))
addresses <- cSplit(indt = addresses, splitCols = "Address", sep = ",")
addresses$Address_1 <- gsub(pattern = "[0-9]+ ", replacement = "", x = addresses$Address_1)
head(addresses)
names(addresses)[3] <- "Street"
names(addresses)[4] <- "Neighbourhood"
addresses[c(10, 15, 19, 24, 25, 33, 39, 48), "number"] <- c(123, 32, 544, 83, 5777, 2333, 5442, 45)
addresses$Neighbourhood <- as.character(addresses$Neighbourhood)
addresses$number <- as.character(addresses$number)
k = 1
str(addresses)
h <- "The number is "
i <- addresses$number[1]
j <- " of the street "
l <- addresses$Street[1]
n <- " in the neighbourhood of "
m <- as.character(addresses$Neighbourhood)[[1]]
o <- " Do you need that I spell it out? "
addresses$number[k]
for(i in addresses$number){
      if(k < length(data$address)){
       content <- append( paste0("The number is ", addresses$number[k], 
               " of the street ",  addresses$Street[k],
               " in the neighbourhood of ", addresses$Neighbourhood[k]), 
               paste0(" Do you need that I spell it out? "), strsplit(addresses$number[k], "")[[1]],
               paste0(" of the street "), strsplit( addresses$Street[k], "")[[1]], 
               paste0(" in the neighbourhood of "),
               strsplit( addresses$Neighbourhood[k], "")[[1]] 
               )
        
                
        col <- data.frame(column = content)
        col <- paste(unlist(col), collapse =" ")
        gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/address/address_", k, ".wav"))
        k = k+1
      }
      else{}
    }
  }
}

# Celphone ---------------------------------------------------------

h <- "The celphone is "
i <- data$celphone[1]
j <- "Do you need me to repeat it slowly?"
k <- 1
for(i in data$celphone){
  if(k < length(data$celphone)){
    content <-append(paste0(h, i, " ", " ", j),  strsplit( i, "")[[1]])
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/celphone/celphone_", k, ".wav"))
    k =k+1
  }
  else{}
}


# Date Birth ---------------------------------------------------------
data$date_birth <- format(data$date_birth, format="%d %B %Y")

h <- "The date of birth is "
i <- data$date_birth[1]
#j <- "Do you need me to repeat it slowly?"
k <- 1
for(i in data$date_birth){
  if(k < length(data$date_birth)){
    
    content <-paste0("The date of birth is ", i)
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/date_birth/date_", k, ".wav"))
    k =k+1
  }
  else{}
}

# Email ---------------------------------------------------------

h <- "The email is "
i <- data$email[1]
j <- "Do you need me to repeat it slowly?"
k <- 1
for(i in data$email){
  if(k < length(data$email)){
    content <-append(paste0(h, i, " ", " ", j),  strsplit( i, "")[[1]])
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/email/email_", k, ".wav"))
    k =k+1
  }
  else{}
}

# Credit Card ---------------------------------------------------------

h <- "The credit card number is "
i <- data$credit_card_number[1]
j <- "Do you need me to repeat it slowly?"
k <- 1
for(i in data$credit_card_number){
  if(k < length(data$credit_card_number)){
    content <-append(paste0(h, i, " ", " ", j),  strsplit( i, "")[[1]])
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/credit_card/credit_card_", k, ".wav"))
    k =k+1
  }
  else{}
}

# Money  ---------------------------------------------------------
meal <- c("coffee", "latte", "meal", "breakfast", "dinner", "hotel", "bungalow", "pancake", "cheescake", 
          "crumble")
#h <- paste0("The ", meal, " cost")
i <- data$money[1]
j <- "Do you need me to repeat it slowly?"
k <- 1
data$money <- gsub(data$money, pattern = "dollars", replacement = "")
for(i in data$money){
  if(k < length(data$money)){
    content <-paste0("The ", sample(meal, 1), " cost ", i, "dollars")
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/money/money_", k, ".wav"))
    k =k+1
  }
  else{}
}

# Number  ---------------------------------------------------------

i <- data$numbers[1]
#j <- "Do you need me to repeat it slowly?"
k <- 1
for(i in data$numbers){
  if(k < length(data$numbers)){
    content <-paste0("The number is ", i, "")
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/numbers/numbers_", k, ".wav"))
    k =k+1
  }
  else{}
}

# Hour  ---------------------------------------------------------

hour1 <-data.frame(hour = paste0(seq(1:5), ":", rep(x = 30, 5))) #15
hour2 <-data.frame(hour = paste0(seq(15:24), ":",rep(15, 9)))  #9
hour3 <-data.frame(hour = paste0(seq(10:15), ":", rep(x = 00, 5),  rep(x = 00, 15))) #15
hour4 <-data.frame(hour = paste0(seq(15:24), ":",rep(15, 9)))  #9
hour5 <-data.frame(hour = paste0(sample(seq(1:24), 10), ":", sample(seq(1:60), 10)))
hour <- rbind(hour1, hour2, hour3, hour4, hour5)
data <- cbind(data, hour)

data$hour[1:25] <- paste0(data$hour[1:25], " am")
data$hour[26:50] <- paste0(data$hour[26:50], " pm")
saveRDS(data, "/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/data.rds")
data <- read_rds("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/data.rds")

i <- data$hour[1]
#j <- "Do you need me to repeat it slowly?"
k <- 1
for(i in data$hour){
  if(k < length(data$hour)){
    content <-paste0("We meet them at ", i)
    col <- data.frame(column = content)
    col <- paste(unlist(col), collapse =" ")
    gl_talk(col, output = paste0("/Users/guadalupegonzalez/Documents/GitHub/politicxsentwitteR/speller/speller-IELTS/audios/hour/hour_", k, ".wav"))
    k =k+1
  }
  else{}
}

