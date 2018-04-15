library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(webshot) #export plotly png

path_plot = "C:/Users/xq.do/Downloads/CLE_USB/CLE_USB/plot/"

setwd("C:/Users/xq.do/Downloads/CLE_USB/CLE_USB/data_challenge")
source("C:/Best_Data_Scientist/function_emblem_plot.R")
source("C:/Best_Data_Scientist/function_merge_factors.R")

###############################################################
###              Read the data                             ####
###############################################################
train = fread("boites_medicaments_train.csv", stringsAsFactors = T)
test = fread("boites_medicaments_test.csv", stringsAsFactors = T)

response.var = "prix"

train[, train.index := 1]
test[, train.index := 0]
test[, prix := -1]

hist(train$prix, breaks = 300)

col = intersect(names(train), names(test))
train = train[, col, with = F]
test = test[, col, with = F]


########     Mix them altogether 
########################################
data = rbind(train, test)
rm(train); rm(test)
gc()

names(data) = tolower(names(data))
names(data) <- make.names(names(data), unique = TRUE)
names(data) <- gsub("_", ".", names(data))



###### Apply some corrections
#######################################


####### Convert date variables if necessary
# lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
# as.Date(parse_date_time(x,'%y-%m-%d'))
# Sys.setlocale("LC_TIME", lct)


##### Replace some unwanted character
data[, tx.rembours := as.numeric(gsub("%", "", tx.rembours, fixed = T))]