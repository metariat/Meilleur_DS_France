library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(webshot) #export plotly png
library(caret)
library(MLmetrics)
library(glmnet)
library(randomForest)
library(xgboost)
library(catboost)
library(Hmisc) #regrouping levels
library(tm)
library(doParallel)
library(stringr)
library(text2vec)
library(quantreg)
library(ranger)

path_plot = "C:/Users/xq.do/Downloads/CLE_USB/CLE_USB/plot/"

setwd("C:/Users/xq.do/Downloads/CLE_USB/CLE_USB/data_challenge")
source("C:/Best_Data_Scientist/function_emblem_plot.R")
source("C:/Best_Data_Scientist/function_string_clean.R")
source("C:/Best_Data_Scientist/function_text_mining.R")

###############################################################
###              Read the data                             ####
###############################################################
train = fread("boites_medicaments_train.csv", stringsAsFactors = T, encoding = "UTF-8")
test = fread("boites_medicaments_test.csv", stringsAsFactors = T, encoding = "UTF-8")

response.var = "prix"

train[, train.index := 1]
test[, train.index := 0]
test[, (response.var) := -1]
setnames(test, "id", "identifiant")
train[, identifiant := -1]

#plot the response variable
hist(train$prix, breaks = 300)

#keep only the common columns between train / test set
col = intersect(names(train), names(test))
train = train[, col, with = F]
test = test[, col, with = F]


########     Mix them altogether 
########################################
data = rbind(train, test)
data[, key := seq(1, nrow(data))]
rm(train); rm(test)
gc()

#rename to the standard naming following google style
names(data) = tolower(names(data))
names(data) <- make.names(names(data), unique = TRUE)
names(data) <- gsub("_", ".", names(data))


###### Apply some corrections
#######################################

####### Convert date variables if necessary
# lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
# as.Date(parse_date_time(x,'%y-%m-%d'))
# Sys.setlocale("LC_TIME", lct)
#date format here: https://www.r-bloggers.com/date-formats-in-r/

##### Replace some unwanted character
data[, tx.rembours := as.numeric(gsub("%", "", tx.rembours, fixed = T))]
data[, libelle := gsub("(s)", "", libelle, fixed = T)]
#to be changed


###### Define numeric vs categorical variables
#######################################

#------get the var type
var.type_ = sapply(data, function(x) class(x) %in% c("integer","numeric"))
num.var_ = names(var.type_[var.type_ == TRUE])
cat.var_ = names(var.type_[var.type_ == FALSE])
#check if some variables are not classified: 
length(var.type_) - length(num.var_) - length(cat.var_)
rm(var.type_)



###### Clean
data[, libelle := StringClean(libelle, digit.del = 0, keep.only.char = 0,
                              stop.lan = "french",
                              custom.stop.words = NULL) %>% 
                  as.factor()]
data[, substances := StringClean(substances, digit.del = 0, keep.only.char = 0,
                              stop.lan = "french",
                              custom.stop.words = NULL) %>% 
                     as.factor()]

###### Convert numeric variable to character
cat.var = c("") #to be changed


###### Convert 2 level variables to numeric
#######################################
for (i in cat.var_){
  if (length(unique(data[, get(i)])) == 2){
    data[, (i) := as.numeric(get(i))]
  }
}

rm(cat.var); rm(cat.var_); rm(i); rm(num.var_); rm(col)
