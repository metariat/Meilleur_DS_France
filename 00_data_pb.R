library(data.table)

setwd("C:/Users/xq.do/Downloads/CLE_USB/CLE_USB/data_challenge")

###############################################################
###              Read the data                             ####
###############################################################
train = fread("boites_medicaments_train.csv", stringsAsFactors = T, encoding = "UTF-8")
nrow(unique(train)) #d�j� environ 400 doublons, mais cela ne pose pas de probl�me.
nrow(unique(train[, prix := NULL])) #691 boites qui trouve au moins une boite de 
#m�me caracteristics mais different prix