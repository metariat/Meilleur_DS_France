###############################################################
###              Missing value imputation                  ####
###############################################################
data[is.na(data)] = -999



###############################################################
###                Feature Engineering                     ####
###############################################################
source("C:/Best_Data_Scientist/00_word_frequency_function.R")
worf = WordFrequency(data$libelle)
worf[1:10, ]
data[, libelle.thermo := as.numeric(grepl("thermoformée", libelle))]
data[, libelle.aluminium := as.numeric(grepl("aluminium", libelle))]
data[, libelle.pvc := as.numeric(grepl("pvc", libelle))]
data[, libelle.pvdc := as.numeric(grepl("pvdc", libelle))]
rm(worf)


############## Text mining ###################################
test = TextFeatureGeneraator(df = data, feature = "substances", id = "key", 
                             lan = "french", pruning.level = 100)
data = cbind(data, test)

test = TextFeatureGeneraator(df = data, feature = "libelle", id = "key", 
                             lan = "french", pruning.level = 100)
data = cbind(data, test)

###############################################################
###         Deconcatenate the concatenated variables       ####
###############################################################


#### 1.voies.admid
#############################

conca.var.1_ = "voies.admin"
levels = as.vector(unique(data[, get(conca.var.1_)])) %>%
           strsplit(., ",") %>%
           unlist() %>%
           unique()

list.deconca.1_ = c()
for (j_ in levels){
  var_ = paste0(conca.var.1_, '.', j_)
  list.deconca.1_ = c(list.deconca.1_, var_)
  data[, (var_) := as.numeric(grepl(j_, get(conca.var.1_)))]
}

#### 2. titulaires
#############################

conca.var.2_ = "titulaires"
levels = as.vector(unique(data[, get(conca.var.2_)])) %>%
  strsplit(., ",") %>%
  unlist() %>%
  unique()

levels = lapply(levels,
                function(x)
                  gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]]))
levels = unlist(levels) %>% make.names() %>% unique()
list.deconca.2_ = c()
for (j_ in levels){
  var_ = paste0(conca.var.2_, '.', j_)
  list.deconca.2_ = c(list.deconca.2_, var_)
  data[, (var_) := as.numeric(grepl(j_, get(conca.var.2_)))]
}

#### 3. substances
#############################
conca.var.3_ = "substances"
levels = as.vector(unique(data[, get(conca.var.3_)])) %>%
  strsplit(., ",") %>%
  unlist() %>%
  unique()

list.deconca.3_ = c()

library(doParallel)
library(foreach)
data.temp = data.table("test.col" = rep(1, nrow(data)))

cores=detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)
data.temp <- foreach(i=1:length(levels), .combine = cbind, .packages='data.table') %dopar% {
  var.temp_ = levels[i]
  temp = data.table("g" = as.numeric(grepl(var.temp_, data[, get(conca.var.3_)]))) #calling a function
  setnames(temp, "g", var.temp_)
  temp #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}
stopCluster(cl)
data = cbind(data, data.temp)
data = subset(data, select = which(!duplicated(names(data)))) 

# for (j_ in levels){
#   var_ = paste0(conca.var.3_, '.', j_)
#   list.deconca.3_ = c(list.deconca.3_, var_)
#   data[, (var_) := as.numeric(grepl(j_, get(conca.var.3_)))]
# }



###############################################################
###            Simplify the deconcanated variables         ####
###############################################################
data[, (conca.var.1_) := rowSums(.SD), .SDcols = list.deconca.1_]
data[, (conca.var.2_) := rowSums(.SD), .SDcols = list.deconca.2_]
data[, (conca.var.3_) := rowSums(.SD), .SDcols = list.deconca.3_]

simplify.thres = 0.01

# first var
conca.var.1.sim_ = paste0(conca.var.1_, '.quang.other')
data[, (conca.var.1.sim_) := 0]
for (i in list.deconca.1_){
  if (sum(data[, get(i)]) <= nrow(data) * simplify.thres){
    data[, (conca.var.1.sim_) := pmax(get(conca.var.1.sim_),
                                      get(i))]
    data[, (i) := NULL]
  }
}

# second var
conca.var.2.sim_ = paste0(conca.var.2_, '.quang.other')
data[, (conca.var.2.sim_) := 0]
for (i in list.deconca.2_){
  if (sum(data[, get(i)]) <= nrow(data) * simplify.thres){
    data[, (conca.var.2.sim_) := pmax(get(conca.var.2.sim_),
                                      get(i))]
    data[, (i) := NULL]
  }
}

# third var
conca.var.3.sim_ = paste0(conca.var.3_, '.quang.other')
data[, (conca.var.3.sim_) := 0]
for (i in list.deconca.3_){
  if (sum(data[, get(i)]) <= nrow(data) * simplify.thres){
    data[, (conca.var.3.sim_) := pmax(get(conca.var.3.sim_),
                                      get(i))]
    data[, (i) := NULL]
  }
}




#----------- The Emblem way
#######################################

#To be developped  #To be developped  #To be developped  #To be developped 
#To be developped  #To be developped  #To be developped  #To be developped  
#To be developped  #To be developped  #To be developped  #To be developped  
#To be developped  #To be developped  #To be developped  #To be developped  
#To be developped  #To be developped  #To be developped  #To be developped  
#To be developped  #To be developped  #To be developped  #To be developped  
#To be developped  #To be developped  #To be developped  #To be developped  
#To be developped  #To be developped  #To be developped  #To be developped  
#To be developped  #To be developped  #To be developped  #To be developped  


rm(conca.var.1_); rm(conca.var.1.sim_); rm(conca.var.2_); rm(conca.var.2.sim_);
rm(i); rm(j_); rm(levels); rm(list.deconca.1_); rm(list.deconca.2_);
rm(simplify.thres); rm(var_); gc()

