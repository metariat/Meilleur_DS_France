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



###############################################################
###                Regroup rare modalities                 ####
###############################################################

p = 0.01 #to be changed
#all the level with exposure inferior to p will be grouped together

#----------- The systematic way
#######################################

#self coding
# for(i in high.cardinality$variable){
#   t_ <- table(data[, get(i)])
#   y_ <- subset(t_, prop.table(t_) < p)
#   z_ <- subset(t_, prop.table(t_) >= p)
#   other_ <- rep("other", sum(y_))
#   new.table_ <- c(z_, table(other_))
#   data[, (i) := as.factor(rep(names(new.table_), new.table_))]
# }

#using pre-written function
for(i in high.cardinality$variable){
  data[, (i) := combine.levels(get(i), minlev = p)]
}
rm(p)



###############################################################
###            Simplify the deconcanated variables         ####
###############################################################
data[, (conca.var.1_) := rowSums(.SD), .SDcols = list.deconca.1_]
data[, (conca.var.2_) := rowSums(.SD), .SDcols = list.deconca.2_]


simplify.thres = 0.03

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
rm(simplify.thres); rm(var_); rm(high.cardinality); gc()

