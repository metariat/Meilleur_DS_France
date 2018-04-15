###############################################################
###              Missing value imputation                  ####
###############################################################
data[is.na(data)] = -999



###############################################################
###                Regroup rare modalities                 ####
###############################################################

i = "forme.pharma"
p = 0.01

#----------- The systematic way
#######################################
for(i in high.cardinality$variable){
  t_ <- table(data[, get(i)])
  y_ <- subset(t_, prop.table(t_) < p)
  z_ <- subset(t_, prop.table(t_) >= p)
  other_ <- rep("other", sum(y_))
  new.table_ <- c(z_, table(other_))
  data[, (i) := as.factor(rep(names(new.table_), new.table_))]
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




