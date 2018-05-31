data[, com.duration := date.amm.annee - date.declar.annee]

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

data[, libelle := NULL]
data[, substances := NULL]

rm(high.cardinality); gc()

saveRDS(data, "final_data_with_tm.rds")

