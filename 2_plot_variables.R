#######  Missing value
##################################
missing.values <- transpose(data.frame(sapply(data, function(x) sum(is.na(x))/length(x))))
colnames(missing.values) = colnames(data)

missing.values <- gather(missing.values, key = "feature", value = "missing_pct")
missing.values %>% 
  ggplot(aes(x = reorder(feature,missing_pct),
             y = missing_pct)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() + theme_bw()

rm(missing.values)

###### Numeric variables
#################################

#------get the var type
var.type_ = sapply(data, function(x) class(x) %in% c("integer","numeric") && length(unique(x)) > 2)
num.var_ = names(var.type_[var.type_ == TRUE])
cat.var_ = names(var.type_[var.type_ == FALSE])
#check if some variables are not classified: 
length(var.type_) - length(num.var_) - length(cat.var_)
rm(var.type_)


#------ plot numeric variables
for(i in num.var_){
  # p_ = ggplot(data, aes(x = get(i), fill = as.factor(train.index))) +
  #         geom_histogram(aes(y=0.5 * ..density..)) +
  #         facet_wrap(~as.factor(train.index), nrow = 2) +
  #         ggtitle(paste0("histogram of ", i)) +
  #         labs(y = "Density", x = i)
  # print(p_)
  # ggsave(paste0(path_plot, "num_histogram ", i, ".png"))
}

###### categorical variables
#################################
high.cardinality = data.frame("variable" = as.character(), 
                              "number.modalities" = as.numeric())

high.cardi.thres = 20 #to be changed
for(i in cat.var_){
  if(length(unique(data[, get(i)])) <= high.cardi.thres){
    # p_ = emblem.plot(data[train.index == 1], i, response.var)
    # tmpFile <- paste0(path_plot, "cat_histogram of ", i, ".png")
    # export(p_, file = tmpFile)
  } else {
    high.cardinality = rbind(high.cardinality, 
                             data.frame("variable" = i, 
                                        "number.modalities" = length(unique(data[, get(i)]))))
    }
}
#output: high.cardinality data frame containing list of high cardinality variables

rm(cat.var_); rm(i); rm(high.cardi.thres); rm(num.var_)
