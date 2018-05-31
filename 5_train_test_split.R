data = readRDS("final_data_with_tm.rds")
#data = unclass(data) %>% data.table()

data = mltools::one_hot(data)
names(data) <- make.names(names(data))

#separate train and prediction set
train = data[train.index == 1, ]
prediction  = data[train.index == 0, ]

#separate training & testing set
K = 2 # on partitionne l'echantillon en 5
set.seed(123) # 
train$cv.id = sample(1:K, nrow(train), replace = T)

#error function
mape_error = function(y, ypred){mean(abs((y - ypred)/y))*100}
