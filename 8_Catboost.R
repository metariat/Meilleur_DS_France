##########################################################
#####                Training                  ###########
##########################################################
train.cv_ = train[cv.id != 1, ]
test.cv_ = train[cv.id == 1, ]

##########################################################
#####                  Tuning                  ###########
y.train_ = train.cv_[, get(response.var)]
x.train_ = subset(train.cv_, select= -c(get(response.var), key, cv.id))

y.test_ = test.cv_[, get(response.var)]
x.test_ = subset(test.cv_, select= -c(get(response.var), key, cv.id))

train_pool_ = catboost.load_pool(data  = x.train_,
                                 label = log(y.train_))
test_pool_  = catboost.load_pool(data  = x.test_,
                                 label = log(y.test_))

nb.core = detectCores()

fit_params <- list(iterations = 10000,
                   loss_function = 'RMSE',
                   depth = 12,
                   learning_rate = 0.1,
                   #l2_leaf_reg = 5,
                   #logging_level = 'Silent',
                   thread_count = nb.core,
                   od_type = "Iter",
                   od_wait = 20)
model <- catboost.train(train_pool_, test_pool_, fit_params)
best.round_ = model$tree_count

#prediction
cat.pred_ <- exp(catboost.predict(model, test_pool_))
error_ = round(mape_error(y.test_, cat.pred_), 2)
print(error_)



#######################################################
#########         Diagonostic            ##############
print(sprintf("predicted mean: %s, observed mean: %s", 
              round(mean(cat.pred_), 2), 
              round(mean(y.test_), 2)))

print(sprintf("predicted median: %s, observed median: %s", 
              round(median(cat.pred_), 2), 
              round(median(y.test_), 2)))

taux_ = 1
cat.pred.temp_ = cat.pred_ * taux_
print(round(mape_error(y.test_, cat.pred.temp_), 2))


##########################################################
#####                Training                  ###########
y.train_ = train[, get(response.var)]
x.train_ = subset(train, select= -c(get(response.var), key, cv.id))

x.test_ = subset(prediction, select= -c(get(response.var), key))

train_pool_ = catboost.load_pool(data  = x.train_,
                                 label = log(y.train_))
test_pool_  = catboost.load_pool(data  = x.test_,
                                 label = NULL)

fit_params[["iterations"]] = best.round_
fit_params[["od_type"]] = NULL
fit_params[["od_wait"]] = NULL

model <- catboost.train(train_pool_, NULL, fit_params)

#prediction
cat.pred_ <- exp(catboost.predict(model, test_pool_))
submission = data.table("id" = prediction$identifiant,
                        "prix" = cat.pred_)
fwrite(submission, "C:/Best_Data_Scientist/submission/submission_cat.csv", sep = ";")


##########################################################
#####                 Stacking                 ###########
##########################################################

stack.cv = data.table()
for (i in 1:K){
  train.cv_ = train[!cv.id == i, ]
  test.cv_ = train[cv.id == i, ]
  stack.cv_ = subset(test.cv_, select= c(key))
  
  y.train_ = train.cv_[, get(response.var)]
  x.train_ = subset(train.cv_, select= -c(get(response.var), key, cv.id))
  
  y.test_ = test.cv_[, get(response.var)]
  x.test_ = subset(test.cv_, select= -c(get(response.var), key, cv.id))
  
  train_pool_ = catboost.load_pool(data  = x.train_,
                                   label = log(y.train_))
  test_pool_  = catboost.load_pool(data  = x.test_,
                                   label = log(y.test_))
  
  
  model <- catboost.train(train_pool_, NULL, fit_params)
  
  train.pred_ = exp(catboost.predict(model, train_pool_))
  test.pred_ = exp(catboost.predict(model, test_pool_))
  
  print(sprintf("fold %s, training error :%s, testing error: %s",
                i,
                round(mape_error(y.train_, train.pred_), 2),
                round(mape_error(y.test_, test.pred_), 2)))
  
  stack.cv_[, pred := test.pred_]
  stack.cv = rbind(stack.cv, stack.cv_)
}

fwrite(stack.cv, "C:/Best_Data_Scientist/stacking/catboost_prediction.csv")

rm(mod); rm(param); rm(stack.cv); rm(stack.cv_); rm(submission); rm(test.cv_);
rm(train.cv_); rm(train.temp_); rm(x.test_); rm(x.train_); rm(best.round_);
rm(best.thres); rm(dtrain_); rm(dtest_); rm(i); rm(train.pred_); rm(test.pred_);
rm(y.test_); rm(y.train_); gc()

