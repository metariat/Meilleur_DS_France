##########################################################
#####           Threshold detection            ###########
##########################################################

thres = c()

for (thres_ in exp(seq(log(16), log(800), len = 5))){ #to be changed
  train.cv_ = train[cv.id != 1, ]
  test.cv_ = train[cv.id == 1, ]
  thres_ = round(thres_, 0)
  #application of the threshold
  train.cv_ = train.cv_[abs(get(response.var)) < thres_]
  
  #data set preparation
  y.train_ = train.cv_[, get(response.var)]
  x.train_ = subset(train.cv_, select= -c(get(response.var), key))
  x.train_ = as.matrix(x.train_)
  
  y.test_ = test.cv_[, get(response.var)]
  x.test_ = subset(test.cv_, select= -c(get(response.var), key))
  x.test_ = as.matrix(x.test_)
  
  dtrain_ = xgb.DMatrix(as.matrix(x.train_), label = log(y.train_))
  dtest_ = xgb.DMatrix(as.matrix(x.test_), label = y.test_)
  
  param <- list(objective           = "reg:linear",
                eval_metric         = "mae")
  
  model_ = xgb.train(param, data = dtrain_, nrounds = 500, nthread = 16)
  
  #testing mae
  xgb.pred_ = exp(predict(model_, new = x.test_))
  error_ = round(mape_error(y.test_, xgb.pred_), 2)
  print(paste0('threshold: ', thres_, ', error: ', error_))
  rm(train.cv_); rm(test.cv_); rm(thres_); rm(error_);
  rm(y.train_); rm(x.train_); rm(y.test_); rm(x.test_);
  rm(dtrain_); rm(dtest_); rm(model_); rm(xgb.pred_); 
}
  
##########################################################
#####         Model training & tuning          ###########
##########################################################
best.thres = 100


train.temp_ = train[abs(get(response.var)) < best.thres]

y.train_ = train.temp_[, get(response.var)]
x.train_ = subset(train.temp_, select= -c(get(response.var), key, cv.id))
x.train_ = as.matrix(x.train_)

x.test_ = subset(prediction, select= -c(get(response.var), key))
x.test_ = as.matrix(x.test_)

dtrain_ = xgb.DMatrix(data = x.train_, label = log(y.train_))
dtest_ = xgb.DMatrix(data = x.test_, label = rep(1, dim(x.test_)[1]))

param <- list(objective    = "reg:linear",
              eval_metric  = "rmse",
              #max_depth    = 10,
              #min_child_weight = 10,
              #etat         = 0.1,
              nthread      = 16)

mod = xgb.cv(dtrain_, 
             params                = param,
             nfold                 = 2,
             nrounds               = 3000,
             print_every_n         = 5,
             early_stopping_rounds = 50)
plot(mod$evaluation_log$train_rmse_mean, type = "l")
lines(mod$evaluation_log$test_rmse_mean, type = "l", col = "red")

best.round_ = mod$best_iteration

mod = xgb.train(dtrain_, 
                params  = param, 
                nrounds = best.round_, 
                verbose = F)

test.pred_ = exp(predict(mod, dtest_))

submission = data.table("id" = prediction$identifiant,
                        "prix" = test.pred_)

fwrite(submission, "C:/Best_Data_Scientist/submission/submission_xgb.csv", sep = ";")



##########################################################
#####                  Stacking                ###########
##########################################################
stack.cv = data.table()
for (i in 1:K){
  train.cv_ = train[!cv.id == i, ]
  train.cv_ = train.cv_[abs(get(response.var)) < best.thres]
  test.cv_ = train[cv.id == i, ]
  stack.cv_ = subset(test.cv_, select= c(key))
  
  y.train_ = train.cv_[, get(response.var)]
  x.train_ = subset(train.cv_, select= -c(get(response.var), key))
  x.train_ = as.matrix(x.train_)
  
  y.test_ = test.cv_[, get(response.var)]
  x.test_ = subset(test.cv_, select= -c(get(response.var), key))
  x.test_ = as.matrix(x.test_)
  
  dtrain_ = xgb.DMatrix(data = x.train_, label = log(y.train_))
  dtest_ = xgb.DMatrix(data = x.test_, label = log(y.test_))
  
  mod = xgb.train(dtrain_, 
                  params  = param, 
                  nrounds = best.round_, 
                  verbose = F)
  
  train.pred_ = exp(predict(mod, dtrain_))
  test.pred_ = exp(predict(mod, dtest_))
  
  print(sprintf("fold %s, training error :%s, testing error: %s",
                i,
                round(mape_error(y.train_, train.pred_), 2),
                round(mape_error(y.test_, test.pred_), 2)))
  
  stack.cv_[, pred := test.pred_]
  stack.cv = rbind(stack.cv, stack.cv_)
}

fwrite(stack.cv, "C:/Best_Data_Scientist/stacking/xgboost_prediction.csv")

rm(mod); rm(param); rm(stack.cv); rm(stack.cv_); rm(submission); rm(test.cv_);
rm(train.cv_); rm(train.temp_); rm(x.test_); rm(x.train_); rm(best.round_);
rm(best.thres); rm(dtrain_); rm(dtest_); rm(i); rm(train.pred_); rm(test.pred_);
rm(y.test_); rm(y.train_); gc()
