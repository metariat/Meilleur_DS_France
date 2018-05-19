##########################################################
#####           Threshold detection            ###########
##########################################################

thres = c()

for (thres_ in exp(seq(log(10), log(800), len = 10))){ #to be changed
  train.cv_ = train[cv.id != 3, ]
  test.cv_ = train[cv.id == 3, ]
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
  
  dtrain_ = xgb.DMatrix(as.matrix(x.train_), label = y.train_)
  dtest_ = xgb.DMatrix(as.matrix(x.test_), label = y.test_)
  
  param <- list(objective           = "reg:linear",
                eval_metric         = "mae")
  
  model_ = xgb.train(param, data = dtrain_, nrounds = 1000)
  
  #testing mae
  xgb.pred_ = predict(model_, new = dtest_)
  error_ = round(mape_error(y.test_, xgb.pred_), 2)
  print(paste0('threshold: ', thres_, ', error: ', error_))
  rm(train.cv_); rm(test.cv_); rm(thres_); rm(error_);
  rm(y.train_); rm(x.train_); rm(y.test_); rm(x.test_);
  rm(dtrain_); rm(dtest_); rm(model_); rm(xgb.pred_); 
}
  
##########################################################
#####               Model training             ###########
##########################################################
best.thres = 20

stack.cv = data.table()
for (i in 1:5){
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
  
  param <- list(objective           = "reg:linear",
                eval_metric         = "mae")
  
  mod = xgb.train(dtrain_, params = param, 
                  max_depth = 15, 
                  eta = 0.1,
                  nrounds = 1000, 
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

