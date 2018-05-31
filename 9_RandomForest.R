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

  rf = ranger(log(prix) ~ ., data = train.cv_, num.trees = 500)
  
  #testing mae
  rf.pred_ = exp(predict(rf, test.cv_)$predictions)
  error_ = round(mape_error(rf.pred_, test.cv_$prix), 2)
  print(paste0('threshold: ', thres_, ', error: ', error_))
  rm(train.cv_); rm(test.cv_); rm(thres_); rm(error_);
  rm(y.train_); rm(x.train_); rm(y.test_); rm(x.test_);
  rm(dtrain_); rm(dtest_); rm(model_); rm(xgb.pred_); 
}


##########################################################
#####                 Training                 ###########
##########################################################
best.thres = 800


train.temp_ = train[abs(get(response.var)) < best.thres]
train.temp_[, cv.id := NULL]
rf = ranger(log(prix) ~ ., data = train.temp_, num.trees = 1500, importance = "impurity")

test.pred_ = exp(predict(rf, prediction)$predictions)

submission = data.table("id" = prediction$identifiant,
                        "prix" = test.pred_)

fwrite(submission, "C:/Best_Data_Scientist/submission/submission_rf.csv", sep = ";")

##########################################################
#####                   Stacking               ###########
##########################################################

stack.cv = data.table()
for (i in 1:K){
  train.cv_ = train[!cv.id == i, ]
  train.cv_ = train.cv_[abs(get(response.var)) < best.thres]
  test.cv_ = train[cv.id == i, ]
  stack.cv_ = subset(test.cv_, select= c(key))

  rf = ranger(log(prix) ~., data =  train.cv_, num.trees = 500)
  
  train.pred_ = exp(predict(rf, train.cv_)$predictions)
  test.pred_ = exp(predict(rf, test.cv_)$predictions)
  
  print(sprintf("fold %s, training error :%s, testing error: %s",
                i,
                round(mape_error(train.cv_$prix, train.pred_), 2),
                round(mape_error(test.cv_$prix, test.pred_), 2)))
  
  stack.cv_[, pred := test.pred_]
  stack.cv = rbind(stack.cv, stack.cv_)
}

fwrite(stack.cv, "C:/Best_Data_Scientist/stacking/rf_prediction.csv")
