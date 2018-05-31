##########################################################
#####           Threshold detection            ###########
##########################################################

thres = c()
for (thres_ in exp(seq(log(20), log(200), len = 5))){ #to be changed
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
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  cv.out_ = cv.glmnet (x.train_, log(y.train_), 
                       alpha = 1, nfolds = 2, 
                       type.measure = "mae", 
                       parallel = T)
  stopCluster(cl)
  
  bestlam_ = cv.out_$lambda.min #best lambda
  
  #testing mae
  lasso.pred.test_ = exp(predict(cv.out_, s = bestlam_, newx = x.test_))
  error_ = round(mape_error(y.test_, lasso.pred.test_), 2)
  print(paste0('threshold: ', thres_, ', error: ', error_))
}





##########################################################
#####                 Training                 ###########
##########################################################
best.thres = 60

train.cv_ = train[abs(get(response.var)) < best.thres]
test.cv_ = prediction

y.train_ = train.cv_[, get(response.var)]
x.train_ = subset(train.cv_, select= -c(get(response.var), key, cv.id))
x.train_ = as.matrix(x.train_)
  
x.test_ = subset(test.cv_, select= -c(get(response.var), key))
x.test_ = as.matrix(x.test_)
  
#Model
cl <- makeCluster(detectCores())
registerDoParallel(cl)
cv.out_ = cv.glmnet (x.train_, log(y.train_), 
                     alpha = 1, nfolds = 3, 
                     type.measure = "mse", 
                     parallel = T)
stopCluster(cl)

bestlam_ = cv.out_$lambda.min #best lambda
  
#prediction
lasso.pred.train_ = exp(predict(cv.out_, s = bestlam_, newx = x.train_))
lasso.pred.test_ = exp(predict(cv.out_, s = bestlam_, newx = x.test_))
  
submission = data.frame("id" = prediction$identifiant,
                        "prix" = lasso.pred.test_)

fwrite(submission, "C:/Best_Data_Scientist/submission/submission_lasso.csv", sep = ";")





##########################################################
#####                 Stacking                 ###########
##########################################################
stack.cv = data.table()
for (i in 1:K){
  train.cv_ = train[!cv.id == i, ]
  train.cv_ = train.cv_[abs(get(response.var)) < best.thres]
  test.cv_ = train[cv.id == i, ]
  stack.cv_ = subset(test.cv_, select= c(key))
  
  y.train_ = train.cv_[, get(response.var)]
  x.train_ = subset(train.cv_, select= -c(get(response.var), key, cv.id))
  x.train_ = as.matrix(x.train_)
  
  y.test_ = test.cv_[, get(response.var)]
  x.test_ = subset(test.cv_, select= -c(get(response.var), key, cv.id))
  x.test_ = as.matrix(x.test_)
  
  #Model
  cv.out_ = glmnet (x.train_, log(y.train_), 
                    alpha = 1, lambda = bestlam_)
  
  #prediction
  lasso.pred.train_ = exp(predict(cv.out_, s = bestlam_, newx = x.train_))
  lasso.pred.test_ = exp(predict(cv.out_, s = bestlam_, newx = x.test_))
  
  #stack prediction
  stack.cv_[, pred := lasso.pred.test_]
  stack.cv = rbind(stack.cv, stack.cv_)
  
  print(sprintf("round: %s, training_error : %s, testing error: %s", 
                i,
                round(mape_error(y.train_, lasso.pred.train_),2),
                round(mape_error(y.test_, lasso.pred.test_), 2)))
}
fwrite(stack.cv, "C:/Best_Data_Scientist/stacking/lasso_prediction.csv")

rm(train.cv_); rm(test.cv_); rm(x.train_); rm(x.test_)
rm(y.train_); rm(y.test_); rm(cv.out_); rm(lasso.pred.test_);
rm(lasso.pred.train_); rm(stack.cv_); rm(cl); rm(stack.cv); 
rm(i); rm(error_); rm(thres); rm(thres_);
rm(submission); gc()