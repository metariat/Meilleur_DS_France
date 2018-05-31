setwd("C:/Best_Data_Scientist/stacking/")

lasso = fread("lasso_prediction.csv")
xgb = fread("xgboost_prediction.csv")
cat = fread("catboost_prediction.csv")
rf = fread("rf_prediction.csv")

setnames(lasso, "pred", "lasso.pred")
setnames(xgb, "pred", "xgb.pred")
setnames(cat, "pred", "cat.pred")
setnames(rf, "pred", "rf.pred")

data = merge(lasso, xgb, by = "key")
data = merge(data, cat, by ="key")
data = merge(data, rf, by = "key")

observed = subset(train, select = c("key", "prix"))

data = merge(observed, data, by = "key")
sum(is.na(data))

mod = rq(prix ~ 0 + lasso.pred + xgb.pred, data = data)


setwd("C:/Best_Data_Scientist/submission/")
lasso.pred = fread("submission_lasso.csv")
xgb.pred = fread("submission_xgb.csv")
cat.pred = fread("submission_cat.csv")

final.pred = (mod$coefficients[1] * lasso.pred$X1+ 
              mod$coefficients[2] * xgb.pred$prix) / 
             sum(mod$coefficients[1], mod$coefficients[2])

submission = data.frame("id" = xgb.pred$id,
                        "prix" = final.pred)


fwrite(submission, "stacking.csv", sep = ";")
