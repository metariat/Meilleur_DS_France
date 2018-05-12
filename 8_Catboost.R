y.train = training$prix
x.train = subset(training, select= -c(prix))

y.test = testing$prix
x.test = subset(testing, select= -c(prix))



# Enable caret to use MAE as eval metric
mapeSummary <- function (train,
                        lev = NULL,
                        model = NULL) {
  out <- mape_error(train$obs, train$pred)  
  names(out) <- "mape"
  out
}

control <- trainControl(method = "cv",
                        number = 2,
                        verboseIter = FALSE,
                        summaryFunction = mapeSummary)

grid <- expand.grid(depth = c(10),
                    learning_rate = c(0.2),
                    iterations = c(5000),
                    l2_leaf_reg = c(3.5),
                    rsm = c(0.6),
                    border_count = c(64))

cb <- train(y          = log(y.train),
            x          = data.frame(x.train), 
            preProcess = NULL,
            method     = catboost.caret, 
            metric     = "mape", 
            maximize   = FALSE, 
            tuneGrid   = grid, 
            trControl  = control)

#model information
print(cb)

cat.pred <- exp(predict(cb, data.frame(x.test)))
mape_error(y.test, cat.pred)
