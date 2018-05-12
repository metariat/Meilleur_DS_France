training_ = training[prix < 50, ]

train_pool = catboost.load_pool(data = subset(training_, select = -c(prix)),
                                label = training_[, prix])
test_pool = catboost.load_pool(data = subset(testing, select = -c(prix)),
                               label = testing[, prix])


fit_params <- list(iterations = 5000,
                   thread_count = 10,
                   loss_function = 'MAPE',
                   border_count = 64,
                   depth = 10,
                   learning_rate = 0.2,
                   l2_leaf_reg = 3.5,
                   train_dir = 'train_dir')
model <- catboost.train(train_pool, NULL, fit_params)


prediction <- catboost.predict(model, 
                               test_pool, 
                               prediction_type = 'RawFormulaVal')

mape_error(testing$prix, prediction)
