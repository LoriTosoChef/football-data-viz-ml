gc()

library('caret')
library('e1071')
library('xgboost')
library('data.table')

set.seed(seed = 42)

setDT(dataset_player_overall)

# Train Test Split 75-25
test_index <- createDataPartition(dataset_player_overall$target, p=0.75, list=FALSE)
test <- dataset_player_overall[-test_index,]
training <- dataset_player_overall[test_index,]

# Use 5 folds in standard cross validation
k <- 5
control <- trainControl(method = "cv", number = k)
metric <- "Accuracy"

# SVM Radial
svm1 <- Sys.time()
fit.svm_radial <- train(target ~ ., data = training, 
                 method = "svmRadial", metric = metric, trControl = control)
svm2 <- Sys.time()

svm2 - svm1
rm(svm2, svm1)

# SVM Polynomial
svm1 <- Sys.time()
fit.svm_poly <- train(target ~ ., data = training, 
                        method = "svmPoly", metric = metric, trControl = control)
svm2 <- Sys.time()

svm2 - svm1
rm(svm2, svm1)

# XGB with default parameters
xgb1 <- Sys.time()
fit.xgb <- train(target ~ ., data = training, 
                 method = "gbm", distribution = 'multinomial', shrinkage = 0.1, n.trees = 1000, 
                 metric = metric, trControl = control)
xgb2 <- Sys.time()
xgb2 - xgb1
rm(xgb2, xgb1)

# Random Forest
rf1 <- Sys.time()
fit.rf <- train(target ~ ., data = training, 
                method = "rf", metric = metric, trControl = control)
rf2 <- Sys.time()
rf2 - rf1
rm(rf2, rf1)