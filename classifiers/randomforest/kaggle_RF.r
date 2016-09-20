library(readr)
library(xgboost)
library(e1071)
library(AUC)
library(randomForest)

set.seed(321)

args = commandArgs(trailingOnly = TRUE)

cat("reading the train : ", args[1], args[2], args[3], "\n")

train_train <- read_csv(args[1])
train_test  <- read_csv(args[2])

cat("Train: ", dim(train_train), "\n", "Test: ", dim(train_test), "\n")

feature.names <- names(train_train)[2:ncol(train_train)]
#cat(feature.names)

###############################################################
#Prediction on Training Data
cat("training a XGBoost classifier on train-train\n")
clf_train_train <- xgboost(data = data.matrix(train_train[, feature.names]),
                           label       = train_train$target,
                           nrounds     = 20,
                           objective   = "binary:logistic",
                           eval_metric = "auc")



cat("training a RF classifier on train-train\n")
model_formula = as.formula(paste("target ~", paste(feature.names, collapse="+")))
#print(model_formula)
#print( as.factor(train_train$target)[1:nrow(train_train)])
#rf_train_train = randomForest(model_formula, data=data.frame(train_train), 
#                              tree = 20, do.trace = 1, y = (train_train$target))

#rf_train_train = randomForest(x = data.frame(train_train[, feature.names]), 
#                              y = as.factor(train_train$target)[1:nrow(train_train)],
#                              ntree = 30,
#                              do.trace=1)

rf_train_train = tuneRF(x = data.frame(train_train[, feature.names]), 
                        y = as.factor(train_train$target)[1:nrow(train_train)],
                        ntreeTry = 400, stepFactor=2, improve=0.05,
                        plot=TRUE, doBest=FALSE, trace=TRUE)


rm(train_train)
gc()
###############################################################
#Predict on Train-Test Phase
if (FALSE){
cat("Predict XGBoost on train-test\n")
train_test_predctions_xgb = data.frame(1:nrow(train_test))
#train_test_predctions_xgb$xgb = NA
train_test_predctions_rf = data.frame(1:nrow(train_test))
#train_test_predctions_rf$rf = NA
  
for (rows in split(1:nrow(train_test), ceiling((1:nrow(train_test))/10000)))
{
  train_test_predctions_xgb[rows, "xgb"] <- predict(clf_train_train, data.matrix(train_test[rows, feature.names]))
  train_test_predctions_rf[rows, "rf"] <- predict(rf_train_train, 
                                                      newdata =  data.matrix(train_test[rows, feature.names]),
                                                      type = "prob")
}

train_test_predctions_rf$rf = 1 - train_test_predctions_rf$rf

#print(train_test_predctions_xgb)
#cat("Prediction XGB AUC: \n")
#print (accuracy(train_test_predctions_xgb$xgb, train_test$target)) 
cat("Prediction XGB AUC: \n")
print(auc(roc(train_test_predctions_xgb$xgb[1:nrow(train_test)], as.factor(train_test$target)[1:nrow(train_test)])) ) 
cat("Prediction RF AUC: \n")
print(auc(roc(train_test_predctions_rf$rf[1:nrow(train_test)], as.factor(train_test$target)[1:nrow(train_test)]) ) ) 

gc()
}
###############################################################
####################################################################################################
if(FALSE){
#Prepare the fianl test
test  <- read_csv(args[3])
final_test_featurenames <- names(test)
final_test_pred_step_1_xgb = data.frame(1:nrow(test))
final_test_pred_step_1_xgb$xgb = NA
final_test_pred_step_1_rf = data.frame(1:nrow(test))
final_test_pred_step_1_rf$rf = NA
cat("done-1\n", dim(final_test_pred_step_1_rf), "\n")

for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000)))
{
    final_test_pred_step_1_xgb[rows, "xgb"] <- predict(clf_train_train, 
                                                       data.matrix(test[rows, ]))
    final_test_pred_step_1_rf[rows, "rf"] <- predict(rf_train_train, 
                                                       newdata= data.matrix(test[rows, ]),
                                                       type="prob")
}
#print(final_test_pred_step_1_rf)
#Write Data to file
write.csv(final_pred_step_2, file = 'Combined_Pred-test.csv', row.names = FALSE)
}
#RF 
#rf = randomForest(log(train$cost + 1)~., train[,-match(c("id", "cost"), names(train))], ntree = 20, do.trace = 2)

#SVM
####################################################################################
if(FALSE){
#SVM Learing
cat("Start SVM\n")
x_train = train[,feature.names]
y_train = train$target
svm_model = svm(x_train, y_train, scale = TRUE, type = 'C-classification', kernel = "radial", gamma =  1 / ncol(x_train),
                cost = 1, nu = 0.5, class.weights = NULL, cachesize = 1000, tolerance = 0.001, epsilon = 0.1,
                shrinking = TRUE, cross = 5, probability = FALSE, fitted = TRUE)

print(summary(svm_model))
svm_submission <- data.frame(ID=test$ID)
svm_submission$target = NA
}

