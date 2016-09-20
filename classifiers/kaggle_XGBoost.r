library(readr)
library(xgboost)
library(e1071)

set.seed(1)

cat("reading the train and test data\n")
#train <- read_csv("../../data/train.csv-1")
#test  <- read_csv("../../data/test.csv-1")
train <- read_csv("../../data/train.csv")
test  <- read_csv("../../data/test.csv")


#Drop Features with All NA
lstDrp = c()
j = 1
for (i in 2:ncol(test))
{
	if (sum(is.na(train[, i])) == nrow(train))
	{
		lstDrp[j] = i
		j = j + 1
		#train[, i] = NULL
		cat("NA Dropped: ", i, "\n")
	}
  else if (sum(as.double(train[, i])) == 0)
  {
    lstDrp[j] = i
    j = j + 1
    #train[, i] = NULL
    cat("Zero Dropped: ", i, "\n")
  }
}

train[lstDrp] = list(NULL)
test[lstDrp] = list(NULL)
#cat("Dropped lst: ", lstDrp, "\n")
cat("New Din Train: ", dim(train), "\n")
cat("New Dim Test: ", dim(test), "\n")

feature.names <- names(train)[2:ncol(train)-1]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
	  if (class(train[[f]])=="character") {
		      levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
	    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
	  }
}

#cat("replacing missing values with -1\n")
#train[is.na(train)] <- -1
#test[is.na(test)]   <- -1

cat("replace missing values with mean \n")

#Impute Numerical Variables with Mean and Categorical with Mode
for (i in 2:ncol(test))
{
	#if(class(train[, i]) == "character")
	#{
	#	mode(train[, i])
	#}
	
	fMean = mean(train[!is.na(train[, i]), i])
	#cat("mean", i, ":", fMean, "\n")
	train[is.na(train[, i]), i] = fMean
	test[is.na(test[, i]), i] = fMean
	
	if(is.na(train[,i]) == TRUE)
	{
		cat("NA", i, " ", feature.names[i], "\n")
	}

}

#Write Data to file
write.csv(cbind(train$target, train[, 2:ncol(train)-1]), file = 'Processed_Train.csv', row.names=FALSE)


#return(train)
#if (sum(!is.na(train)) ) { cat("NAN ", sum(!is.na(train)), "\n")}

cat(dim(train), "\n", dim(test), "\n")
#"
###############################################################
#Prediction on Training Data
if (FALSE){
if (TRUE) {
gc()
cat("sampling train to get around 8GB memory limitations -1\n")
train_sample_row = sample(nrow(train), 45000)
#left_train_samples = train[-c(train_sample_row), ] 
train_1 <- train[train_sample_row, ]
gc()

#Do train on the 1st sample
cat("training a XGBoost classifier 1 \n")
clf1 <- xgboost(data = data.matrix(train_1[,feature.names]),
               label       = train_1$target,
               nrounds     = 25,
               objective   = "binary:logistic",
               eval_metric = "auc")

rm(train_1)
gc()

# Predict Left over training samples
left_train_samples = train[-c(train_sample_row), ] 
train_pred1 = data.frame(ID=left_train_samples$ID)
for (rows in split(1:nrow(left_train_samples), ceiling((1:nrow(left_train_samples))/10000))) {
  
  train_pred1[rows, "target"] <- predict(clf1, data.matrix(left_train_samples[rows,feature.names]))
}
rm(clf1)
#********************************************************************
#Do train on new sample and predict 1st sample
gc()
cat("sampling train to get around 8GB memory limitations -2\n")
#cat(nrow(left_train_samples), "\n")
train_sample_new_row = sample(nrow(left_train_samples), 45000)
train_2 <- left_train_samples[train_sample_new_row, ]
rm(left_train_samples)
gc()
test_1 = train[train_sample_row, ]

#Do train on the 1st sample
cat("training a XGBoost classifier 2 \n")
clf2 <- xgboost(data = data.matrix(train_2[,feature.names]),
                label       = train_2$target,
                nrounds     = 25,
                objective   = "binary:logistic",
                eval_metric = "auc")
rm(train_2)
gc()
# Predict Left over training samples
train_pred2 = data.frame(ID=test_1$ID)
for (rows in split(1:nrow(test_1), ceiling((1:nrow(test_1))/10000))) {
  
  train_pred2[rows, "target"] <- predict(clf2, data.matrix(test_1[rows,feature.names]))
  
}
#Write Training Predictions to File
write_csv(rbind(train_pred1, train_pred2), "pred_training_logistic.csv")
rm(train_pred1, train_pred2, test_1, clf2)
gc()
}

###################################################################
#Final Prediction Phase
cat("sampling train to get around 8GB memory limitations -3\n")
train <- train[sample(nrow(train), 50000),]
gc()

cat("training a XGBoost classifier\n")
clf <- xgboost(data = data.matrix(train[,feature.names]),
			   label       = train$target,
			   nrounds     = 20,
			   objective   = "binary:logistic",
			   eval_metric = "auc")
#			   missing	   = NAN )

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

cat("making predictions in batches due to 8GB memory limitation\n")
submission <- data.frame(ID=test$ID)
submission$target <- NA

for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
  
	    submission[rows, "target"] <- predict(clf, data.matrix(test[rows,feature.names]))
	    
	    #SVM Prediction
	    #svm_submission[rows, "target"] <- predict(svm_model, data.matrix(test[rows,feature.names]))
}

cat("saving the submission file\n")
write_csv(submission, "xgboost_submission.csv")
#write_csv(svm_submission, "svm_submission.csv")
#"
}
