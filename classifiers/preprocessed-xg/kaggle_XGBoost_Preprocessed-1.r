library(readr)
library(xgboost)
library(e1071)

set.seed(1)

cat("reading the train and test data\n")
#train <- read_csv("../../data/train.csv-1")
#test  <- read_csv("../../data/test.csv-1")
train <- read_csv("../../../data/train.csv")
test  <- read_csv("../../../data/test.csv")


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
lstDrp = c()
j = 1
for (i in 2:ncol(test))
{
  if (sum(as.double(train[, i])) == 0)
  {
    lstDrp[j] = i
    j = j + 1
    #train[, i] = NULL
    cat("Zero Dropped: ", i, "\n")
  }
}

train[lstDrp] = list(NULL)
test[lstDrp] = list(NULL)

feature.names <- names(train)[2:ncol(train)-1]

#Write Data to file
#write.csv(cbind(train$target, train[, 2:ncol(train)-1]), file = 'Processed_Train.csv', row.names=FALSE)


cat("New Din Train: ", dim(train), "\n")
cat("New Dim Test: ", dim(test), "\n")

###################################################################
#Final Prediction Phase
cat("sampling train to get around 8GB memory limitations -3\n")
train <- train[sample(nrow(train), 50000),]
gc()

cat("training a XGBoost classifier\n")
clf <- xgboost(data = data.matrix(train[,feature.names]),
			   label       = train$target,
			   nrounds     = 50,
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
write_csv(submission, "xgboost_preprocessed_submission.csv")
#write_csv(svm_submission, "svm_submission.csv")
#"
