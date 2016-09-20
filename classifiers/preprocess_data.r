library(readr)
library(xgboost)
library(e1071)

set.seed(1)

cat("reading the train and test data\n")
#train <- read_csv("../../data/train.csv-1")
#test  <- read_csv("../../data/test.csv-1")
train <- read_csv("../../data/train.csv")
test  <- read_csv("../../data/test.csv")

#Train ID - Features - Target
#Test ID - Features

#Drop Features with All NA
lstDrp = c()
j = 1
for (i in 2:ncol(test) )
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
#cat("Dropped lst: ", lstDrp, "\n")
cat("New Din Train: ", dim(train), "\n")
cat("New Dim Test: ", dim(test), "\n")


#Write Data to file
write.csv(cbind(train$target, train[, 3:ncol(train)-1]), file = 'Preprocess_Train.csv', row.names=FALSE)
write.csv(test[, 2:ncol(test)], file = 'Preprocess_Test.csv', row.names=FALSE)


#return(train)
#if (sum(!is.na(train)) ) { cat("NAN ", sum(!is.na(train)), "\n")}

cat(dim(train), "\n", dim(test), "\n")
