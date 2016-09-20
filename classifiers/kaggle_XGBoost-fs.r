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
		cat("Dropped: ", i, "\n")
	}
}

train[lstDrp] = list(NULL)
test[lstDrp] = list(NULL)
#cat("Dropped lst: ", lstDrp, "\n")
cat("New Din Train: ", dim(train), "\n")
cat("New Dim Test: ", dim(test), "\n")

#feature.names <- names(train)[2:ncol(train)-1]
mrmr_feature_index = c(105,963,789,710,5,1415,919,879,497,1120,412,187,674,177,620,77,73,724,1540,1023,534,95,1093,226,154,498,59,1116,33,162,878,713,629,1569,145,1097,52,207,1849,618,505,300,962,109,1073,625,79,49,816,156,129,1557,160,1027,1251,499,603,36,1098,70,53,51,1004,1092,1565,101,965,75,399,61,1119,159,1024,125,1554,14,1,1382,164,209,68,1522,766,152,66,961,34,1380,716,97,630,87,1541,1762,179,1117,126,1638,37,1032,150,110,966,50,69,830,787,56,1014,799,141,615,228,166,67,668,35,16,613,1112,208,1040,60,1559,1856,94,80,172,832,127,279,1543,586,500,1118,76,201,938,78,169,144,1028,890,1363,71,174,1381,54,1257,1858,15,1626,62,852,133,1558,86,1037,948,128,230,1327,206,6,687,84,96,170,731,1542,1099,83,1791,628,176,1379,81,117,506,85,804,788,7,1847,134,951,1134,229,113,1219,1021,135,72,865,1100,57,1774,13,88,55,63,1025,1631,681,121,1376,1855,89,111,839,825,82,908,102,686,791,744,860,136,233,1358,58,896,1041,137,626,142,582,64,1695,1848,1145,112,1770,1033,118,1639,17,1359,227,777,835,65,794,838,1618,143,909,1870,939,585,120,1833,1851,1778,1029,882,1701,103,1384,1231,688,746,119,780,1793,104,796,1867,836,784,1630,1326,1372,875,1143,1850,700,857,654,1761,851,769,785,1789,689,578,786,683,1139,1357,1625,877,833,797,1345,759,1853,1815,756,1146,608,1732,837,1875,699,1823,552,1595,1331,802,1895,745,581,1619,1822,1132,728,885,1640,1854,1814,1869,606,1731,792,1342,1094,801,1616,1788,1872,888,1852,579,653,1777,1894,1332,1624,1769,1868,874,1773,1610,607,1343,1877,1611,682,1790,1893,881,1792,1878,1244,602,1368,1889,887,1344,1891,1881,1362,891,605,1866,1238,598,684,1237,1892,609,1241,1871,1370,889,1760,1885,1297,1874,900,600,1240,1876,590,1807,952,599,1295,893,1890,1797,577,953,610,1242,1883,1785,1873,591,1361,1806,1884,1300,1311,1880,1805,1221,1821,1886,1299,1804,1879,583,1365,1818,1887,1243,1819,936,1882,1776,1317,604,1888,1796,1312,1763,685,1245,1373,1772,576,1369,1765,1220,1768,1239,601,1784,1799,1350,1107,575,1766,1366,1787,574,1798,1367,1347,1782,1809,1294,1817,1764,1364,1268,680,1351,1783,1795,584,1816,1280,1808,589,1803,1346,1794,1272,1801,588,1802,1292,1800,1293,1349,1276,1236,1348,1234,1235,580,1316,1324,1306,1322,1323,1313,1320,1310,933,1314,587,1305,1315,1308,1307)

mrmr_feature_index = mrmr_feature_index + 1
feature.names <- names(train)[mrmr_feature_index]
cat("FEatures Used: ", feature.names, "\n")

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
#write.csv(train[, 2:ncol(train)], file = 'Processed_Train.csv')

#return(train)
#if (sum(!is.na(train)) ) { cat("NAN ", sum(!is.na(train)), "\n")}

cat(dim(train), "\n", dim(test), "\n")
#"
###############################################################
#Prediction on Training Data
if (FALSE) {
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
#cat("sampling train to get around 8GB memory limitations -3\n")
#train <- train[sample(nrow(train), 50000),]
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
write_csv(submission, "xgboost_submission-fs.csv")
#write_csv(svm_submission, "svm_submission.csv")
#"
