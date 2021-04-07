setwd(".../Tugas 1 - Klasifikasi")

#---Klasifikasi non-Numeric----
#--PRAPROSES--
library(readr)
data_mix <- read_delim("Tugas Klp #1 Data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#View(data)
#sapply(data,class)

library(dplyr)
#cek duplikat data
which(duplicated(data_mix))

#cek missing value
sapply(data_mix, function(x) sum(is.na(x)))

#cek tipe data
sapply(data_mix,class)

#ubah nama kolom y jadi class & tipe data jadi faktor
names(data_mix)[names(data_mix) == "y"] <- "class"
data_mix$class <- as.factor(data_mix$class)
sapply(data_mix,class)

#splitting data
set.seed(1234)

sample_size <- floor(0.67*nrow(data_mix))
train_index <- sample(seq_len(nrow(data_mix)), size=sample_size)
train_data <- data_mix[train_index,]
test_data <- data_mix [-train_index,]

#==Decision Tree==
library(rminer)
library(rpart)
library(rpart.plot)
library(caret)

#split dengan GINI
dtreemodel_rpart <- rpart(class~., train_data, parms=list(split = "gini"))
predict_dtreepart <- predict(dtreemodel_rpart, test_data[,1:16], type="class")
confusionMatrix(predict_dtreepart, test_data$class)

rpart.plot(dtreemodel_rpart)

#split dengan INFO GAIN
dtreemodel_rpart <- rpart(class~., train_data, parms=list(split = "information"))
predict_dtreepart <- predict(dtreemodel_rpart, test_data[,1:16], type="class")
confusionMatrix(predict_dtreepart, test_data$class)

rpart.plot(dtreemodel_rpart)

#==Naive Bayes==
library(e1071)
nbmodel <- naiveBayes(class~., data=train_data)
nbpredict <- predict(nbmodel, test_data[,1:16], type="class")

confusionMatrix(nbpredict,test_data$class)

library(caret)
library(klaR)
trainControl <- trainControl(method="cv", number=5)
fit.nb <- train(class~., data=train_data, method="nb",
                metric="Accuracy", trControl=trainControl)

predict.nb <- predict(fit.nb, test_data[,1:16])

confusionMatrix(predict.nb, test_data$class)

#---Klasifikasi Numeric----
#--PRAPROSES--
datanumonly <- data_mix
names(datanumonly)[names(datanumonly) == "y"] <- "class"
datanumonly$class <- as.factor(datanumonly$class)

#-ubah ke numerik-

#$age > dibagi 3
summary(datanumonly$age)
datanumonly$age <- cut(datanumonly$age,
                        breaks = c(-Inf, 33, 48, Inf),
                        labels = c("young", "adult", "senior"))
datanumonly$age <- as.numeric(datanumonly$age)

datanumonly$job <- factor(datanumonly$job,
                           levels = c("unemployed", "unknown", "admin.", "management",
                                      "housemaid", "entrepreneur", "student", "blue-collar",
                                      "self-employed", "retired", "technician", "services"))
datanumonly$job <- as.numeric(datanumonly$job)

#$marital
datanumonly$marital <- ifelse(datanumonly$marital == "married",1,0)

#$education 
datanumonly$education <- factor(datanumonly$education, ordered = TRUE,
                                 levels = c("unknown", "primary", "secondary", "tertiary"),
                                 labels = c("0", "1", "2", "3"))
datanumonly$education <- as.numeric(datanumonly$education)

#$default credit
datanumonly$default <- ifelse(datanumonly$default == "yes",1,0)

#$balance > dibagi 3
summary(datanumonly$balance)
datanumonly$balance <- cut(datanumonly$balance,
                            breaks = c(-Inf, 72, 1428, Inf),
                            labels = c("low", "med", "high"))
datanumonly$balance <- as.numeric(datanumonly$balance)

#$housing loan
datanumonly$housing <- ifelse(datanumonly$housing == "yes",1,0)

#$personal loan
datanumonly$loan <- ifelse(datanumonly$loan == "yes",1,0)

#mengubah kolom kontak
datanumonly$contact <- factor(datanumonly$contact,
                               levels = c("unknown", "telephone", "cellular"),
                               labels = c("0", "1", "2"))
datanumonly$contact <- as.numeric(datanumonly$contact)

#$month > factor > numeric
datanumonly$month <- factor(datanumonly$month, 
                             levels = c("jan", "feb", "mar", "apr", 
                                        "may", "jun", "jul", "aug", 
                                        "sep", "oct", "nov", "dec"))
datanumonly$month <- as.numeric(datanumonly$month)

#$duration > dibagi 3
summary(datanumonly$duration)
datanumonly$duration <- cut(datanumonly$duration,
                             breaks = c(-Inf, 103, 319, Inf),
                             labels = c("short", "med", "long"))
datanumonly$duration <- as.numeric(datanumonly$duration)

#$poutcome
datanumonly$poutcome <- factor(datanumonly$poutcome, 
                                levels = c("unknown", "failure", "other", "success"),
                                labels = c("0", "1", "2", "3"))
datanumonly$poutcome <- as.numeric(datanumonly$poutcome)

#spliting datanumonly
set.seed(1234)

sample_size <- floor(0.67*nrow(datanumonly))
train_index <- sample(seq_len(nrow(datanumonly)), size=sample_size)
train_datanumonly <- datanumonly[train_index,]
test_datanumonly <- datanumonly [-train_index,]

#==KNN==
library(caret)

#using knn3
knnModel <- knn3(class~., data = train_datanumonly, k = 5)

knnPredict <- predict(knnModel, test_datanumonly[,1:16], type="class")

table(test_datanumonly$class, knnPredict)
confusionMatrix(knnPredict, test_datanumonly$class)
  
  
#==Neural Network==
#w/ nnet
library(nnet)
nnmodel <- nnet(class~., data = train_datanumonly, size = 10, maxit = 300,
            trace = FALSE)

predict_nnmodel <- predict(nnmodel, newdata = test_datanumonly[,1:16], type = 'class')
pred1 <- factor(predict_nnmodel)

confusionMatrix(pred1, test_datanumonly$class)

#w/ neuralnet

#data_test_2 <- test_data %>% mutate(class = ifelse(class == "benign", 0, 1))
library(neuralnet)
NNModel <- neuralnet(class~., data = train_datanumonly, linear.output = FALSE,
                hidden = 5, rep = 10, threshold = 0.1)
#rep=fungsinya buat loopingnya, spt max iteration
plot(NNModel, rep = "best")

predict_nn <- neuralnet::compute(NNModel, test_datanumonly[,1:16])
nn_result1 <- data.frame(actual = test_datanumonly$class,
                         prediction = predict_nn$net.result)

rounded_result <- sapply(nn_result1, round, digits = 0)
rounded_resultdf <- data.frame(rounded_result)
attach(rounded_resultdf)

table(actual, prediction.2)

actualfactor <- as.factor(actual)
predfactor <- as.factor(prediction.2)

confusionMatrix(actualfactor, predfactor)

#---Evaluasi Model----
#==Repeated Holdout==
#Decision Tree
library(rminer)
full_accuracy = 0 #menampung total akurasi dari seluruh looping
list_accuracy <- list() #menyimpan daftar akurasi
full_precision = 0 
list_precision <- list()
full_recall = 0 
list_recall <- list()
full_fmeasure = 0
list_fmeasure <- list()

for(i in 1:100) {
  H = holdout(data_mix$class, ratio = 2/3, mode="random", seed = NULL)
  rpart_model <- rpart(class~., data = data_mix[H$tr,])
  rpartpredict <- predict(rpart_model, data_mix[H$ts, 1:16], type="class")
  result <- confusionMatrix(rpartpredict, data_mix[H$ts,]$class)
  
  accuracy <- result$overall['Accuracy']
  precision <- result$byClass['Precision']
  recall <- result$byClass['Recall']
  fmeasure <- result$byClass['F1']
  
  cat("batch: ",i,
      "accuracy: ",accuracy,"\n")
  full_accuracy = full_accuracy + accuracy
  list_accuracy[[i]] <- accuracy
  
  cat("batch: ",i,
      "precision: ",precision,"\n")
  full_precision = full_precision + precision
  list_precision[[i]] <- precision
  
  cat("batch: ",i,
      "recall: ",recall,"\n")
  full_recall = full_recall + recall
  list_recall[[i]] <- recall
  
  cat("batch: ",i,
      "fmeasure: ",precision,"\n")
  full_fmeasure = full_fmeasure + fmeasure
  list_fmeasure[[i]] <- fmeasure
}
cat("Tree: ",
    "Accuracy: ", full_accuracy/100, "\n")
holdout <- c(1:100)
plot(holdout, list_accuracy, type="o")

cat("Tree: ",
    "Precision: ", full_precision/100, "\n")
holdout <- c(1:100)
plot(holdout, list_precision, type="o")

cat("Tree: ",
    "Recall: ", full_recall/100, "\n")
holdout <- c(1:100)
plot(holdout, list_recall, type="o")

cat("Tree: ",
    "Fmeasure: ", full_fmeasure/100, "\n")
holdout <- c(1:100)
plot(holdout, list_fmeasure, type="o")

#Naive Bayes
library(e1071)
library(caret)
library(klaR)
full_accuracy = 0 #menampung total akurasi dari seluruh looping
list_accuracy <- list() #menyimpan daftar akurasi
full_precision = 0 
list_precision <- list()
full_recall = 0 
list_recall <- list()
full_fmeasure = 0
list_fmeasure <- list()

for(i in 1:100) {
  H = holdout(data_mix$class, ratio = 2/3, mode="random", seed = NULL)
  nbmodel <- naiveBayes(class~., data = data_mix[H$tr,])
  nbpredict <- predict(nbmodel, data_mix[H$ts, 1:16], type="class")
  result <- confusionMatrix(nbpredict, data_mix[H$ts,]$class)
  
  accuracy <- result$overall['Accuracy']
  precision <- result$byClass['Precision']
  recall <- result$byClass['Recall']
  fmeasure <- result$byClass['F1']
  
  cat("batch: ",i,
      "accuracy: ",accuracy,"\n")
  full_accuracy = full_accuracy + accuracy
  list_accuracy[[i]] <- accuracy
  
  cat("batch: ",i,
      "precision: ",precision,"\n")
  full_precision = full_precision + precision
  list_precision[[i]] <- precision
  
  cat("batch: ",i,
      "recall: ",recall,"\n")
  full_recall = full_recall + recall
  list_recall[[i]] <- recall
  
  cat("batch: ",i,
      "fmeasure: ",precision,"\n")
  full_fmeasure = full_fmeasure + fmeasure
  list_fmeasure[[i]] <- fmeasure
}
cat("Naive_bayes: ",
    "Accuracy: ", full_accuracy/100, "\n")
holdout <- c(1:100)
plot(x, list_accuracy, type="o")

cat("Naive_bayes: ",
    "Precision: ", full_precision/100, "\n")
holdout <- c(1:100)
plot(x, list_precision, type="o")

cat("Naive_bayes: ",
    "Recall: ", full_recall/100, "\n")
holdout <- c(1:100)
plot(x, list_recall, type="o")

cat("Naive_bayes: ",
    "Fmeasure: ", full_fmeasure/100, "\n")
holdout <- c(1:100)
plot(x, list_fmeasure, type="o")

#Repeated holdout - KNN
library(rminer)
library(caret)
full_accuracy = 0 #menampung total akurasi dari seluruh looping
list_accuracy <- list() #menyimpan daftar akurasi
full_precision = 0 
list_precision <- list()
full_recall = 0 
list_recall <- list()
full_fmeasure = 0
list_fmeasure <- list()

for(i in 1:100) {
  H = holdout(datanumonly$class, ratio = 2/3, mode="random", seed = NULL)
  
  knnModel_num <- knn3(class~., data = datanumonly [H$tr,], k = 5)
  knnPredict_num <- predict(knnModel_num, newdata=datanumonly[H$ts,1:16], type="class")
  result <- confusionMatrix(table(knnPredict_num, datanumonly[H$ts,]$class))
  
  accuracy <- result$overall['Accuracy']
  precision <- result$byClass['Precision']
  recall <- result$byClass['Recall']
  fmeasure <- result$byClass['F1']
  
  cat("batch: ",i,
      "accuracy: ",accuracy,"\n")
  full_accuracy = full_accuracy + accuracy
  list_accuracy[[i]] <- accuracy
  
  cat("batch: ",i,
      "precision: ",precision,"\n")
  full_precision = full_precision + precision
  list_precision[[i]] <- precision
  
  cat("batch: ",i,
      "recall: ",recall,"\n")
  full_recall = full_recall + recall
  list_recall[[i]] <- recall
  
  cat("batch: ",i,
      "fmeasure: ",precision,"\n")
  full_fmeasure = full_fmeasure + fmeasure
  list_fmeasure[[i]] <- fmeasure
}
cat("KNN: ",
    "Accuracy: ", full_accuracy/100, "\n")
holdout <- c(1:100)
plot(holdout, list_accuracy, type="o")

cat("KNN: ",
    "Precision: ", full_precision/100, "\n")
holdout <- c(1:100)
plot(holdout, list_precision, type="o")

cat("KNN: ",
    "Recall: ", full_recall/100, "\n")
holdout <- c(1:100)
plot(holdout, list_recall, type="o")

cat("KNN: ",
    "Fmeasure: ", full_fmeasure/100, "\n")
holdout <- c(1:100)
plot(holdout, list_fmeasure, type="o")

#Repeated holdout - Neural Network
library(nnet)
full_accuracy = 0 #menampung total akurasi dari seluruh looping
list_accuracy <- list() #menyimpan daftar akurasi
full_precision = 0 
list_precision <- list()
full_recall = 0 
list_recall <- list()
full_fmeasure = 0
list_fmeasure <- list()

for(i in 1:100) {
  H = holdout(datanumonly$class, ratio = 2/3, mode="random", seed = NULL)
  
  nn1 <- nnet(class~., data = datanumonly, size = 10, maxit = 300,
              trace = FALSE)
  predict_nn1 <- predict(nn1, newdata = datanumonly[H$ts,1:16], type = 'class')
  result <- confusionMatrix(table(predict_nn1, datanumonly[H$ts,]$class))
  
  accuracy <- result$overall['Accuracy']
  precision <- result$byClass['Precision']
  recall <- result$byClass['Recall']
  fmeasure <- result$byClass['F1']
  
  cat("batch: ",i,
      "accuracy: ",accuracy,"\n")
  full_accuracy = full_accuracy + accuracy
  list_accuracy[[i]] <- accuracy
  
  cat("batch: ",i,
      "precision: ",precision,"\n")
  full_precision = full_precision + precision
  list_precision[[i]] <- precision
  
  cat("batch: ",i,
      "recall: ",recall,"\n")
  full_recall = full_recall + recall
  list_recall[[i]] <- recall
  
  cat("batch: ",i,
      "fmeasure: ",fmeasure,"\n")
  full_fmeasure = full_fmeasure + fmeasure
  list_fmeasure[[i]] <- fmeasure
}

cat("nnet: ",
    "Accuracy: ", full_accuracy/100, "\n")
holdout <- c(1:100)
plot(holdout, list_accuracy, type="o")

cat("nnet: ",
    "Precision: ", full_precision/100, "\n")
holdout <- c(1:100)
plot(holdout, list_precision, type="o")

cat("nnet: ",
    "Recall: ", full_recall/100, "\n")
holdout <- c(1:100)
plot(holdout, list_recall, type="o")

cat("nnet: ",
    "Fmeasure: ", full_fmeasure/100, "\n")
holdout <- c(1:100)
plot(holdout, list_fmeasure, type="o")


#==Cross Validation==
#Decision Tree
folds <- cut(seq(1,nrow(data_mix)), breaks=10, label=FALSE)
full_accuracy_cv = 0
list_accuracy_cv <- list()
full_precision_cv = 0
list_precision_cv <- list()
full_recall_cv = 0
list_recall_cv <- list()
full_fmeasure_cv = 0
list_fmeasure_cv <- list()

for(i in 1:10) {
  testIndex <- which(folds == i, arr.ind = TRUE)
  testData <- data_mix[testIndex, ]
  trainData <- data_mix[-testIndex, ]
  
  rpart_model <- rpart(class~., data = trainData)
  rpartpredict <- predict(rpart_model, testData[,1:16], type="class")
  result <- confusionMatrix(rpartpredict, testData$class)
  
  accuracy_cv <- result$overall['Accuracy']
  precision_cv <- result$byClass['Precision']
  recall_cv <- result$byClass['Recall']
  fmeasure_cv <- result$byClass['F1']
  
  cat("batch: ",i,
      "accuracy: ",accuracy_cv,"\n")
  full_accuracy_cv = full_accuracy_cv + accuracy_cv
  list_accuracy_cv[[i]] <- accuracy_cv
  
  cat("batch: ",i,
      "precision: ",precision_cv,"\n")
  full_precision_cv = full_precision_cv + precision
  list_precision_cv[[i]] <- precision_cv
  
  cat("batch: ",i,
      "recall: ",recall_cv,"\n")
  full_recall_cv = full_recall_cv + recall_cv
  list_recall_cv[[i]] <- recall_cv
  
  cat("batch: ",i,
      "fmeasure: ",fmeasure_cv,"\n")
  full_fmeasure_cv = full_fmeasure_cv + fmeasure_cv
  list_fmeasure[[i]] <- fmeasure_cv
}

cat("Tree: ",
    "Accuracy: ", full_accuracy_cv/10,"\n")
x <- c(1:10)
plot(x, list_accuracy_cv, type="o")

cat("nnet: ",
    "Precision: ", full_precision_cv/100, "\n")
holdout <- c(1:100)
plot(holdout, list_precision_cv, type="o")

cat("nnet: ",
    "Recall: ", full_recall_cv/100, "\n")
holdout <- c(1:100)
plot(holdout, list_recall_cv, type="o")

cat("nnet: ",
    "Fmeasure: ", full_fmeasure_cv/100, "\n")
holdout <- c(1:100)
plot(holdout, list_fmeasure_cv, type="o")

#KNN



#---Perbandingan Kinerja---
