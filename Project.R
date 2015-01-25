library(caret)
train <- read.csv("pml-training.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv("pml-testing.csv", header = TRUE, stringsAsFactors = TRUE)
nsv <- nearZeroVar(train, saveMetrics = TRUE)
train2 <- train[,names(train)[!nsv$nzv]] # Remove near zero variables

# Noticing that many values are NA, remove columns with more than 90% NA
train2 <- train2[,colSums(is.na(train2))<0.9*nrow(train2)]
train2 <- train2[,6:59] # Remove the first columns of X, names, timestamp etc
test2 <- test[,names(train2)[1:53]] # Keep the same columns as train except 'classe'


intrain2 <- createDataPartition(y = train2$classe, p = 0.75, list = FALSE)
mtrain <- train2[intrain2,] # Model train set
mtest <- train2[-intrain2,] # Model test set

myControl <- trainControl(method = "oob", number = 3)
fit <- train(classe ~ ., data = mtrain, method ="rf", trControl = myControl)

pred1 <- predict(fit, mtest)
confusionMatrix(pred1,mtest$classe)
sum(pred1 == train2$classe)/nrow(train2)
pred2  <- predict(fit, test2)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}