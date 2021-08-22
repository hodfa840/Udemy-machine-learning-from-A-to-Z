digits <- read.csv("optdigits.csv", header = F)
colnames(digits)[ncol(digits)] <- "number"
digits$number <- as.factor(digits$number)
n <- dim(digits)[1] 
set.seed(12345)
id <- sample(1:n, floor(n*0.5)) 
train <- digits[id,]
id1 <- setdiff(1:n, id) 
set.seed(12345) 
id2 <- sample(id1, floor(n*0.25)) 
validation <- digits[id2,]
id3 <- setdiff(id1,id2) 
test <- digits[id3,]

head(digits)


#install.packages('kknn')
library(kknn)

knn_train <- kknn(number ~., train, train, k = 30, kernel = "rectangular")
knn_test <- kknn(number ~., train, test, k = 30, kernel = "rectangular")
pred_train<- fitted(knn_train)
confusion_train <- table(train$number, pred_train)
pred_test <- fitted(knn_test)
confusion_test <- table(test$number, pred_test)

print(confusion_train)


print(confusion_test)


print(diag(confusion_train)/rowSums(confusion_train))
print(diag(confusion_test)/rowSums(confusion_test))

missclass <- function(X,X1) { 
  n = length(X)
  return(1-sum(diag(table(X,X1)))/n)
}
mismatch_rate_train <- missclass(train$number, pred_train)
mismatch_rate_test <- missclass(test$number, pred_test)
cat("The mismatch rate for train data is:", mismatch_rate_train, 
    "\nThe mismatch rate for test data is:", mismatch_rate_test)

train_eight <- train[which(train$number==8),]
train_eight$prob <- knn_train$prob[which(train$number==8), "8"]
sortedResult <- sort(train_eight[, "prob"], index.return = T)
#sortedResult$x[1:3] #0.1000000 0.1333333 0.1666667
hard_ids <- sortedResult$ix[1:3] #50  43 136
#tail(sortedResult$x, n=c(2)) #1 1
easy_ids <- tail(sortedResult$ix, n=c(2)) #179 183
plotHeatmap <- function(index){
  heatmap(matrix(as.numeric(train_eight[index, 1:64]), 8, 8, byrow = T), Colv = NA, Rowv = NA)
}

 for (i in c(hard_ids)) {
   plotHeatmap(i)
 }

for (i in c( easy_ids)) {
  plotHeatmap(i)
}

e_t <- 0
e_v <- 0
for (k in 1:30) {
  kt <- kknn(number ~., train, train, k = k, kernel = "rectangular")
  e_t[k] <- missclass(train$number, kt$fitted.values)
  kv <- kknn(number ~., train, validation, k = k, kernel = "rectangular")
  e_v[k] <- missclass(validation$number, kv$fitted.values)
}
plot(1:30, e_v, type = "l", col = "red", xlab = "K", ylab = "miss-classification error rate")
lines(1:30, e_t, col = "blue")
#legend(2, 0.06, legend=c("validation", "train"),
      # col=c("red", "blue"), lty=1:1, cex=0.8)

k_test_optik <- kknn(number ~., train, test, k = 3, kernel = "rectangular")
cat("Test mis-classification rate when k=3 is:", missclass(test$number, fitted(k_test_optik)))
cat("Train mis-classification rate when k=3 is:", e_t[3])
cat("Validation mis-classification rate when k=3 is:", e_v[3])

max(e_v)

#install.packages("reshape")
#install.packages("data.table", type="source", dependencies=TRUE)
library(data.table)
library(reshape)
cross_entropy_train <- c()
cross_entropy_val <- c()
num_classes <- length(unique(train$number))
num_train_examples <- nrow(train)
num_val_examples <- nrow(validation)
one_hot_y_train <- t(sapply(as.numeric(train$number), 
                            function(x) {c(rep(0, x - 1), 1, rep(0, num_classes - x))}))
one_hot_y_val <- t(sapply(as.numeric(validation$number), 
                          function(x) {c(rep(0, x - 1), 1, rep(0, num_classes - x))}))

for (k_choice in 1:30) {
  
  # fit model
  train_model <- kknn(formula = number ~., kernel = "rectangular", train = train, test = train, k = k_choice)
  val_model <- kknn(formula = number ~., kernel = "rectangular", train = train, test = validation, k = k_choice)
  
  
  
  # cross-entropy
  cross_entropy_train <- c(cross_entropy_train, sum(one_hot_y_train * -log(train_model$prob + 10^-15))/num_train_examples)
  cross_entropy_val <- c(cross_entropy_val, sum(one_hot_y_val * -log(val_model$prob + 10^-15))/num_val_examples)
  
}


cross_entropy <- melt(data.table(k = 1:30, Training = cross_entropy_train, Validation = cross_entropy_val),
                      "k", variable.name = "Legend")

plot(1:30, cross_entropy_val, type = "l", xlab = "K", ylab = "cross entropy",col='blue',lty=1)
lines(1:30, cross_entropy_train, type = "l", xlab = "K", ylab = "cross entropy",col='red',lty=2)
legend(x='topright', legend=c("valid" , 'train'), col=c('blue','red'),lty=1:2)






