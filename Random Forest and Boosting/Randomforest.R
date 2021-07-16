library(randomForest)

# dataset <- list(x=0,y=0) #creating a list of list

set.seed(12345)

#creating training data
dataset=list(list())
for (i in 1:1000) {
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y<-as.numeric(x1<x2)
  #y = ifelse(x1<x2,yes = "1",no = "0")
  trlabels<-as.factor(y)
  dataset[[i]] =list(x=trdata,y=trlabels) 
}

#creating test set

set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
telabels<-as.factor(y)
plot(x1,x2,col=c('red',"blue")[telabels],pch=19)

#Random Forest Models
#creating a nested list to save the result for the 1000s datasets when ntree =  c(1,10,100)

tree = c(1,10,100)
 
forest = list(list(list()))

set.seed(12345)
for (i in 1:1000) {
  
  forest[[i]] = list() #bcz I didn't specify the length at the beginning, first I create a list for it!
  for (n in 1:3) {
    
    forest[[i]][[n]] = randomForest(y=dataset[[i]]$y,
                                    x=dataset[[i]]$x,
                                    ntree = tree[n],
                                    nodesize = 25,
                                    keep.forest = TRUE,
                                    xtest = tedata,
                                    ytest = telabels)
  }
}
error.1.tree = numeric(1000)
error.10.tree = numeric(1000)
error.100.tree = numeric(1000)
#calculating errors
for (i in 1:1000) {
  
  error.1.tree[i] = forest[[i]][[1]]$test$err.rate[1]
   error.10.tree[i] = forest[[i]][[2]]$test$err.rate[10] #the 10th element is the error rate for ntree=10
   error.100.tree[i] = forest[[i]][[3]]$test$err.rate[100]   #the 100th element is the error rate for ntree=100 
}



mean.error.1 = mean(error.1.tree)

mean.error.10 = mean(error.10.tree)

mean.error.100 = mean(error.100.tree)
print(c(mean.error.1,mean.error.10,mean.error.100))

c(var(error.1.tree),var(error.10.tree),var(error.100.tree))
#exercise 1.b

# Generate 1000 data sets
dataset <- list(list(x=0,
                      y=0))
set.seed(12345)
for (set in 1:1000) {
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y<-as.numeric(x1<0.5)
  trlabels<-as.factor(y)
  
  dataset[[set]] <- list(x=trdata,
                          y=trlabels)
}


# Test data
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<0.5)
telabels<-as.factor(y)
plot(x1,x2,col=c('red',"blue")[telabels],pch=19)


trees <- c(1,10,100)
forests_two <- list(list(list()))
set.seed(12345)
for (set in 1:1000) {
  forests_two[[set]] <- list()
  for (ntrees in 1:3) {
    forests_two[[set]][[ntrees]] <- randomForest(x = dataset[[set]]$x, 
                                                 y = dataset[[set]]$y,
                                                 xtest = tedata,
                                                 ytest = telabels, 
                                                 ntree = trees[ntrees],
                                                 nodesize = 25,
                                                 keep.forest = TRUE)
  }
}


error2.1.tree = numeric(1000)
error2.10.tree = numeric(1000)
error2.100.tree = numeric(1000)
for (i in 1:1000) {
  
  error2.1.tree[i] = forests_two[[i]][[1]]$test$err.rate[1]
  error2.10.tree[i] = forests_two[[i]][[2]]$test$err.rate[10] #the 10th element is the error rate for ntree=10
  error2.100.tree[i] = forests_two[[i]][[3]]$test$err.rate[100]   #the 100th element is the error rate for ntree=100 
}

mean.error2.1 = mean(error2.1.tree)

mean.error2.10 = mean(error2.10.tree)

mean.error2.100 = mean(error2.100.tree)
print(c(mean.error2.1,mean.error2.10,mean.error2.100))
c(var(error2.1.tree),var(error2.10.tree),var(error2.100.tree))


#exercise 1c

### 1.3 - Random forest for y = x1<05 & x2<0.5 | x1>0.5 & x2>0.5

# Once again we repeat the exercise, this time with Y computed based on x1<05 & x2<0.5 or x1>0.5 & x2>0.5. The data is set up with the following code:

# Generate 1000 data sets
datasets <- list(list(x=0,
                      y=0))
set.seed(12345)
for (set in 1:1000) {
  x1<-runif(100)
  x2<-runif(100)
  trdata<-cbind(x1,x2)
  y<-as.numeric((x1<0.5 & x2<0.5)|(x1>0.5 & x2>0.5))
  trlabels<-as.factor(y)
  
  datasets[[set]] <- list(x=trdata,
                          y=trlabels)
}

set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric((x1<0.5 & x2<0.5)|(x1>0.5 & x2>0.5))
telabels<-as.factor(y)
plot(x1,x2,col=c('red','blue')[telabels])

#The random forests are then learned with the following code:

trees <- c(1,10,100)
forests_three <- list(list(list()))
set.seed(12345)
for (set in 1:1000) {
  forests_three[[set]] <- list()
  for (ntrees in 1:3) {
    forests_three[[set]][[ntrees]] <- randomForest(x = datasets[[set]]$x, 
                                                   y = datasets[[set]]$y,
                                                   xtest = tedata,
                                                   ytest = telabels, 
                                                   ntree = trees[ntrees],
                                                   nodesize = 12,
                                                   keep.forest = TRUE)
  }
}



















