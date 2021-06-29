set.seed(12345)
n = 400

gen.mix <- function(n,mu,k,sig){
  library(MASS)
  
  d = length(mu[1,]) # dim of the data points
  
  results = array(NA,c(n,d))
  for(i in 1:n){
    
    results[i,] = mvrnorm(n = 1,mu = mu[k[i],],Sigma = sig[, ,k[i]])
  }
  return(results)
}
# number of component is 3
pi  <- c(.3,.5,.2)   # mixing coeffs

# the data is in 2-D
# we start with 3 vector of size for mu

mu = matrix(NA, ncol = 2,nrow = 3)
mu = matrix(c(17,8
               ,18,9.5,
               19,7.7),ncol = 2,byrow = TRUE)
 
# covariance matrix is sigma :: a 3 matrix of size 2*2 (one for each class)

# create an empty array for sigma

sigs = array(rep(NA),c(2,2,3)) # 3D matrix

sigs[ , , 1] = matrix(c(.3,.2,.2,.3),ncol = 2,byrow = TRUE)
sigs[ , , 2] = matrix(c(.3,-.2,-.2,.3),ncol = 2,byrow = TRUE)
sigs[ , , 3] = matrix(c(.3,.2,.2,.3),ncol = 2,byrow = TRUE)

classes = sample(1:3,size = n,replace = TRUE, prob=pi)
#dataset constructed by a mixture of three Gaussians
data = gen.mix(n = 400,k = classes,sig = sigs,mu = mu)

dev.new()
plot(data,col=c(2,11,4)[classes],pch=19, xlab="X1", ylab="X2")




