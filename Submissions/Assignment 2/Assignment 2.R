#Importing Packages
library(ggplot2)
library(MASS)
library(glmnet)
library(nnet)

#Finding the coefficient vector beta_hat_ls
coefficient_vector <-function(X,Y,n,indices){
  
  #Creating the matrix of Independent data set X
  sub_X = X[,indices]
  
  #Calculating beta_hat_ls
  beta_hat_ls <- solve(t(sub_X) %*% sub_X) %*% t(sub_X) %*% Y
  return(beta_hat_ls)
}

#Number of observations and variables
n<- 100
p<- 20

###Question 1

##Question 1(a)

#Generating 100 observations of 20 independent, identically separated variables of N(0,I20) distribution 
mean <- rep(0,p)
covariance <- diag(p)
X<- mvrnorm(n, mu=mean, Sigma = covariance)

#Generating 4 numbers i1,i2,i3,i4 from [1:20] randomly without replacement
i_vector = c(sample(1:20, 4, FALSE))

i_1<-i_vector[1]
i_2<-i_vector[2]
i_3<-i_vector[3]
i_4<-i_vector[4]

#Generating 4 numbers a,b,c,d from [1:20] randomly without replacement
A_vector <- c(rnorm(4,mean=0,sd=0.25))
a<-A_vector[1]
b<-A_vector[2]
c<-A_vector[3]
d<-A_vector[4]

#Generating Y1,Y2,Y3..........Y100
Y<-rep(0,100)
for(k in 1:100){
  n_k <-rnorm(1,mean=0,sd=0.01)
  Y[k]<-(a*X[k,i_1]+b*X[k,i_2]+c*X[k,i_3]+d*X[k,i_4]+n_k)
}

cat("The value of i1, i2, i3, i4 is [",i_1," ",i_2," ",i_3," ",i_4,"]", "\n")      #Printing i_1,i_2,i_3,i_4

##Question 1(b)

#Finding coefficient vector
cat("beta_hat_ls is [",coefficient_vector(X,Y,n,c(1:20)),"]", "\n")           #printing beta_hat_ls

##Question 1(c) - FORWARD SELECTION METHOD

# Initializing the best predictor and minimum value of RSS to infinity
best_predictors <- c()
min_rss <- Inf

# Traverse through all the predictors
for (i in 1:20) {
  # Check if i has already been selected
  if (i %in% best_predictors) {
    next
  }
  
  # Add i to set of candidate predictors
  candidate_predictors <- c(best_predictors, i)
  candidate_predictors
  
  # Fit the model with candidate predictors
  x <- X[, candidate_predictors]
  #print(x)
  x_ <- cbind(rep(1, n), x)  # Add intercept term
  #print(x_)
  y <- Y
  beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
  
  # Compute RSS
  y_hat <- x %*% beta_hat
  rss <- sum((y - y_hat)^2)
  
  # Check if RSS is lower than current minimum
  if (rss < min_rss) {
    best_predictors <- candidate_predictors
    min_rss <- rss
  }
  
  # Stop if desired number of predictors have been chosen
  if (length(best_predictors) == 4) {
    break
  }
}
#Printing the best set of parameters
cat("Best predictors: [", best_predictors,"]", "\n")

#Finding and printing the corresponding coefficient vector
cat("Corresponding Coefficient Vector is: [",coefficient_vector(X,Y,n,best_predictors),"]", "\n")

##Question 1(d) - RIDGE REGRESSION

#Centering the data set
X_centred <- scale(X,center=TRUE, scale=FALSE)
Y_centred <- scale(Y,center=TRUE, scale=FALSE)

#defining lambda
lambda <- 0.01

#Defining the ridge penalty matrix
R_pen <- diag(lambda, ncol(X_centred))

#Finding beta_ridge
beta_ridge <- solve(t(X_centred)%*%X_centred+R_pen)%*%t(X_centred)%*%Y_centred
cat("The coefficient vector beta_ridge is as follows")
print(beta_ridge)          #Printing beta_ridge vector

#Sorting in decreasing order
indices <- c(1:20)
abs_beta_ridge <- abs(beta_ridge)
abs_beta_ridge <- cbind(indices,abs_beta_ridge)
sorted_beta_ridge <- abs_beta_ridge[order(abs_beta_ridge[,2],decreasing = TRUE),]

#Printing the indices that correspond to the five highest coefficients.
cat("The indices that correspond to the five highest coefficients are : [",sorted_beta_ridge[1:5,1],"]","\n")

##Question 1(e) Lasso Regression

X_lasso <- X_centred                #Taking the X_centered matrix

cost_lasso <- function(beta) {
  Y_hat <- sum((Y_centred - X_lasso %*% beta)^2) + lambda * sum(abs(beta))
  return(Y_hat)
}
beta_lasso <- optim(beta_hat_ls,cost_lasso)

cat("The coefficient vector beta_lasso is as follows")
print(beta_lasso)          #Printing beta_lasso vector

#Sorting in decreasing order
abs_beta_lasso <- abs(beta_lasso$par)
abs_beta_lasso <- cbind(indices,abs_beta_lasso)
sorted_beta_lasso <- abs_beta_lasso[order(abs_beta_lasso[,2],decreasing = TRUE),]

#Printing the indices that correspond to the five highest coefficients.
cat("The indices that correspond to the five highest coefficients are : [",sorted_beta_lasso[1:5,1],"]","\n")


### Question 2

## Question 2(a)

#Generating 50 observations of 2 independent, identically separated variables of N([0,0],I2) distribution 
mean1 <- c(0,0)
covariance1 <- diag(2)
bin1<- mvrnorm(50, mu=mean1, Sigma = covariance1)

#Generating 50 observations of 2 independent, identically separated variables of N([2,0],I2) distribution 
mean2 <- c(2,0)
covariance2 <- diag(2)
bin2<- mvrnorm(50, mu=mean2, Sigma = covariance2)

#Generating 50 observations of 2 independent, identically separated variables of N([1,sqrt(3)],I2) distribution 
mean3 <- c(1,sqrt(3))
covariance3 <- diag(2)
bin3<- mvrnorm(50, mu=mean3, Sigma = covariance3)

# Combining the data points and their labels into a data frame
data <- data.frame(x = c(bin1[,1], bin2[,1], bin3[,1]), 
                   y = c(bin1[,2], bin2[,2], bin3[,2]), 
                   label = c(rep("bin1", 50), rep("bin2", 50), rep("bin3", 50)))

# Plotting the data points on a scatter plot with different colors for each label
ggplot(data, aes(x = x, y = y, color = label)) +
  geom_point() +
  labs(x = "X1", y = "X2", color = "Label") +
  theme_bw()

##Question 2(b)

# Re-framing the above data
X_ind <- rbind(bin1, bin2, bin3)
Y_ind <- rep(1:3, each = 50)

# Creating indicator matrices for the 3 bins
K <- 3                                            #No. of Bins
G <- matrix(0, nrow = length(Y_ind), ncol = K)    #Creating matrix for storing indicator variables
for (i in 1:K) {
  G[Y_ind == i, i] <- 1                         
}

# Finding the linear classifier using indicator matrix
X_aug <- cbind(1, X_ind)
theta <- solve(t(X_aug) %*% X_aug) %*% t(X_aug) %*% G

# Plotting the scatter plot with classifying lines
Y_label <- rep('a',150)
for(i in 1:150){
  if(Y_ind[i]==1) Y_label[i]<-"bin1"
  if(Y_ind[i]==2) Y_label[i]<-"bin2"
  if(Y_ind[i]==3) Y_label[i]<-"bin3"
}
ggplot(data = data.frame(x = X_ind[,1], y = X_ind[,2], label = factor(Y_label)), aes(x = x, y = y, color = label)) +
  geom_point() +
  stat_function(fun = function(x) (-theta[1, 1] - theta[2, 1]*x)/theta[3, 1], color = "red") +
  stat_function(fun = function(x) (-theta[1, 2] - theta[2, 2]*x)/theta[3, 2], color = "green") +
  stat_function(fun = function(x) (-theta[1, 3] - theta[2, 3]*x)/theta[3, 3], color = "blue") +
  labs(x = "X1", y = "X2", color = "Label") +
  theme_bw()

##Question 2(c)

#Taking the value of X_lda and Y_lda
X_lda <- X_ind
Y_lda <- Y_ind

# Finding the mean for class
u1 <- colMeans(X_lda[Y_lda == 1,])
u2 <- colMeans(X_lda[Y_lda == 2,])
u3 <- colMeans(X_lda[Y_lda == 3,])

#Calculating the in-class covariance matrix
Sw1 <- t(X_lda[Y_lda == 1,]-u1)%*%(X_lda[Y_lda == 1,]-u1)
Sw2 <- t(X_lda[Y_lda == 2,]-u2)%*%(X_lda[Y_lda == 2,]-u2)
Sw3 <- t(X_lda[Y_lda == 3,]-u3)%*%(X_lda[Y_lda == 3,]-u3)
Sw <- (Sw1 + Sw2 + Sw3)

# Finding the overall mean
u = colMeans(X_lda)

#Finding the between class covariance matrix
Sb <- 50 *(((u1 - u) %*% t(u1 - u)) +
           ((u1 - u) %*% t(u1 - u)) +
           ((u1 - u) %*% t(u1 - u)))

# Finding the LDA coefficients
# theta <- solve(Sw) %*% (u1 - u2 + u1 - u3)
Sw_inv_Sb <- solve(Sw) %*% Sb
eigenvalues <- eigen(Sw_inv_Sb, symmetric = TRUE)$values
eigenvectors <- eigen(Sw_inv_Sb, symmetric = TRUE)$vectors
theta <- eigenvectors[, 1:(length(unique(Y_lda)) - 1)]

# Plotting the scatter plot with classifying lines
ggplot(data = data.frame(x = X_lda[,1], y = X_lda[,2], label = factor(Y_label)), aes(x = x, y = y, color = label)) +
  geom_point() +
  geom_abline(intercept = -theta[1]/theta[2], slope = -theta[3]/theta[2], color='red') +
  geom_abline(intercept = -theta[2]/theta[3], slope = -theta[1]/theta[3], color='green') +
  geom_abline(intercept = -theta[3]/theta[1], slope = -theta[2]/theta[1], color='blue')
  labs(x = "X1", y = "X2", color = "Label") +
  theme_bw()

## Question 2(d) - Logistic Regression

#Taking the value of X_logr and Y_logr
X_logr <- X_ind
Y_logr <- Y_ind

#Setting up the training and test data
set.seed(123)
train <- sample(1:150, 100, replace = FALSE)
test <- setdiff(1:150, train)
X_train = X_logr[train,]
Y_train = Y_logr[train]
X_test = X_logr[test,]
Y_test = Y_logr[test]

#Creating data frame from these
train <- data.frame(cbind(X_train, Y_train))
test <- data.frame(cbind(X_test, Y_test))

# Fitting the logistic regression model using glm
fit <- glmnet(as.matrix(train[, 1:2]), train[, 3], family = "multinomial")

# Predicting the classes for the test data
pred <- predict(fit, as.matrix(test[, 1:2]), type = "class")

# Plotting the scatter plot with classifying lines
ggplot(data = data.frame(x = X_logr[,1], y = X_logr[,2], label = factor(Y_label)), aes(x = x, y = y, color = label)) +
  geom_point() +
  labs(x = "X1", y = "X2", color = "Label") +
  theme_bw()
  
