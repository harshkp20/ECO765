#Importing packaes
library(MASS)

#Number of obserations and variables
n<- 100
p<- 20

###Question 1

##Question 1(a)

#Generating 100 observations of 20 independant, identically separated variables of N(0,I20) distribution 
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

#Printing i_1,i_2,i_3,i_4
cat(i_1," ",i_2," ",i_3," ",i_4)

##Question 1(b)

#Creating the matrix of Independant dataset X
vect_1=c(rep(1,100))
sub_X = X[,c(i_1,i_2,i_3,i_4)]
X_mat = cbind(vect_1,sub_X)

#Calculating beta_hat_ls
beta_hat_ls <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% Y
print(beta_cap_ls)

##Question 1(c)

