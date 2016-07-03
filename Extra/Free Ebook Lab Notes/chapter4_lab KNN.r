#We model KNN using the function knn()
#It is a one step process that requires 4 inputs
#1) Matrix containing the predictors associating with the training data, train.X
#2) Matrix containing the predictors associated with the data for which we wish to make predictions, test.X
#3) A vector containing the class labels for the training observations, train.Direction
#4) A value for K, the number of nearest neighbors to be used by the classifier

library(class)
library(MASS)
library(ISLR)
attach(Smarket)

train=(Year<2005) #Creates a boolean array of length 1250 same as the number of observations
Smarket.2005=Smarket[!train,]   #Smarket[!train,] is test set; Smarket[train,] is training set
dim(Smarket.2005)
Direction.2005=Direction[!train]


train.X=cbind(Lag1,Lag2)[train,]
train.X
test.X=cbind(Lag1,Lag2)[!train,]
test.X
train.Direction=Direction[train]

#We set a seed because observations are tied as nearest neighbors and R will randomly break a tie.
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)


#Messing around finding best k value
knn.max.mean <- 0
for (i in 1:50) {
    set.seed(1)
    knn.pred=knn(train.X,test.X,train.Direction,k=i)
    table(knn.pred,Direction.2005)
    knn.mean=mean(knn.pred==Direction.2005)
    if (knn.mean > knn.max.mean) {
        knn.max.mean = knn.mean
        knn.max.k = i
    }
}
knn.max.k
1 - knn.max.mean
#3 turns out to give the lowest test error rate of 46.8%

#QDA gave the best results.