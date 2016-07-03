library(MASS)
library(ISLR)
attach(Smarket)

train=(Year<2005) #Creates a boolean array of length 1250 same as the number of observations
Smarket.2005=Smarket[!train,]   #Smarket[!train,] is test set; Smarket[train,] is training set
dim(Smarket.2005)
Direction.2005=Direction[!train]

#Identical to lda
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
#Great error rate of 40% for stock market data!  This may suggest that this method captures the true relationship
#More data should be collected before coming to a conclusion about if this will consistantly beat the market.

