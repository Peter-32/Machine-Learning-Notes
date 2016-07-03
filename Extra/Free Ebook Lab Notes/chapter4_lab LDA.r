library(MASS)
library(ISLR)
attach(Smarket)

train=(Year<2005) #Creates a boolean array of length 1250 same as the number of observations
Smarket.2005=Smarket[!train,]   #Smarket[!train,] is test set; Smarket[train,] is training set
dim(Smarket.2005)
Direction.2005=Direction[!train]


lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit) #Plots the linear discriminants
#Remember if these are large we say y=1 (up).  The coefficients are used such that -.642*Lag1-.514*Lag2 is either large or small
#large means group "Up"

#There are three elements to predict for LDA.
#1) class - prediction about movement of the market
#2) posterior, a matrix whose kth column contains the P(Y=k|X=x)
#3) x contains the linear discriminants described earlier
lda.pred=predict(lda.fit, Smarket.2005)
head(lda.pred$class)
head(lda.pred$posterior)
head(lda.pred$x)

lda.class=lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)

#Applying a 50% threshold to the posterior probabilities allows us to recreate the predictions in lda.pred$class
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1] #Probability of a decrease
lda.class[1:20]

#If we want a different threshold it is easy to do so
sum(lda.pred$posterior[,1]>.9) #0; the greatest posterior probability of decrease in all of 2005 was 52.02%





