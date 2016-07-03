library(ISLR)
#Response variable is "Purchase"; whether or not a given individual purchases a caravan insurance policy.

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
#KNN scale is very important.  50 years of age is miniscule compared to $1,000 of salary
#A good way is to standardize the data so all have mean zero and sd of one.
#scale() function does just this
#The 86th column is the Purchase variable
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

#We now split it into a test and training set with K = 1
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)

knn.pred=knn(train.X,test.X,train.Y,3)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)

knn.pred=knn(train.X,test.X,train.Y,5)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
#Messing around looking for best k value
#contrasts(test.Y[1])
#knn.max.mean <- 0
#for (i in 1:50) {
#    set.seed(1)
#    knn.pred=knn(train.X,test.X,train.Y,k=i)
#    table(knn.pred,test.Y)
#    knn.mean=mean(ifelse(knn.pred==1,10,1)*(knn.pred==test.Y)) ## 10 points for getting a yes right
#    if (knn.mean > knn.max.mean) {
#        knn.max.mean = knn.mean
#        knn.max.k = i
#    }
#}
#knn.max.k
#1 - knn.max.mean


# We can try fitting logistic regression.
# We get a 5 times better result than random guessing by using a .25% chance of purchase as the cut-off
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)






