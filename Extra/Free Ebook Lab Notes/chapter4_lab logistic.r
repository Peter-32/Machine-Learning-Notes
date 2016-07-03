library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket) #Error
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
## Logistic Regression
# glm() is a generalized linear model; logistic regression is family=binomial
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial)
summary(glm.fit) #The smallest p-value here is Lag1 and it is a negative coefficient; still it is a large p-value at 0.145
coef(glm.fit) #This accesses the coefficients of the model
summary(glm.fit)$coef #Alternatively you can do this to pull from summary
summary(glm.fit)$coef[,4]

# predict() can be used to predict given new values; type="response" means P(Y=1|X=x)
contrasts(Smarket$Direction) # Tells us which value is 1 and which is 0.
glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]

#Let's put our prediction in the table
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

table(glm.pred, Direction) ##Confusion matrix; this should be used in most classification problems
(507+145)/1250 #correct answers
mean(glm.pred==Direction) #similar way to doing it
# 47.8% training error rate; we still have to apply this to a test set
# We can get a better picture by splitting the data into a training and test set

train=(Year<2005) #Creates a boolean array of length 1250 same as the number of observations
Smarket.2005=Smarket[!train,]   #Smarket[!train,] is test set; Smarket[train,] is training set
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)


##Let's use a model that has the lowest p-values
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)



## Without looking at the previous lab, recreate it but use a more conservative approach where you only invest in days where
## your prediction surpasses a threshold.  Then test to see if the direction was up or down given your prediction of up or down.
## After a model is chosen, fit it again to all the data to get new coefficients

attach(Smarket)
cor(Smarket[,-9])
names(Smarket)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)

train=(Year<2005)
testSet=Smarket[!train,]
trainingSet=Smarket[train,]
dim(test)
dim(trainingSet)


#Long fitting process
#summary(glm(Direction~Lag1,data=trainingSet,family=binomial))$coef[8]
#summary(glm(Direction~Lag2,data=trainingSet,family=binomial))$coef[8]
#summary(glm(Direction~Lag3,data=trainingSet,family=binomial))$coef[8]
#summary(glm(Direction~Lag4,data=trainingSet,family=binomial))$coef[8]
#summary(glm(Direction~Lag5,data=trainingSet,family=binomial))$coef[8]
#summary(glm(Direction~Volume,data=trainingSet,family=binomial))$coef[8]

#summary(glm(Direction~Lag1+Lag2,data=trainingSet,family=binomial))$coef
#summary(glm(Direction~Lag1+Lag3,data=trainingSet,family=binomial))$coef
#summary(glm(Direction~Lag1+Lag4,data=trainingSet,family=binomial))$coef
#summary(glm(Direction~Lag1+Lag5,data=trainingSet,family=binomial))$coef
#summary(glm(Direction~Lag1+Volume,data=trainingSet,family=binomial))$coef

#summary(glm(Direction~Lag1+Lag2+Volume,data=trainingSet,family=binomial))$coef
#Two models I've decided to go with
glm.fit1=glm(Direction~Lag1,data=Smarket,family=binomial,subset=train)
glm.fit2=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)


### FIT 1
glm.probs=predict(glm.fit1,testSet,type="response")
glm.probs[1:10]
summary(glm.probs)
hist(glm.probs,breaks=100)
contrasts(Direction)

#95% confidence 
threshold<-.50 + 2.0*sd(glm.probs)

pred.direction=rep("Ignore",252)
pred.direction[glm.probs>=threshold]="Up"

table(pred.direction,testSet$Direction)
table(pred.direction,testSet$Direction)[2,2]/(table(pred.direction,testSet$Direction)[2,2]+table(pred.direction,testSet$Direction)[2,1])
#59% success over 42 days



### FIT 2
glm.probs=predict(glm.fit2,testSet,type="response")
glm.probs[1:10]
summary(glm.probs)
hist(glm.probs,breaks=100)
contrasts(Direction)

#95% confidence 
threshold<-.50 + 2.0*sd(glm.probs)

pred.direction=rep("Ignore",252)
pred.direction[glm.probs>=threshold]="Up"

table(pred.direction,testSet$Direction)
table(pred.direction,testSet$Direction)[2,2]/(table(pred.direction,testSet$Direction)[2,2]+table(pred.direction,testSet$Direction)[2,1])
#54.5% success over 22 days

### FIT 1 to ALL DATA
glm.fit=glm(Direction~Lag1,data=Smarket,family="binomial")
glm.probs=predict(glm.fit1,Smarket,type="response")
glm.probs[1:10]
summary(glm.probs)
hist(glm.probs,breaks=100)
contrasts(Direction)

#95% confidence 
threshold<-.50 + 2.0*sd(glm.probs)

pred.direction=rep("Ignore",1250)
pred.direction[glm.probs>=threshold]="Up"

table(pred.direction,Smarket$Direction)
table(pred.direction,Smarket$Direction)[2,2]/(table(pred.direction,Smarket$Direction)[2,2]+table(pred.direction,Smarket$Direction)[2,1])
#Training set 50.7% success and 49.3% error rate






