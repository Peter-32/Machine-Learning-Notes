####Number 9
library(MASS)
library(ISLR)
Auto2 <- Auto[,!(names(Auto) %in% c('name'))]
rows <- dim(Auto2)[1]
halfRows <- floor(rows/2)
Auto2Train <- Auto2[1:halfRows,]
Auto2Test <- Auto2[(halfRows+1):rows,]
#correlations
pairs(Auto2Train)
cor(Auto2Train, use="complete.obs", method="kendall")
Auto2$origin <- as.factor(Auto2$origin)

##Stepwise building of the model
    fit <- lm(mpg~.,data=Auto2Train)
    step <- stepAIC(fit, direction="both")
    step$anova
    
    #I read the output, which prompts the next steps
    
    #Create the lm
    summary(lm(mpg~.-displacement-cylinders-acceleration,data=Auto2Train))
    
    #I decide to drop horsepower as well.
    Auto2Train <- Auto2Train[,!(names(Auto2Train) %in% c('horsepower','displacement','cylinders','acceleration'))]
    Auto2Test <- Auto2Test[,!(names(Auto2Test) %in% c('horsepower','displacement','cylinders','acceleration'))]
    head(Auto2Train)
    head(Auto2Test)
    summary(lm(mpg~.,data=Auto2Train))

##Build the model again with polynomials and interactions; and transformations; test VIF afterwards
    library(car)
    power <- 2
    #summary(lm(mpg~poly(weight,power)*poly(year,power)*poly(origin,power),data=Auto2Train))
    fit <- lm(mpg~weight+I(weight^2)+year+I(year^2)+origin,data=Auto2Train)
    summary(fit)
    vif(fit)
    
    fit <- lm(mpg~log(weight)+weight+log(year)+year+origin,data=Auto2Train)
    summary(fit)
    vif(fit)
    
    fit <- lm(mpg~sqrt(weight)+weight+sqrt(year)+year+origin,data=Auto2Train)
    summary(fit)
    vif(fit)
    
    #there are other combinations, but the VIF is too high to include these transformations.
    
    #This model is what I think is best so far.
    summary(lm(mpg~weight+year+origin,data=Auto2Train))
    
    #Below were tests for interactions; The best result is listed last
    summary(lm(mpg~weight+year+origin+year*origin,data=Auto2Train))
    summary(lm(mpg~weight+year+origin+weight*year,data=Auto2Train))
    summary(lm(mpg~weight+year+origin+weight*origin,data=Auto2Train))
    
    #Test the collinearity of the new fit from the previous line
    fit <- lm(mpg~year+weight*origin,data=Auto2Train)
    vif(fit) #Reject this interaction on grounds of collinearity added
    
    #Step Anova
    fit <- lm(mpg~weight+year+origin,data=Auto2Train)
    step <- stepAIC(fit, direction="both")
    step$anova
    
    #Fit so far
    summary(fit)

###Note: non-linear, correlated observations, variance changes for inputs, outliers, leverage points, collinearity    
##Test for high leverage points
    p <- 3 #3 predictors
    hats <- hatvalues(fit)
    hats <- sort(hats) #Don't see anything too crazy;
    n <- length(hats)
    avgHat <- (p+1)/n #formula for the average value (p+1)/n
    hats-avgHat #How high above the average value is each observation; sorted least to greatest already.
    #I believe no change is needed
    plot(hatvalues(fit))    #Quick plot of leverages based on observation number; This is probably better than previous lines above
    abline(h=avgHat) #Plot the average value; horizontal line "h"
    which.max(hatvalues(fit)) #Book mentioned using this; the sort already gave this information too.
    #No changes made

##Plot graphs on training and test set.
    fit <- lm(mpg~weight+year+origin,data=Auto2Train)
    plot(fit)

##Could remove outliers
    
##Test the test set; Not sure if this is the right way to do it.
    fitTest <- lm(mpg~weight+year+origin,data=Auto2Test)
    summary(fitTest)
    #R squared is 0.7753
    plot(fitTest)
    
    
    ####Number 10
library(MASS)
library(ISLR)
fit <- lm(Sales~Price+Urban+US,data=Carseats)  
summary(fit) #noticed we cand rop urban yes
plot(fit)
# We remove Urban
# Note if there are multiple levels we test using F test not individual t tests.
fit <- lm(Sales~Price+US,data=Carseats)
#Reject Urban
#Our fit so far isn't very good R squared is .2393
confint(fit)

#High leverage points
plot(hatvalues(fit))
avgHat <- (2+1)/dim(Carseats)[1]
abline(h=avgHat) #Plot the average value; horizontal line "h"
which.max(hatvalues(fit)) #Book mentioned using this; the sort already gave this information too.
hats <- hatvalues(fit)
hats <- sort(hats)

#Finding a better model
fit <- lm(Sales~.,data=Carseats)
step <- stepAIC(fit, direction="both")
step$anova    

fit <- lm(Sales~CompPrice+Income+Advertising+Price+ShelveLoc+Age,data=Carseats)
summary(fit)


####Number 11
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
plot(x,y,pch=20)
#Remove the intercept:
fit <- lm(y~0+x)
abline(fit,lwd=3,col="red")
summary(fit)

plot(y,x,pch=20)
fit <- lm(x~0+y)
abline(fit,lwd=3,col="red")
summary(fit)
#The t value is equivalent for both fits.  The estimate and standard error are different, but the ratio of the two stays the same.

####Number 13
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100,0,.25)
y <- -1 + 0.5*x + eps
fit <- lm(y~x)
summary(fit)
coef(fit)
#(Intercept)     x 
#-1.0094232   0.4997349
#Close to the true parameters B0 and B1 of -1 and 0.5
plot(x,y,pch=20)
abline(fit,lwd=3,col="red")
#example(legend)
legend(list(x = -2,y = 0.2), legend = c("fit"), col = 2:3, lty = 1, merge = TRUE)   #, trace = TRUE)
confint(fit)

plot(x,y,pch=20)
eps <- rnorm(100,0,.05)
fit <- lm(y~x+I(x^2))
abline(fit,lwd=3,col="red")
legend(list(x = -2,y = 0.2), legend = c("fit"), col = 2:3, lty = 1, merge = TRUE)   #, trace = TRUE)
summary(fit)
confint(fit)

eps <- rnorm(100,0,1)
y <- -1 + 0.5*x + eps
plot(x,y,pch=20)
fit <- lm(y~x+I(x^2))
abline(fit,lwd=3,col="red")
legend(list(x = -2,y = 0.2), legend = c("fit"), col = 2:3, lty = 1, merge = TRUE)   #, trace = TRUE)
summary(fit)
confint(fit)

####Number 14
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
fit <- lm(y~x1+x2)
plot(fit)
summary(fit)
