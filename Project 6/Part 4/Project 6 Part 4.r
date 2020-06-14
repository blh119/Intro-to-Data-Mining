#Brian Holliday
#Professor Li
#Intro to Data Mining 
#13 May 2020

#Project 6d

#Problem 1

dat <- read.csv('Project6 (2).csv', sep = ',', header = TRUE)


dat <- na.omit(dat)

head(dat)

names(dat) <- c('x1', 'x2', 'x3', 'x4', 'x5')

head(dat)

A <- data.frame(cbind(dat$x1, dat$x2, dat$x4))

names(A) <- c('x1','x2','x4')

y <- dat$x5
y

#Standard Multilinear Regression without constant

lin_model  <- lm(y ~.-1, data = data.frame(A,y))

summary(lin_model)


lin_pred <- predict(lin_model, newdata = data.frame(A,y))

RMSE <- sqrt(sum((y_pred)^2))/sqrt(length(y))
RMSE

#Problem 2


#For this regression with are going to use the exponential model
#Will will linearize it to run a linear regression. 

# x5 = b1x1e^(b2x2 + b4x4)

#ln(x5/x1) = ln(b1) + b2x2 + b4x4

x5 <- dat$x5
x1 <- dat$x1
x4 <- dat$x4
x2 <- dat$x2

exp_model <- lm(log(x5/x1) ~., data = data.frame(x2,x4) )
summary(exp_model)

exp_pred <- predict(exp_model, newdata = data.frame(x2,x4))

x1*exp(exp_pred) # this gives us x5 by itself

RMSE <- sqrt(sum((x1*exp(exp_pred) - x5)^2))/sqrt(length(x5))
RMSE

# Problem 3

#We can clearly see that the exponential model is the better model
#with the high R^2 score 
#
#
#
#


