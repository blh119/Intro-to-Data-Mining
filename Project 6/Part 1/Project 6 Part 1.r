#Brian Holliday
#Professor Li
#Intro to Data Mining
#29 April 2020

#Project 6 Part 1


#Number 1
x <- c(1,2,0,3)
y <- c(1,2,0,2)

k <- sum((x - mean(x))*(y - mean(y)))/sum((x - mean(x))^2)
k

b <- mean(y) - k*mean(x)
b

#rline = 0.2*x + 0.7

#Number 2

lin_model <- lm(y ~ x)
lin_model #we get the same answer


#Number 3

E <- (sum(((k*x + b) - y)^2))^(1/2)
E


#Number 4

y_pred <- predict(lin_model, newx=x)
RMSE <- sqrt(sum((y_pred-y)^2))/sqrt(length(y))
RMSE


