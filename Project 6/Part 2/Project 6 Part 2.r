#Brian Holliday
#Professor Li
#Intro to Data Mining 
#4 April 2020

#Project 6b

#Problem 1
dat <- read.csv('Project6 (2).csv', sep = ',', 
                 header = TRUE)
dat <- na.omit(dat)

names(dat) <- c('x1','x2','x3','x4','x5')


# Find the linear model for x3 in terms of x1, x2, x4 and x5

multireg_model <- lm(x3 ~ x1 + x2 + x4 + x5 - 1, data = dat, na.action = na.omit)

summary(multireg_model)

#Problem 2

y <- na.omit(dat$x3)

y_pred <- predict(multireg_model, newdata = dat)

RMSE <- sqrt(sum((y_pred - dat$x3)^2))/sqrt(length(y))
RMSE



