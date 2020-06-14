#Brian Holliday
#Professor Li 
#Data Minin
#22 May 2020 

#import data

crime <- read.csv('crimedata(1).csv', header = TRUE, sep = ',')
head(crime)


names(crime)[1] <- c('population')
names(crime)

#Crime Rate is value that we are trying to predict

names(crime)[25] <- c('CrimeRate')
names(crime)

#Replace ? with NA and remove NA values for prediction column
arr <- which(crime$CrimeRate == '?')

length(arr)/length(crime$CrimeRate) #about 10 percent of the data is missing


crime$CrimeRate[arr] <- NA #replace ? with na values 

library(glmnet)


indx <- sapply(crime, is.factor)
indx

crime[indx] <- lapply(crime[indx], function(x) as.numeric(as.character(x)))

sapply(crime, is.factor)

xvalues <- as.matrix(subset(crime,select = -c(CrimeRate)))

library(glmnetUtils)

lars_model <- glmnet(CrimeRate ~ ., data = crime, 
                     alpha=1, 
                     standardize= TRUE)  

plot(lars_model, xvar = "lambda", label = TRUE)  

x_val <- subset(crime, select=-c(CrimeRate))
crime_pred <- predict(lars_model, newdata = x_val, na.action = na.pass) 


err_crime <- sqrt(colMeans((crime_pred - crime$CrimeRate)^2, 
                           na.rm = TRUE ))

lars_model$lambda #100 lambda values

plot(x = log(lars_model$lambda), y = err_crime, pch = 20)








#Split the data 80% training 20% testing

train_sample = sample(1:nrow(crime), floor(nrow(crime)*0.80))
train_crime = crime[train_sample, ]
test_crime = crime[-train_sample, ]









#lets get the train error

train_crime_lars <- glmnet(CrimeRate ~., data = train_crime, alpha = 1
                     , standardize = TRUE)

x_train <- subset(train_crime, select = -c(CrimeRate))

lambda_sel <- 10^seq(-5,5,0.05)



pred_train <- predict(train_crime_lars, newdata = x_train,
                      na.action = na.pass,
                      s = lambda_sel)

err_train <- sqrt(colMeans((pred_train - train_crime$CrimeRate)^2, na.rm = TRUE)) 

err_train


plot(x = log10(lambda_sel), y = err_train,
     xlab = 'log(lambda)',
     ylab = 'RMSE',
     type = 'o', lty = 1, pch = 20, col = 'blue',
     main = 'Log10(Lambda) vs Training RMSE ')







#lets get the test error

test_crime_lars <- glmnet(CrimeRate ~., data = test_crime, alpha = 1
                           , standardize = TRUE)

x_test <- subset(test_crime, select = -c(CrimeRate))

lambda_sel <- 10^seq(-5,5,0.05)

pred_test <- predict(test_crime_lars, newdata = x_train,
                      na.action = na.pass,
                      s = lambda_sel)

err_test <- sqrt(colMeans((pred_test- test_crime$CrimeRate)^2, na.rm = TRUE)) 

err_test

plot(x = log10(lambda_sel), y = err_test,
     xlab = 'log(lambda)',
     ylab = 'RMSE',
     type = 'o', lty = 1, pch = 20, col = 'red',
     main = 'Log10(lambda vs Testing Error' )






#Plot the training and testing error together

plot(x = log10(lambda_sel), y = err_train,
     xlab = 'log(lambda)',
     ylab = 'RMSE',
     ylim = c(200, 1000),
     type = 'o', lty = 1, pch = 20, col = 'blue',
     main = 'Log10(Lambda) vs Training RMSE ')

lines(log10(lambda_sel),err_test,
      col = 'red')

legend(-4,800, legend = c('Training Error', 'Testing Error'),
       col = c('blue', 'red'), lty = 1, pch = 20)






#Diagram of abs(training error - testing error)

plot(x = log10(lambda_sel), 
     y = abs(err_train - err_test),
     xlab = 'log(lambda)',
     ylab = 'Training-Testing Difference',
     xlim = c(-4,4),
     type = 'o', lty = 1, pch = 20,
     main = 'log(lambda) vs Error Difference',
     col = 'purple')

legend(-4,400, legend = c('Error Difference'),
      col = c('purple'), lty = 1, pch = 20)

#we can say that lambda for zone 2 is between 1 and 2.5




#from looking at the graph we will choose lambda values

opt_lambda <- 10^seq(1.95,2.5,0.05)

coef <- coef(train_crime_lars, s = opt_lambda)

coef

as.matrix(log10(opt_lambda))








