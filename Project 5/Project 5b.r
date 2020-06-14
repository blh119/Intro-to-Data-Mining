#Brian Holliday 
#Professor Li
#Introduction to Data Mining 
#27 April 2020

#Project 5B

#Number 1

library(rpart)
library(rpart.plot)

cars = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data",
                  sep = ",", header = FALSE)

names(cars) = c('buying', 'maint', 'doors',
                'persons', 'lug_boot', 'safety', 'car_evaluation')

ctree = rpart(car_evaluation ~ ., data = cars, main = 'Decision tree for Car Evaluation')  #default split="gini"
ctree

#Plot the tree 
rpart.plot(ctree, main="Decision tree for cars",type = 3)


#Different ways to display the tree:  yesno=2, type=3, type=4
#To change the depth to 2: maxdepth=2
#To get the fully-grown tree: cp=-1

#predict using the tree model
cars_predict = predict(ctree, cars, type = "class")
cars_predict


#compute the misclassification error
cars_error = sum(cars_predict != cars$car_evaluation)/nrow(cars)
cars_error


#Number 2
#Part A

#According to the decision tree, the evaluation for the performence car is: unaccecptabe.
#The tree suggests that any car that only seats two people is going to get a grade of unacceptable,
#with 33 percent of the data points seating only two people. 

#Part B

#According to the decision tree, the evaluation for the compact SUV is: acceptable
#This type of car is rare in this dataset only accounting for 2 percent of the data rows

#Number3
#According the the tree there is only one way to get a very good car:
#buying = low/med
#maint = low/med
#persons = 4/more
#lug_boot = big/med
#safety = high

#This type of car only accounts for 4 percent of the data. 
#This critera also describes a large suv/mini-van type of vechical. 
#This vechical must be of low to medium price, with good storage space, 

#Number4

#split the data into training data(80%) and testing data(20%)
#compute the error for the testing and training data for depth 1-8

testing <- rep(1:8)
training <- rep(1:8)

#maxdepth = 1
train_sample = sample(1:nrow(cars), floor(nrow(cars)*0.80))
train_data = cars[train_sample, ]
test_data = cars[-train_sample, ]

#get tree for train and test
fit_tree_train_1 = rpart(car_evaluation ~ ., data = train_data, maxdepth = 1)
fit_tree_test_1 = rpart(car_evaluation ~., data = test_data, maxdepth = 1)

#predict the training and testing data
train_predict_1 = predict(fit_tree_train_1, train_data, type = "class")
test_predict_1 = predict(fit_tree_test_1, test_data, type = 'class')

#Get the error for both the training and test
train_error_1 = sum(train_predict_1 != train_data$car_evaluation)/nrow(train_data)
test_error_1 = sum(test_predict_1 != test_data$car_evaluation)/nrow(test_data)


train_error_1
test_error_1

#add it to the list
training[1] <- train_error_1
testing[1] <- test_error_1


#maxdepth = 2

#get tree for train and test
fit_tree_train_2 = rpart(car_evaluation ~ ., data = train_data, maxdepth = 2)
fit_tree_test_2 = rpart(car_evaluation ~., data = test_data, maxdepth = 2)

#predict the training and testing data
train_predict_2 = predict(fit_tree_train_2, train_data, type = "class")
test_predict_2 = predict(fit_tree_test_2, test_data, type = 'class')

#Get the error for both the training and test
train_error_2 = sum(train_predict_2 != train_data$car_evaluation)/nrow(train_data)
test_error_2 = sum(test_predict_2 != test_data$car_evaluation)/nrow(test_data)


train_error_2
test_error_2

#add it to the list
training[2] <- train_error_2
testing[2] <- test_error_2


#maxdepth = 3

#get tree for train and test
fit_tree_train_3= rpart(car_evaluation ~ ., data = train_data, maxdepth = 3)
fit_tree_test_3 = rpart(car_evaluation ~., data = test_data, maxdepth = 3)

#predict the training and testing data
train_predict_3 = predict(fit_tree_train_3, train_data, type = "class")
test_predict_3 = predict(fit_tree_test_3, test_data, type = 'class')

#Get the error for both the training and test
train_error_3 = sum(train_predict_3 != train_data$car_evaluation)/nrow(train_data)
test_error_3 = sum(test_predict_3 != test_data$car_evaluation)/nrow(test_data)


train_error_3
test_error_3

#add it to the list
training[3] <- train_error_3
testing[3] <- test_error_3


#maxdepth = 4

#get tree for train and test
fit_tree_train_4 = rpart(car_evaluation ~ ., data = train_data, maxdepth = 4)
fit_tree_test_4 = rpart(car_evaluation ~., data = test_data, maxdepth = 4)

#predict the training and testing data
train_predict_4 = predict(fit_tree_train_4, train_data, type = "class")
test_predict_4 = predict(fit_tree_test_4, test_data, type = 'class')

#Get the error for both the training and test
train_error_4 = sum(train_predict_4 != train_data$car_evaluation)/nrow(train_data)
test_error_4 = sum(test_predict_4!= test_data$car_evaluation)/nrow(test_data)


train_error_4
test_error_4

#add it to the list
training[4] <- train_error_4
testing[4] <- test_error_4


#maxdepth = 5

#get tree for train and test
fit_tree_train_5 = rpart(car_evaluation ~ ., data = train_data, maxdepth = 5)
fit_tree_test_5 = rpart(car_evaluation ~., data = test_data, maxdepth = 5)

#predict the training and testing data
train_predict_5 = predict(fit_tree_train_5, train_data, type = "class")
test_predict_5 = predict(fit_tree_test_5, test_data, type = 'class')

#Get the error for both the training and test
train_error_5 = sum(train_predict_5 != train_data$car_evaluation)/nrow(train_data)
test_error_5 = sum(test_predict_5 != test_data$car_evaluation)/nrow(test_data)


train_error_5
test_error_5

#add it to the list
training[5] <- train_error_5
testing[5] <- test_error_5


#maxdepth = 6

#get tree for train and test
fit_tree_train_6 = rpart(car_evaluation ~ ., data = train_data, maxdepth = 6)
fit_tree_test_6 = rpart(car_evaluation ~., data = test_data, maxdepth = 6)

#predict the training and testing data
train_predict_6 = predict(fit_tree_train_6, train_data, type = "class")
test_predict_6 = predict(fit_tree_test_6, test_data, type = 'class')

#Get the error for both the training and test
train_error_6 = sum(train_predict_6 != train_data$car_evaluation)/nrow(train_data)
test_error_6  = sum(test_predict_6 != test_data$car_evaluation)/nrow(test_data)


train_error_6
test_error_6

#add it to the list
training[6] <- train_error_6
testing[6] <- test_error_6


#maxdepth = 7

#get tree for train and test
fit_tree_train_7 = rpart(car_evaluation ~ ., data = train_data, maxdepth = 7)
fit_tree_test_7 = rpart(car_evaluation ~., data = test_data, maxdepth = 7)

#predict the training and testing data
train_predict_7 = predict(fit_tree_train_7, train_data, type = "class")
test_predict_7 = predict(fit_tree_test_7, test_data, type = 'class')

#Get the error for both the training and test
train_error_7 = sum(train_predict_7 != train_data$car_evaluation)/nrow(train_data)
test_error_7 = sum(test_predict_7 != test_data$car_evaluation)/nrow(test_data)

train_error_7
test_error_7

#add it to the list
training[7] <- train_error_7
testing[7] <- test_error_7


#maxdepth = 8

#get tree for train and test
fit_tree_train_8 = rpart(car_evaluation ~ ., data = train_data, maxdepth = 8)
fit_tree_test_8 = rpart(car_evaluation ~., data = test_data, maxdepth = 8)

#predict the training and testing data
train_predict_8 = predict(fit_tree_train_8, train_data, type = "class")
test_predict_8 = predict(fit_tree_test_8, test_data, type = 'class')

#Get the error for both the training and test
train_error_8 = sum(train_predict_8 != train_data$car_evaluation)/nrow(train_data)
test_error_8 = sum(test_predict_8 != test_data$car_evaluation)/nrow(test_data)


train_error_8
test_error_8

#add it to the list
training[8] <- train_error_8
testing[8] <- test_error_8

training
testing
depth <- rep(1:8)

error <- data.frame(depth, training, testing)
error

#We could also see this in a graph

 

#Problem 5
#The results of the of calcalating the error for both the training and the testing data we see that most of the time
#the training error is higher than the testing error. The outlier in this evualtion is the maxdepth = 2 row. We see that
#depth 1 through 5 the testing and training error are very close, but the calculation for the testing error goes
#unchanged after maxdepth is equal to 5. This tell us that a depth 5 tree is good enough for the tree 
#because not much important data is missing from the tree after depth five. 


#Problem 6

#training_data 
ctree = rpart(car_evaluation ~ ., data = train_data, main = 'Decision tree for Car Evaluation')  #default split="gini"
ctree

rpart.plot(ctree, type=3,  main="Decision tree for Car Evaluation Training")


#testing_daa
ctree = rpart(car_evaluation ~ ., data = test_data, main = 'Decision tree for Car Evaluation')  #default split="gini"
ctree

rpart.plot(ctree, type=3,  main="Decision tree for Car Evaluation Testing")

#According to our tree the biggest facting in the tree is persons or the amount of passengers the car can fit. We see
#that if you car can only seat two people, it is unacceptable and that accounts for 33 percent of the data. Next would
#be the safety rating. If you car can seat four or more people and the safety is low this accounts for 22 percent of the
#data. The third most important factor is the buying price which accounts for the remaining 45 percent of the data. 