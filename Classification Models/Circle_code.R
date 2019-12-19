#Fall IS542
#Option B : Logistics Regression and Naive Bayes Classifier

#   Data Source Link: http://abel.ischool.illinois.edu/data/circle.arff

# By:-
#   Sayali Mohite

#Using R version 3.6.1 (2019-07-05) -- "Action of the Toes"

#Get the data 
#And storing it in the Current Working Directory


# Loading the ggplot library
library(ggplot2)

#Changing the working Directory
setwd("C:\\Users\\admin\\Desktop\\Data Stats\\Assignment 07")

# reading the data
circle = read.csv("circle.arff", header=FALSE, comment.char = "@")

######################## MODEL 1: Logistics Regression without Polynomical terms

# Fitting Logistics model
logistic_model = glm(V3~V2+V1, family = "binomial", data = circle)

# Predicting the probability of having a circle
p =predict(logistic_model, circle, type = "response")

#Making 0.5 or more as a cutoff rate for having circle 
Logistic = ifelse(p >= 0.5, 1, 0)

#plotting the expected output as per logistics regression
ggplot(circle, aes(x = V1, y =V2, color = Logistic)) + 
  geom_point(size = 2) + 
  ggtitle("Logistics Regression without Polynomial Terms")


######################## MODEL 2: Logistics Regression with Polynomical terms

#Fitting the Logistics Regression
logistic_model = glm(V3~V2+V1+ I(V1^2)+ I(V2^2), family = "binomial", data = circle)

# Predicting the probability of having a circle
p =predict(logistic_model, circle, type = "response")

#Making 0.5 or more as a cutoff rate for having circle 
Logistic_poly = ifelse(p >= 0.5, 1, 0)

#ploting the expected output as per logistics regression with polynomial terms
ggplot(circle, aes(x = V1, y =V2, color = Logistic_poly)) + 
  geom_point(size = 2) +
  ggtitle("Logistics Regression with Polynomial Terms")


######################## MODEL 3: Naive Bayes Classifier

# Loading the library
library(e1071)

#Fitting the naive bayes classifier
circle_nb = naiveBayes(V3 ~V1+V2, data=circle)


# Predicting the probability of having a circle
nb_p = predict(circle_nb, circle, type = "raw")

#Making 0.5 or more as a cutoff rate for having circle 
Naive_Bayes = ifelse(nb_p >= 0.5, 1, 0)

#ploting the expected output as per Naive Bayes Classifier
ggplot(circle, aes(x = V1, y =V2, color = Naive_Bayes[,2])) + 
  geom_point(size = 2) + 
  ggtitle("Naive Bayes Classifier")


