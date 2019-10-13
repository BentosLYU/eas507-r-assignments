library(bnlearn)
library(Rgraphviz)

# Load data, and remove unrelated variables and missing values
titanic_data <- read.csv('train.csv')
titanic_data <-
  titanic_data[,!(names(titanic_data) %in% c('PassengerId', 'Name', 'Ticket', 'Fare', 'Cabin'))]
titanic_data <- titanic_data[complete.cases(titanic_data), ]

# Convert data to factors
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass = as.factor(titanic_data$Pclass)
titanic_data$Age[titanic_data$Age > 18] <- 'Adult'
titanic_data$Age[titanic_data$Age != 'Adult'] <- 'Child'
titanic_data$Age = as.factor(titanic_data$Age)
titanic_data$SibSp <- as.factor(titanic_data$SibSp)
titanic_data$Parch <- as.factor(titanic_data$Parch)

# Learn the data and create a graph
titanic_data <- data.frame(titanic_data)
titanic_dag <- hc(titanic_data)
plot(titanic_dag)
title("Optimal Bayesian Network of Titanic Data")

# Fit the network to the data
fitted_bn <- bn.fit(titanic_dag, data = titanic_data)

# Plot the graph
graphviz.plot(fitted_bn)

# Probability of survival of women and children
cpquery(fitted_bn, (Survived == 1), (Sex == 'female'))
cpquery(fitted_bn, (Survived == 1), (Age == 'Child'))

# Probability of survival of a 1st class female passenger (Rose)
cpquery(fitted_bn, (Survived == 1), (Sex == 'female' & Pclass == 1))

# Probability of un-survival of a 3rd class male passenger (Jack)
cpquery(fitted_bn, (Survived == 0), (Sex == 'male' & Pclass == 3))
