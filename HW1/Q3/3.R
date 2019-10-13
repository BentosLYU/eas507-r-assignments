#Install and load packages
#install.packages("arules")
#install.packages("ElemStatLearn")
#install.packages("MASS")
library(arules)
library(ElemStatLearn)
library(MASS)

#Load data
dataset <- data.frame(Boston)

# Visualise data and create binary incidence matrix -----------------------

#Visualise data
par(mfrow=c(2,2))
hist(Boston$crim)
hist(Boston$zn)
hist(Boston$indus)
hist(Boston$chas)

par(mfrow=c(2,2))
hist(Boston$nox)
hist(Boston$rm)
hist(Boston$age)
hist(Boston$dis)

par(mfrow=c(2,2))
hist(Boston$rad)
hist(Boston$tax)
hist(Boston$ptratio)
hist(Boston$black)

par(mfrow=c(1,2))
hist(Boston$lstat)
hist(Boston$medv)

#Categorise variables
dataset[["crim"]] <- ordered(cut(dataset[["crim"]], c(0,10,20,90)), labels = c("Low", "Moderate", "High"))
dataset[["zn"]] <- ordered(cut(dataset[["zn"]], c(0,30,60,100)), labels = c("Small", "Medium", "Large"))
dataset[["indus"]] <- ordered(cut(dataset[["indus"]], c(0,10,20,30)), labels = c("Small", "Medium", "Large"))
dataset[["chas"]] <- NULL
dataset[["nox"]] <- ordered(cut(dataset[["nox"]], c(0.3,0.5,0.7,0.9)), labels = c("Low", "Medium", "High"))
dataset[["rm"]] <- ordered(cut(dataset[["rm"]], c(3,5,7,9)), labels = c("Low", "Medium", "High"))
dataset[["age"]] <- ordered(cut(dataset[["age"]], c(0,25,60,100)), labels = c("New", "Fairly Old", "Old"))
dataset[["dis"]] <- ordered(cut(dataset[["dis"]], c(3,5,7,9)), labels = c("Close", "Near", "Far"))
dataset[["rad"]] <- ordered(cut(dataset[["rad"]], c(0,10,24)), labels = c("Accessible", "Not Accessible"))
dataset[["tax"]] <- ordered(cut(dataset[["tax"]], c(0,300,500,900)), labels = c("Low", "Medium", "High"))
dataset[["ptratio"]] <- ordered(cut(dataset[["ptratio"]], c(0,16,20,24)), labels = c("Low", "Medium", "High"))
dataset[["black"]] <- ordered(cut(dataset[["black"]], c(0,200,400)), labels = c("Low", "High"))
dataset[["lstat"]] <- ordered(cut(dataset[["lstat"]], c(0,12,24,40)), labels = c("Low", "Medium", "High"))
dataset[["medv"]] <- ordered(cut(dataset[["medv"]], c(0,17,34,51)), labels = c("Low", "Medium", "High"))

#Create binary incidence matrix
bin_inc_matrix <- as(dataset, "transactions")
summary(bin_inc_matrix)

# Visualise data using itemFrequencyPlot() and apply Apriori algorithm ----

#Visualise data
par(mfrow=c(1,1))
itemFrequencyPlot(bin_inc_matrix, support = 0.15, cex.names = 0.8)
title("Item-Frequency Plot")

# Apply Apriori algorithm
rules  <- apriori(bin_inc_matrix, parameter = list(support = 0.05, confidence = 0.75))

# Display summary of the rules
summary(rules)

# Rules associated with areas close to the city and with low crime rate ---

inspect(head(subset(rules, subset=rhs %in% "crim=Low"),5,by="confidence"))
inspect(head(subset(rules, subset=rhs %in% "crim=Low"),5,by="lift"))
inspect(head(subset(rules, subset=rhs %in% "dis=Close"),5,by="confidence"))
inspect(head(subset(rules, subset=rhs %in% "dis=Close"),5,by="lift"))

# Rules associated with areas with low pupil-teacher ratio ----------------

inspect(head(subset(rules, subset=rhs %in% "ptratio=Low"),5,by="confidence"))
inspect(head(subset(rules, subset=rhs %in% "ptratio=Low"),5,by="lift"))

# Perform linear regression and observe results ---------------------------

#Split data into train and test
set.seed(1)
split = sample(1:nrow(Boston), size=nrow(Boston)*0.8)
train_data = Boston[split,]
test_data = Boston[-split,]

#Create model
linear_model <- lm(ptratio~.,train_data)
summary(linear_model)

#Make predictions
prediction = predict.lm(linear_model, newdata = test_data)

#Calculate error
error = mean((test_data$ptratio - prediction)^2)
error

#Plot significant variables
scatter.smooth(Boston$ptratio,Boston$zn)
scatter.smooth(Boston$ptratio,Boston$nox)
scatter.smooth(Boston$ptratio,Boston$rad)
scatter.smooth(Boston$ptratio,Boston$medv)