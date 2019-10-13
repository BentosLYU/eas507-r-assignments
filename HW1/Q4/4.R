#Install and load packages
#install.packages("ISLR")
#install.packages("ElemStatLearn")
#install.packages("DataExplorer")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("arules")

library(ISLR)
library(ElemStatLearn)
library(DataExplorer)
library(rpart)
library(rpart.plot)
library(arules)

#Load data
dataset = as.data.frame(marketing)

#Add a class column
N = dim(dataset)[1]
Class = sample(c(0,1), N,replace = T)
dataset["Class"] = Class

#Remove observations with NA values
dataset <- dataset[complete.cases(dataset),]

#Categorise variables
dataset[["Income"]] <- ordered(cut(dataset[["Income"]], c(0,1,2,3,4,5,6,7,8,9)), labels = c("Less than $10,000","$10,000 to $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999","$30,000 to $39,999","$40,000 to $49,999", "$50,000 to $74,999", "$75,000 or more"))
dataset[["Sex"]] <- ordered(cut(dataset[["Sex"]], c(0,1,2)), labels = c("Male", "Female"))
dataset[["Marital"]] <- ordered(cut(dataset[["Marital"]], c(0,1,2,3,4,5)), labels = c("Married" ,"Living together, not married", "Divorced or separated","Widowed","Single, never married"))
dataset[["Age"]] <- ordered(cut(dataset[["Age"]], c(0,3,5,7)), labels = c("14 thru 34", "35 thru 54", "55 and over"))
dataset[["Edu"]] <- ordered(cut(dataset[["Edu"]], c(0,1,2,3,4,5,6)), labels = c("Grade 8 or less", "Grades 9 to 11","Graduated high school","1 to 3 years of college","College graduate","Grad Study"))
dataset[["Occupation"]] <- ordered(cut(dataset[["Occupation"]], c(0,1,2,3,4,5,6,7,8,9)), labels = c("Professional/Managerial","Sales Worker","Factory Worker/Laborer/Driver","Clerical/Service Worker","Homemaker", "Student, HS or College","Military", "Retired", "Unemployed"))
dataset[["Lived"]] <- ordered(cut(dataset[["Lived"]], c(0,1,2,3,4,5)), labels = c("Less than one year",  "One to three years",  "Four to six years",  "Seven to ten years","More than ten years"))
dataset[["Dual_Income"]] <- ordered(cut(dataset[["Dual_Income"]], c(0,1,2,3)), labels = c("Not Married", "Yes", "No"))
dataset[["Household"]] <- ordered(cut(dataset[["Household"]], c(0,1,2,3,4,5,6,7,8,9)), labels = c("One", "Two", "Three", "Four", "Five", "Six","Seven","Eight", "Nine or more"))
dataset[["Householdu18"]] <- ordered(cut(dataset[["Householdu18"]], c(-1,0,1,2,3,4,5,6,7,8,9)), labels = c("None", "One", "Two", "Three", "Four", "Five", "Six","Seven","Eight", "Nine or more"))
dataset[["Status"]] <- ordered(cut(dataset[["Status"]], c(0,1,2,3)), labels = c("Own", "Rent", "Live with Parents/Family"))
dataset[["Home_Type"]] <- ordered(cut(dataset[["Home_Type"]], c(0,1,2,3,4,5)), labels = c("House","Condominium","Apartment", "Mobile Home","Other"))
dataset[["Ethnic"]] <- ordered(cut(dataset[["Ethnic"]], c(0,1,2,3,4,5,6,7,8)), labels = c("American Indian","Asian", "Black","East Indian","Hispanic","Pacific Islander","White","Other"))
dataset[["Language"]] <- ordered(cut(dataset[["Language"]], c(0,1,2,3)), labels = c("English", "Spanish", "Other"))

#Creating training dataset and reference sample
dataset$Class = 1
train_data = dataset
ref_sample = dataset

for(i in 1:ncol(ref_sample)){
  ref_sample[,i] = sample(ref_sample[,i], nrow(ref_sample), replace=TRUE)
}
ref_sample$Class = 0

combined_df = rbind(ref_sample, train_data)

for(i in 1:ncol(combined_df)){
  combined_df[,i] = as.factor(as.character(combined_df[,i]))
}

#Creating a classification tree on the combined dataframe
tree_model <- rpart.control(maxdepth = 4, minsplit = 5, xval = 10, cp = 0)
tree <- rpart(Class~., data = combined_df, method = "class", control = tree_model)

#Plot tree
x11()
prp(tree, type = 4, extra = 101, leaf.round = 1, box.palette = "BuGn", nn=TRUE, main=" Classification Tree on combined dataset")

#Convert to a binary incidence matrix
bin_inc_matrix <- as(combined_df, "transactions")
summary(bin_inc_matrix)

#Plot the item frequency plot
x11()
itemFrequencyPlot(bin_inc_matrix, support = 0.15, cex.names = 0.8)
title("Item Frequency Plot")

# Apply Apriori algorithm
rules  <- apriori(bin_inc_matrix, parameter = list(support = 0.05, confidence = 0.75))

#Display summary of the rules
summary(rules)

#Rules associated with Class=1
inspect(head(subset(rules, subset=rhs %in% "Class=1"),5,by="confidence"))