#Install and load packages

#install.packages('recommenderlab')
library(recommenderlab)

#Load and normalize data
data(MovieLense)
dataset_normalized=normalize(MovieLense)

#Prepare data using hold-out approach
eval_holdout <- evaluationScheme(dataset_normalized, method = "split", given = 3, train = 0.8, goodRating = 4)
eval_holdout

#Create a model based on User Based Collaborative Filtering to predict ratings
userbased_model <- Recommender(getData(eval_holdout, "train"), "UBCF", param = list(method = "cosine",nn = 50))
userbased_model

#Make predictions on ratings
pred_ub_holdout <- predict(userbased_model, getData(eval_holdout, "known"), type = "ratings")

#Calculate error
pred_error_holdout <- rbind(UBCF = calcPredictionAccuracy(pred_ub_holdout, getData(eval_holdout, "unknown")))
pred_error_holdout

#Evaluate UBCF model
model_error_holdout <- evaluate(x = eval_holdout, method = "UBCF", param = list(method = "cosine"), type = "ratings")
avg(model_error_holdout)

#Prepare data using k-fold approach (k=5)
eval_kfold <- evaluationScheme(MovieLense, method = "cross", k = 5, given = 3, goodRating = 4)
eval_kfold

#Create a k-fold model based on User Based Collaborative Filtering to predict ratings
userbased_model_kfold <- Recommender(getData(eval_kfold, "train"), "UBCF", param = list(method = "cosine",nn = 30))
userbased_model_kfold

#Make predictions on ratings
pred_ub_kfold <- predict(userbased_model_kfold, getData(eval_kfold, "known"), type = "ratings")

#Calculate prediction error
pred_error_kfold <- rbind(UBCF = calcPredictionAccuracy(pred_ub_kfold, getData(eval_kfold, "unknown")))
pred_error_kfold

#Evaluate the recommendation model
model_error_kfold <- evaluate(eval_kfold, method = "UBCF", n = c(1,3,5,10,15,20), param = list(method = "cosine"))
avg(model_error_kfold)

#Plot ROC curve and precision-recall curve
plot(model_error_kfold, annotate = TRUE)
title("ROC Curve")

plot(model_error_kfold, "prec/rec", annotate = TRUE)
title("Precision-recall")
