#Load packages
library(recommenderlab)

#Load data
ratings = read.csv('Ratings.csv',sep='\t')

#Coerce data to matrix and subsequently realRatingMatrix
ratings_matrix = as.matrix(ratings)
dataset = as(ratings_matrix, "realRatingMatrix")
getRatingMatrix(dataset)

#Create UBCF model with Pearson correlation and mean centering
ubcf_model = Recommender(dataset, method = "UBCF", param=list(method = "pearson"))

#Predict ratings
ubcf_pred = predict(ubcf_model,newdata=dataset, type="ratings")
as(ubcf_pred,"matrix")

#Create IBCF model with Cosine similarity
ibcf_model = Recommender(dataset, method = "IBCF")

#Predict ratings
ibcf_pred = predict(ibcf_model,newdata=dataset, type="ratings")
as(ibcf_pred,"matrix")