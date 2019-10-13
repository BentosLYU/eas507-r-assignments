# Load packages
library(cluster)
library(caret)
library(NbClust)
library(factoextra)
library(fpc)

# Import data
set.seed(123)
load(file = "primate.scapulae.rda")

# Data preprocessing

# Replace NA values with mean
primate.scapulae$gamma[is.na(primate.scapulae$gamma)] = round(mean(primate.scapulae$gamma, na.rm=TRUE))

#Remove class column (as it contains characters)
class_names <- primate.scapulae[,10]
primate.scapulae <- primate.scapulae[,-10]

# Scale data
primate.scapulae <- sapply(primate.scapulae, as.numeric)
data_scaled <- scale(primate.scapulae)

# a) Hierarchial clustering -----------------------------------------------

#Find distance matrix
d <- dist(primate.scapulae, method = "euclidean")

# Cluster using Single Linkage and plot the dendrogram
hc_single = hclust(d, method = "single")
plot(hc_single, main = "Hierarchial Clustering with Single Linkage", cex = 0.7, labels = class_names)

# Find misclassification rate
hc_single_5 <- cutree(hc_single, k = 5)
confusionMatrix(as.factor(hc_single_5),as.factor(primate.scapulae[,10]))

# Cluster using Average Linkage and plot the dendrogram
hc_average = hclust(d, method = "average")
plot(hc_average, main = "Hierarchial Clustering with Average Linkage", cex = 0.7, labels = class_names)

# Find misclassification rate
hc_average_5 <- cutree(hc_average, k = 5)
confusionMatrix(as.factor(hc_average_5),as.factor(primate.scapulae[,10]))

# Cluster using Complete Linkage and plot the dendrogram
hc_complete = hclust(d, method = "complete")
plot(hc_complete, main = "Hierarchial Clustering with Complete Linkage", cex = 0.7, labels = class_names)

# Find misclassification rate
hc_complete_5 <- cutree(hc_complete, k = 5)
confusionMatrix(as.factor(hc_complete_5),as.factor(primate.scapulae[,10]))


# b) k-means clustering ---------------------------------------------------

km <- kmeans(data_scaled, centers = 5, nstart = 10)
confusionMatrix(as.factor(km$cluster),as.factor(primate.scapulae[,10]))

# Show the clustering based on the first two principal components
fviz_cluster(km, data = data_scaled)

# Calculate gap statistic
gap_stat <- clusGap(data_scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
