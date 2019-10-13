# Load packages
#install.packages("kohonen")
#install.packages("mlbench")
library(kohonen)
library(mlbench)

# Load data
data(BreastCancer)

# Remove data with NA values
bcancer.data <- BreastCancer[complete.cases(BreastCancer), ]

# Remove ID column
bcancer.data <- bcancer.data[,-1]

# Scale data
bcancer.scaled <- scale(as.numeric(unlist(bcancer.data)))

# Fit an SOM
set.seed(123)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
bcancer.som <- supersom(bcancer.scaled, grid = som_grid, rlen = 3000, mode = "batch")

codes <- bcancer.som$codes[[1]]

graphics.off()

# Plot SOM
x11()
plot(bcancer.som, main = "Cancer Data")

# Show mean distances to closest codebook vector during training
x11()
plot(bcancer.som, type = "changes", main = "Cancer Data")

# Show number of objects mapped
x11()
plot(bcancer.som, type = "count")

# Show where objects are mapped
x11()
plot(bcancer.som, type = "mapping")

coolBlueHotRed <- function(n, alpha = 1){
  rev(rainbow(n, end=4/6, alpha = alpha))
  }

# Show U-matrix
x11()
plot(bcancer.som, type = "dist.neighbours", palette.name = coolBlueHotRed)

# Find distance matrix and perform complete hierarchical clustering
d <- dist(codes, method = "euclidean")
hc_complete = hclust(d, method = "complete")

# Plot dendrogram
x11()
plot(hc_complete, main = "Hierarchial Clustering with Complete Linkage")

# Cluster data
tumor_clusters <- cutree(hc_complete, k=2) # or h = 2.5

# Plot SOM using above clusters

custom_colors <- c("green", "red")
custom_bgcolor <- custom_colors[tumor_clusters]

x11()
plot(bcancer.som, type = "mapping", col = "black", bgcol = custom_bgcolor)
add.cluster.boundaries(bcancer.som, tumor_clusters)
