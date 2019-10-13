#Load packages
library(cluster)
library(ISLR)

#Load data
data(USArrests)

# Cluster states with complete linkage and Euclidean distance -------------

# Find distance matrix
d <- dist(USArrests, method = "euclidean")
hc_complete = hclust(d, method ="complete")
plot(hc_complete, main ="Hierarchial Clustering with Complete Linkage", cex=0.8)

# Create 3 distinct clusters of states ------------------------------------

clustered_states <- cutree(hc_complete, h=150)
clustered_states[as.logical(clustered_states==1)]
clustered_states[as.logical(clustered_states==2)]
clustered_states[as.logical(clustered_states==3)]

# Hierarchial Clustering of data with standard deviation 1 ----------------

data_scaled = scale(USArrests)
d_scaled <- dist(data_scaled, method = "euclidean")
hc_complete_scaled = hclust(d_scaled, method ="complete")
plot(hc_complete_scaled, main =" Hierarchical Clustering of Scaled Data (with Complete Linkage)", cex=0.8, cex.main=0.8)

# Create 3 distinct clusters of states (scaled data) ----------------------

clustered_states_scaled <- cutree(hc_complete_scaled, 3)
clustered_states_scaled

# Compare the clusters
table(clustered_states, clustered_states_scaled)
