
# Load csv ----------------------------------------------------------------

gene_data = read.csv(file = "Ch10Ex11.csv", header = F)

# Hierarchial clustering using correlation based distance -----------------

d <- as.dist(1 - cor(gene_data))

# Cluster using Single Linkage and plot the dendrogram
hc_single = hclust(d, method = "single")
plot(hc_single, main = "Hierarchial Clustering with Single Linkage", cex = 0.8)

# Cluster using Average Linkage and plot the dendrogram
hc_average = hclust(d, method = "average")
plot(hc_average, main = "Hierarchial Clustering with Average Linkage", cex = 0.8)

# Cluster using Complete Linkage and plot the dendrogram
hc_complete = hclust(d, method = "complete")
plot(hc_complete, main = "Hierarchial Clustering with Complete Linkage", cex = 0.8)


# Finding genes that differ the most --------------------------------------

# Using PCA to find the genes
genes.pca <- prcomp(t(gene_data),center = TRUE,scale = TRUE)
summary(genes.pca)

# Find PVE
genes_pve <- genes.pca$sdev^2/sum(genes.pca$sdev^2)
plot(genes_pve, xlab = "Principal Component", ylab = "PVE", main = "PVE of Principal Components", type = 'b')

# Compute factor loadings
factor_loadings <- apply(genes.pca$rotation, 1, sum)
gene_indices <- order(abs(factor_loadings), decreasing = TRUE)
gene_indices[1:20]
