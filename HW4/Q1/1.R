library(bnlearn)
library(Rgraphviz)
library(corrplot)
library(ggplot2)
library(factoextra)
library(magrittr)
library(dplyr)
library(igraph)
library(gRain)
library(gRbase)
library(ggm)
library(gRim)

# Load and scale data
parkinsons_data <- read.csv("parkinsons_updrs.data", sep = ",")

# Plot correlations
corrplot(cor(parkinsons_data))

# Keep a subset of variables
cols_to_keep <-
  c(
    'age',
    'sex',
    'test_time',
    'Jitter...',
    'Shimmer',
    'RPDE',
    'DFA',
    'PPE',
    'motor_UPDRS',
    'total_UPDRS'
  )
parkinsons_data <-
  parkinsons_data[, (names(parkinsons_data) %in% cols_to_keep)]

# Convert integer data to numeric and factor data
parkinsons_data$age <- as.numeric(parkinsons_data$age)
parkinsons_data$sex = as.numeric(parkinsons_data$sex)

# Perform hierarchial clustering
d <- dist(parkinsons_data)
hc_complete = hclust(d, method = "complete")
x11()
plot(hc_complete, main = "Hierarchial Clustering with Complete Linkage", cex =
       0.8)

hc_clustered <- cutree(hc_complete, k = 2)

#Perform PCA and get the first two principal components
parkinsons_pca <- prcomp(parkinsons_data)
pc1 <- parkinsons_pca$x[, 1]
pc2 <- parkinsons_pca$x[, 2]

# Visualise capturing of total_UPDRS and motor_UPDRS
total_UPDRS <-
  parkinsons_data$total_UPDRS %>% cut(breaks = c(0, 10, 20, 30, 40, 50, 60))
motor_UPDRS <-
  parkinsons_data$motor_UPDRS %>% cut(breaks = c(0, 10, 20, 30, 40))

x11()
ggplot(parkinsons_data,
       aes(pc1, pc2, color = parkinsons_data$total_UPDRS)) + geom_point() + geom_text(aes(label =
                                                                                            hc_clustered))
x11()
ggplot(parkinsons_data, aes(pc1, pc2, color = as.factor(hc_clustered))) + geom_point()

x11()
ggplot(parkinsons_data,
       aes(pc1, pc2, color = parkinsons_data$motor_UPDRS)) + geom_point() + geom_text(aes(label =
                                                                                            hc_clustered))
x11()
ggplot(parkinsons_data, aes(pc1, pc2, color = as.factor(hc_clustered))) + geom_point()

# K-means clustering
km <- kmeans(d, centers = 2, iter.max = 10)
x11()
fviz_cluster(
  km,
  data = parkinsons_pca$x,
  geom = "point",
  stand = FALSE,
  frame.type = "norm"
) +
  xlab("PC1") +
  ylab("PC2")

# Check capture of total_UPDRS and motor_UPDRS
table(km$cluster, total_UPDRS)
table(km$cluster, motor_UPDRS)

# Fitting a Bayesian network with total_UPDRS
parkinsons_data <-
  parkinsons_data[, -which(names(parkinsons_data) %in% c("motor_UPDRS"))]
parkinsons_data <- as.data.frame(parkinsons_data)

# Use hill climbing algorithm to learn the data and plot the network
parkinsons_network <- hc(parkinsons_data)
bayesian_park <- amat(parkinsons_network)
bayesian_park <- layoutGraph(as.graphNEL(parkinsons_network))
nodeRenderInfo(bayesian_park) <- list(fontsize = 25)
x11()
renderGraph(bayesian_park)
title(main = "Parkinson Bayesian Network")

# Force total_UPDRS to be at the bottom of the network
park_data_f <- rep(1, 9)
park_data_f[4] <- 2
park_mat_f <- matrix(0, 9, 9)

colnames(park_mat_f) <- names(parkinsons_data)
rownames(park_mat_f) <- names(parkinsons_data)
park_mat_f[park_data_f == 2, park_data_f < 2] <- 1
connections <- data.frame(get.edgelist(as(park_mat_f, "igraph")))
names(connections) <- c("From", "To")

# Plot the new graph
parkinsons_network_forced <-
  hc(parkinsons_data, blacklist = connections)
bayesian_park_forced <- amat(parkinsons_network_forced)
bayesian_park_forced <-
  layoutGraph(as.graphNEL(parkinsons_network_forced))
nodeRenderInfo(bayesian_park_forced) <- list(fontsize = 25)
x11()
renderGraph(bayesian_park_forced)
title(main = "Parkinson Bayesian Network")

# Convert continuous variables to categorical variables
parkinsons_data$age <-
  cut(parkinsons_data$age, breaks = c(30, 50, 70, 100))
parkinsons_data$sex <-  as.factor(parkinsons_data$sex)
parkinsons_data$test_time <-
  cut(parkinsons_data$test_time, breaks = c(-5, 50, 100, 150, 220))
parkinsons_data$total_UPDRS <-
  cut(parkinsons_data$total_UPDRS, breaks = c(0, 10, 20, 30, 40, 50, 60))
parkinsons_data$Jitter... <-
  cut(parkinsons_data$Jitter..., breaks = c(0, 0.006, 1))
parkinsons_data$Shimmer <-
  cut(parkinsons_data$Shimmer, breaks = c(0, 0.03, 1))
parkinsons_data$RPDE <-
  cut(parkinsons_data$RPDE, breaks = c(0, 0.55, 1))
parkinsons_data$DFA <-
  cut(parkinsons_data$DFA, breaks = c(0.5, 0.6, 0.7, 0.87))
parkinsons_data$PPE <-
  cut(parkinsons_data$PPE, breaks = c(0, 0.25, 1))

# Compile conditional probability tables
parkinsons_cpt = compileCPT(list(
  xtabs(~ age, parkinsons_data),
  xtabs(~ Jitter..., parkinsons_data),
  xtabs(~ test_time, parkinsons_data),
  xtabs(~ sex + age + Jitter..., parkinsons_data),
  xtabs(~ PPE + age + sex + Jitter..., parkinsons_data),
  xtabs(~ Shimmer + age + sex + PPE + Jitter..., parkinsons_data),
  xtabs(~ DFA + age + sex + PPE + Shimmer, parkinsons_data),
  xtabs(~ RPDE + sex + PPE + Shimmer + test_time + Jitter..., parkinsons_data),
  xtabs(
    ~ total_UPDRS + age + sex + DFA + PPE + RPDE + test_time,
    parkinsons_data
  )
))

parkinsons_grain <- grain(parkinsons_cpt)

park_grain_ev <-
  setFinding(parkinsons_grain,
             nodes = c("total_UPDRS"),
             states = c("(50,60]"))

# Find marginal probability with and without evidence for high total_UPDRS score
querygrain(park_grain_ev,
           nodes = c("Jitter..."),
           type = "marginal")
querygrain(parkinsons_grain,
           nodes = c("Jitter..."),
           type = "marginal")