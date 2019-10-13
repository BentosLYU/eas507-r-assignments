library(rcompanion)
library(gRbase)
library(gRim)
library(gRain)
library(glasso)
library(graph)
library(kohonen)

#Load data
data(state)
us_data <- data.frame(state.x77)

# Data preprocessing
us_data$Population <- sqrt(us_data$Population)
us_data$Income <- sqrt(us_data$Income)
us_data$Life.Exp <- sqrt(us_data$Life.Exp)
us_data$Area <- log2(us_data$Area)

# Compute partial correlations
cov_weights <- cov.wt(us_data, method = "ML")
p_corr_us_data <- cov2pcor(cov_weights$cov)
heatmap(p_corr_us_data)

S <- cov_weights$cov

# Estimate GGM for different rho values
rhos <- c(1, 2, 4, 8, 10)
m0.lasso <- glassopath(S, rho = rhos)
graphics.off()
for (i in 1:length(rhos)) {
  my.edges <- m0.lasso$wi[, , i] != 0
  diag(my.edges) <- FALSE
  a_mat <- my.edges
  colnames(a_mat) <- names(us_data)
  g.lasso <- as(my.edges, "graphNEL") # convert for plotting
  nodes(g.lasso) <- names(us_data)
  glasso.net <- cmod(g.lasso, data = us_data)
  
  x11()
  plot(glasso.net)
  title(paste('GGM for rho = ', rhos[i]))
}

# SOM
us_data.scaled <- scale(us_data)
set.seed(123)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
us_data.som <- som(us_data.scaled, grid = som_grid, rlen = 3000)

# Plot SOM
x11()
plot(us_data.som)

# Property plot
x11()
plot(us_data.som,
     type = "property",
     property = getCodes(us_data.som, 1)[, 1])