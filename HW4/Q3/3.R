library(igraph)

# Construct Webgraph A
nodes_a <- data.frame(names = c("A", "B", "C", "D", "E", "F"))
node_connections_a <- data.frame(
  from = c("B", "B", "C", "D", "D", "E", "F"),
  to = c("C", "E", "A", "B", "E", "D", "C")
)
webgraph_a <-
  graph.data.frame(node_connections_a, directed = TRUE, vertices = nodes_a)

# Plot the graph
x11()
plot(webgraph_a, layout = layout_with_kk, vertex.color = "lightblue")
title("Webgraph A")

# Calculate PageRank vectors for different damping constants
pr_a1 <- page.rank(webgraph_a, damping = 0.05)
pr_a1$vector

pr_a2 <- page.rank(webgraph_a, damping = 0.25)
pr_a2$vector

pr_a3 <- page.rank(webgraph_a, damping = 0.5)
pr_a3$vector

pr_a4 <- page.rank(webgraph_a, damping = 0.75)
pr_a4$vector

pr_a5 <- page.rank(webgraph_a, damping = 0.95)
pr_a5$vector

# Construct Webgraph B
nodes_b <-
  data.frame(names = c("A", "B", "C", "D", "E", "F", "G", "H"))
node_connections_b <- data.frame(
  from = c("B", "C", "D", "E", "F", "G", "H"),
  to = c("A", "A", "B", "B", "C", "C", "C")
)
webgraph_b <-
  graph.data.frame(node_connections_b, directed = TRUE, vertices = nodes_b)

# Plot the graph
x11()
plot(webgraph_b, layout = layout_with_kk, vertex.color = "lightpink")
title("Webgraph B")

# Find PageRank vector for damping constant 0.15
pr_b <- page.rank(webgraph_b, damping = 0.15)
pr_b$vector