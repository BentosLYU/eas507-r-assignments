data(mtcars)

# Function to create the order component
ordercomp = function(x)
{
  Y = nrow(x) + 1
  ordercomp = rep(0, Y)
  ordercomp[1] = x[Y - 1, 1]
  ordercomp[2] = x[Y - 1, 2]
  loc = 2
  for (i in seq(Y - 2, 1))
  {
    for (j in seq(1, loc))
    {
      if (ordercomp[j] == i)
      {
        ordercomp[j] = x[i, 1]
        if (j == loc)
        {
          loc = loc + 1
          ordercomp[loc] = x[i, 2]
        } else
        {
          loc = loc + 1
          for (k in seq(loc, j + 2))
            ordercomp[k] = ordercomp[k - 1]
          ordercomp[j + 1] = x[i, 2]
        }
      }
    }
  }
  - ordercomp
}

# Function to perform hierarchial clustering
hc = function(d,
              method = c("single", "complete", "average"))
{
  if (!is.matrix(d))
    d = as.matrix(d)
  # Pick a clustering function:
  clustering_method = switch(match.arg(method),
                             single   = min,
                             complete = max)
  N = nrow(d)
  diag(d) = Inf
  n = -(1:N)                       # Cluster membership
  m = matrix(0, nrow = N - 1, ncol = 2)   # hclust merge object output
  h = rep(0, N - 1)                   # hclust height object output
  for (j in seq(1, N - 1))
  {
    # Find pair with minimum distance and corresponding indices
    h[j] = min(d)
    i = which(d - h[j] == 0, arr.ind = TRUE)
    i = i[1, , drop = FALSE]
    p = n[i]
    # Order each pair
    p = p[order(p)]
    m[j,] = p
    # Combine pair and previous groups into current group
    grp = c(i, which(n %in% n[i[1, n[i] > 0]]))
    n[grp] = j
    # Apply distance operation based on clustering method
    r = apply(d[i,], 2, clustering_method)
    # Select next minimum distance by modifying distance matrix
    d[min(i),] = d[, min(i)] = r
    d[min(i), min(i)]        = Inf
    d[max(i),] = d[, max(i)] = Inf
  }
  
  # Returning a structure which is same as that of the hclust object
  structure(
    list(
      merge = m,
      height = h,
      order = ordercomp(m),
      labels = rownames(d),
      method = method,
      call = match.call(),
      dist.method = "euclidean"
    ),
    class = "hclust"
  )
}

hc_complete = hclust(dist(mtcars), method = "complete")
plot(hc_complete, main = "Complete linkage of mtcars data")
hc_average = hclust(dist(mtcars), method = "average")
plot(hc_average, main = "Average linkage of mtcars data")
hc_single = hclust(dist(mtcars), method = "single")
plot(hc_single, main = "Single linkage of mtcars data")