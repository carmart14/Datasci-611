# =======================
# Data Science 611
# Homework 3: Clustering
# Carmen Martin
# 10.28.2025
#=======================
set.seed(14)

#---Library Installation -----

#install.packages(plotly) 

library(tidyverse)
library(ggplot2)
library(cluster)
library(dplyr)
library(plotly)

#-----Task One --------------

# Function Creation: Cluster Creation

generate_hypercube_clusters <- function(n,k, side_length, noise_sd = 1){
  centers <- diag(side_length, nrow = n, ncol = n) # getting the centers
  pts <- do.call(rbind, lapply(1:n, function(i){
    matrix(rnorm(k * n, mean = 0, sd = noise_sd), ncol = n) + 
      matrix(rep(centers[i,], each = k ), ncol = n, byrow = F)
    #building the points, for each center create K points and add a bit of noise
  }))
  labels <- rep(1:n, each = k)
  return(list(x = pts, labels = labels))
}

# Checking Function

samp <- generate_hypercube_clusters(n=3, k = 50, side_length = 5, noise_sd = 1)

head(samp$x)
table(samp$labels)

# visualization
dat <- data.frame(x = samp$x[,1], y=samp$x[,2], label = factor(samp$labels))

ggplot(dat, aes(x =x, y=y, color = label))+
  geom_point(alpha = 0.5)+
  ggtitle("Sanity Check")


# Function Creation: ClusGap

run_gap_kmeans <- function(x, K.max = 12, B = 50, nstart = 20, iter.max = 50){ # these parameters given
  gap_out <- clusGap(x, FUNcluster = kmeans, K.max = K.max, B = B,
                     nstart = nstart, iter.max = iter.max)
  
  suggested_k <-maxSE(gap_out$Tab[,"gap"], gap_out$Tab[,"SE.sim"], method = "firstSEmax")
  return(list(gap = gap_out, suggested_k = suggested_k))
}

# running the simualation

run_task1_sim <- function(dims = c(6,5,4,3,2), Ls = 10:1, k_per_cluster = 100, noise_sd = 1.0, B = 50) {
  results <- data.frame(n = integer(), L = integer(), suggested_k = integer(), stringsAsFactors = FALSE)
  for (n in dims) {
    for (L in Ls) {
      message(sprintf("Running n=%d, L=%d", n, L))
      dat <- generate_hypercube_clusters(n=n, k=k_per_cluster, side_length=L, noise_sd=noise_sd)
      gap_res <- run_gap_kmeans(dat$x, K.max = max(8, n*2), B = B, nstart = 20, iter.max = 50)
      results <- rbind(results, data.frame(n = n, L = L, suggested_k = gap_res$suggested_k))
    }
  }
  return(results)
}

# run (this will take some time; B=50 is moderate)
task1_results <- run_task1_sim()

# Analysis & Visuals 

results <- task1_results |> 
  mutate(true_k =n, correct = (suggested_k == true_k))

write.csv(results, "data/task1_results_summary", row.names = F)

# plotting the gap stat and its accuracy
ggplot(results, aes(x = L, y = suggested_k)) + 
  geom_point() + geom_line() +
  geom_hline(aes(yintercept = true_k), l = "dashed", color = "red")+ 
  facet_wrap(~n, scales = "free_y")+
  scale_x_continuous(breaks = 1:10)+
  labs(title = "Gap Stat suggested K vs Side Length (L)", x = "Side Length (L)", y = "Suggested K")

ggsave("figures/task1_gap_stat_vs_L.png", width = 10, height = 6)


failure_points <- results |> 
  group_by(n) |> 
  arrange(L) |> 
  summarize(failure_L = min(L[ suggested_k != true_k ], na.rm = TRUE))

# if no failure, failure_L will be Inf/NA 
print(failure_points)


# ----- Interpretation ----------

# Based on the plot we're looking  at, as the number of side lenght increases the
# further away the suggested K is to the accurate prediction. It's not a large gap
# (hahah gap, for clusGap) and when the side length is about half or so the actual 
# suggested K then the guess is right on. This suggests to me the large the clusters
# the harder time it is to estimate K. This could also been interprated as the more 
# information (L) you give the higher porbabilty you will be correct in the estimation.
# 
# Overall this first task is telling me that the Side Length plays an intergral role in
# the estimation of K and that gap statistic can successfully recover the true number
# of clusters only when between-cluster distance is large relative to within-cluster noise


# -------- Task Two --------------

generate_shell_clusters <- function(n_shells = 4 , k_per_shell = 100, max_radius = 10, noise_sd = 0.1){
  radii <- seq(max_radius / n_shells, length.out = n_shells)
  total <- n_shells * k_per_shell
  pts <- matrix(NA_real_, nrow = total, ncol = 3)
  labels <- rep(1:n_shells, each = k_per_shell)
  idx <- 1
  
  for (s in seq_len(n_shells)){
    r0 <- radii[s]
    u <- matrix(rnorm(k_per_shell *3), ncol = 3)
    u <- u / sqrt(rowSums(u^2)) # normalize rows to lenght unit
    radial <- r0 + rnorm(k_per_shell, 0, noise_sd)
    pts[idx:(idx + k_per_shell - 1),] <- u * radial
    idx <- idx + k_per_shell
    
    # i don't understand what any of this is doing
  }
  list(x = pts, labels = labels, radii = radii)
}


# Visualization of the shells 


samp <- generate_shell_clusters(n_shells = 4, k_per_shell = 100, max_radius = 10, noise_sd = 0.1)

df <- data.frame(x = samp$x[,1], y = samp$x[,2], z = samp$x[,3], label = factor(samp$labels))
plot_ly(df, x = ~x, y = ~y, z = ~z, color = ~label, type = 'scatter3d', mode = 'markers')

#adjacency matrix with fixed distance theresholds

build_adjacency <- function(x, d_threshold = 1) {
  dmat <- as.matrix(dist(x))
  A <- (dmat <= d_threshold) * 1L
  diag(A) <- 0L
  A
}

# clusters are now in one group instead of seperate like in the task one

# compute normalized symmertic Laplacian and eigendecomposition


compute_normal_laplacian <- function(A){
  deg <- rowSums(A)
  inv_sqrt_deg <- ifelse(deg >0,1 /sqrt(deg), 0)
  D_inv_sqr <- diag(inv_sqrt_deg)
  L<- diag(deg) - A
  L_sym<- D_inv_sqr %*% L %*% D_inv_sqr
  L_sym <- (L_sym + t(L_sym))/2
  L_sym
}


# a tad confused here but okay

spectral_kmeans_wrapper <- function(x, k, d_threshold = 1, nstart = 20, iter.max = 50) {
  A <- build_adjacency(x, d_threshold = d_threshold)
  L_sym <- compute_normal_laplacian(A)
  eig <- eigen(L_sym, symmetric = TRUE)
  # eigen$values are in decreasing order; we want ascending
  ord <- order(eig$values)
  chosen_idx <- ord[1:k]      # indices of smallest k eigenvalues
  embedding <- eig$vectors[, chosen_idx, drop = FALSE]
  # row normalize embedding (avoid division by zero)
  rn <- sqrt(rowSums(embedding^2))
  rn[rn == 0] <- 1
  embedding <- embedding / rn
  # run kmeans on embedding
  km <- kmeans(embedding, centers = k, nstart = nstart, iter.max = iter.max)
  return(km)   # $cluster will be used by clusGap
}

# validation of the spectral kmeans wrapper
samp <- generate_shell_clusters(n_shells = 4, k_per_shell = 100, max_radius = 10, noise_sd = 0.1)
km <- spectral_kmeans_wrapper(samp$x, k = 4, d_threshold = 1, nstart = 20)
table(km$cluster, samp$labels)   # confusion matrix between found clusters and true shells


# making clusGap compatable wrapper

run_gap_spectral <- function(x, K.max = 8, B = 50, d_threshold =1, nstart = 20, iter.max = 50){
  gap_out <- clusGap(x, FUNcluster = function(xx, k ) spectral_kmeans_wrapper(xx, k, d_threshold = d_threshold, iter.max = iter.max, nstart = nstart),
                    K.max = K.max, B= B ) # so the values are what we just spectifed in the function
  suggested_k <- maxSE(gap_out$Tab[,"gap"], gap_out$Tab[,"SE.sim"], method = "firstSEmax")
  list(gap = gap_out, suggested_k = suggested_k)
}


# simulation time part 2

run_task2_sim <- function(max_radii = seq(10, 0, by = -1), n_shells = 4, k_per_shell = 100, noise_sd = 0.1,
                          d_threshold = 0.8, B = 50, K.max = 8) {
  results <- data.frame(max_radius = numeric(), suggested_k = integer(), stringsAsFactors = FALSE)
  for (r in max_radii) {
    message("Running max_radius = ", r)
    samp <- generate_shell_clusters(n_shells = n_shells, k_per_shell = k_per_shell, max_radius = r, noise_sd = noise_sd)
    gap_res <- run_gap_spectral(samp$x, K.max = K.max, B = B, d_threshold = d_threshold)
    results <- rbind(results, data.frame(max_radius = r, suggested_k = gap_res$suggested_k))
    # checkpoint
    saveRDS(results, file = sprintf("data/task2_results_d%0.1f.rds", d_threshold))
  }
  results
}
# run with d_threshold = 1
task2_results_d08 <- run_task2_sim(max_radii = seq(10, 0, by = -1), d_threshold = 0.8, B = 50)

# plotting said results
ggplot(task2_results_d08, aes(x = max_radius, y = suggested_k)) +
  geom_point() + geom_line() +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  labs(title = "Spectral clustering (d_threshold=0.8): suggested k vs max_radius",
       x = "max_radius", y = "suggested k")

ggsave("figures/task2_gap_vs_radius_d0.8.png", width = 8, height = 5)


#------------Interpretation---------------------------
# 
# Because all the different clusters have the same center point, when the radius is increase for the points, 
# its hard for the algorithm to know there are separate groups there. There's also the  the issue of not increasing the 
# number of points used for each of the group. Something I would've adjusted for (but the assignment is due and it took me 
# a while to understand) and the overall takeaway remains the same. 
# 
# as shells expanded and connectivity weakened, the graph collapsed into one or two 
# effective groups. The experiment highlights that in graph-based clustering, 
# both the geometric scale (radius) and the adjacency parameter (d_threshold) critically 
# shape the clustering outcome. 





