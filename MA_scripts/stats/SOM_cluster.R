#####################################
#### Stats: Self Organizing Maps ####
#####################################

#### Automated Cluster SOMs ####

# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"
plot <- "~/Schreibtisch/Thesis/final_paper/Figures/results"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(kohonen)
library(RColorBrewer)
library(beepr)


# --------------------------------------------------------------------------------------------------------------- #
#### Preparation ####
source("~/Schreibtisch/Thesis/scripts/stats/prepDat.R")


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data #### 
ALL <- read.table(file.path(path, "final", "macroinvertebrate_ALL_int_final.csv"), sep = ",", row.names = 1, header = TRUE)
ALL[is.na(ALL)] <- 0

# --------------------------------------------------------------------------------------------------------------- #
#### Plot Setup
5*sqrt(length(ALL[, 4:ncol(ALL)]))
# 14 cells are sufficient

# --- Set grid
grid_dim <- somgrid(4, 4, "hexagonal")


# --------------------------------------------------------------------------------------------------------------- #
#### Clustering SOM: Europe #### 
# --- Saving plots
setwd("~/Schreibtisch/Thesis/final_paper/Figures/results/SOM/EUR")

# --- Extract data for Europe
data <- ALL[grepl("EUR", ALL$region), ]

# ---- SOM ---- #
matrix <- as.matrix(data[4:ncol(data)])

# Calculate map
set.seed(123)
som_model <- som(matrix, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_model, type = "changes")

# ---- Hierarchical clustering ---- #
# Determine optimal number of cluster
set.seed(123)

# Compute and plot wss for k = 2 to k = 10
k.max <- 10
codes <- getCodes(som_model)

wss_dat <- matrix(codes, nrow = nrow(codes), ncol = ncol(codes), byrow = FALSE)

wss <- sapply(1:k.max, function(k){kmeans(wss_dat, k, nstart = 50, iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters",
     ylab = "Total within-clusters sum of squares")

# Number of clusters: 4

# --- Hierarchical clustering
set.seed(123)
som_cluster <- cutree(hclust(dist(codes)), 4)

# ---- Plotting ---- #
pdf(file = "SOM_cluster.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(0, 8, 0, 1))
plot(som_model, type = "mapping", main = " ", bgcol = rainbow(5, s = 0.3)[som_cluster],
     pch = c(0, 2, 3, 16, 18), cex = 1.5)
add.cluster.boundaries(som_model, som_cluster, lwd = 4)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 8), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(x = "left", legend = unique(data$order), xpd = TRUE, inset = c(0, 0), pch = c(0, 2, 3, 16, 18), bty = "n", cex = 2)
dev.off()

# Reset graphical parameters
dev.off()


# --------------------------------------------------------------------------------------------------------------- #
#### Clustering SOM: North America #### 
# --- Saving plots
setwd("~/Schreibtisch/Thesis/final_paper/Figures/results/SOM/NAM")

# --- Extract data for North America
data <- ALL[grep("NAM", ALL$region), ]

# ---- SOM ---- #
matrix <- as.matrix(data[4:ncol(data)])

# Calculate map
set.seed(123)
som_model <- som(matrix, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_model, type = "changes")

# ---- Hierarchical clustering ---- #
# Determine optimal number of cluster
set.seed(123)

# Compute and plot wss for k = 2 to k = 10
k.max <- 10
codes <- getCodes(som_model)

wss_dat <- matrix(codes, nrow = nrow(codes), ncol = ncol(codes), byrow = FALSE)

wss <- sapply(1:k.max, function(k){kmeans(wss_dat, k, nstart = 50, iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters",
     ylab = "Total within-clusters sum of squares")

# Number of clusters: 5

# ---- Hierarchical clustering ---- #
som_cluster <- cutree(hclust(dist(codes)), 3)

# ---- Plotting ---- #
pdf(file = "SOM_cluster.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(0, 8, 0, 1))
plot(som_model, type = "mapping", main = " ", bgcol = rainbow(5, s = 0.3)[som_cluster],
     pch = c(0, 2, 3, 16, 18), cex = 1.5)
add.cluster.boundaries(som_model, som_cluster, lwd = 4)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 8), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(x = "left", legend = unique(data$order), xpd = TRUE, inset = c(0, 0), pch = c(0, 2, 3, 16, 18), bty = "n", cex = 2)
dev.off()

# Reset graphical parameters
dev.off()


# --------------------------------------------------------------------------------------------------------------- #
#### Clustering SOM: Australia #### 
# --- Saving plots
setwd("~/Schreibtisch/Thesis/final_paper/Figures/results/SOM/AUS")

# --- Extract data for Europe
data <- ALL[grep("AUS", ALL$region), ]

# ---- SOM ---- #
matrix <- as.matrix(data[4:ncol(data)])

# Calculate map
set.seed(1234)
som_model <- som(matrix, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_model, type = "changes")

# ---- Hierarchical clustering ---- #
# Determine optimal number of cluster
set.seed(123)

# Compute and plot wss for k = 2 to k = 10
codes <- getCodes(som_model)

wss_dat <- matrix(codes, nrow = nrow(codes), ncol = ncol(codes), byrow = FALSE)

wss <- sapply(1:k.max, function(k){kmeans(wss_dat, k, nstart = 50, iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters",
     ylab = "Total within-clusters sum of squares")

# Number of clusters: 3

# --- Hierarchical clustering
som_cluster <- cutree(hclust(dist(codes)), 4)

# ---- Plotting ---- #
pdf(file = "SOM_cluster.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(0, 8, 0, 1))
plot(som_model, type = "mapping", main = " ", bgcol = rainbow(5, s = 0.3)[som_cluster],
     pch = c(0, 2, 3, 16, 18), cex = 1.5)
add.cluster.boundaries(som_model, som_cluster, lwd = 4)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 8), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(x = "left", legend = unique(data$order), xpd = TRUE, inset = c(0, 0), pch = c(0, 2, 3, 16, 18), bty = "n", cex = 2)
dev.off()

# Reset graphical parameters
dev.off()
