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
#### SOM: Europe ####
EUR <- read.table(file.path(path, "final", "macroinvertebrate_EUR_bin.csv"), sep = ",", row.names = 1, header = TRUE)

str(EUR)

# --- Trait used voltinism
data <- EUR[4:ncol(EUR)]

data <- as.matrix(data)

# --- Set grid
grid_dim <- somgrid(10, 10, "hexagonal")

# --- Calculate map
set.seed(123)
som_model <- som(data, grid = grid_dim, rlen = 1800); beep(4)

# Plot training process
plot(som_model, type = "changes")

# --- Heat map of voltinism
str(som_model)
som_model$codes

png(filename = "~/Schreibtisch/Thesis/data/final/plots/SOM_EUR_locomotion_all.png")
par(mfrow = c(2,3))
plot(som_model, type = "property", property = som_model$codes[[1]][, 9], main = "Skater")
plot(som_model, type = "property", property = som_model$codes[[1]][, 10], main = "Swimmer")
plot(som_model, type = "property", property = som_model$codes[[1]][, 11], main = "Burrower")
plot(som_model, type = "property", property = som_model$codes[[1]][, 12], main = "Sprawler")
plot(som_model, type = "property", property = som_model$codes[[1]][, 13], main = "Sessil")
dev.off()

# --- Test with reproduction
# Reproduction only has 0 and 1 as information. Pilliere reproduceable?

# --- Heat map of voltinism
str(som_model)
som_model$codes

png(filename = "~/Schreibtisch/Thesis/data/final/plots/SOM_EUR_reproduction_all.png")
par(mfrow = c(1,3))
plot(som_model, type = "property", property = som_model$codes[[1]][, 33], main = "Aquatic Eggs")
plot(som_model, type = "property", property = som_model$codes[[1]][, 34], main = "Terrestrial Eggs")
plot(som_model, type = "property", property = som_model$codes[[1]][, 35], main = "Ovoviparity")
dev.off()


# --- Model with only locomotion trait
data <- select(EUR, grep("loc_", names(EUR)))

data <- as.matrix(data)

# --- Set grid
grid_dim <- somgrid(10, 10, "hexagonal")

# --- Calculate map
set.seed(123)
som_loc <- som(data, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_loc, type = "changes")

# --- Heat map of voltinism
str(som_loc)
som_loc$codes

png(filename = "~/Schreibtisch/Thesis/data/final/plots/SOM_EUR_locomotion_iso.png")
par(mfrow = c(2,3))
plot(som_loc, type = "property", property = som_loc$codes[[1]][, 1], main = "Skater")
plot(som_loc, type = "property", property = som_loc$codes[[1]][, 2], main = "Swimmer")
plot(som_loc, type = "property", property = som_loc$codes[[1]][, 3], main = "Burrower")
plot(som_loc, type = "property", property = som_loc$codes[[1]][, 4], main = "Sprawler")
plot(som_loc, type = "property", property = som_loc$codes[[1]][, 5], main = "Sessil")
dev.off()


# ------------------------------------------------------------- #
# ---- Test ----
EUR <- read.table(file.path(path, "final", "macroinvertebrate_EUR_int.csv"), sep = ",", row.names = 1, header = TRUE)

data <- select(EUR, voltinism)
levels(as.factor(data$voltinism))

row1 <- which(data$voltinism == 1)
row2 <- which(data$voltinism == 2)
row3 <- which(data$voltinism == 3)

# ---- SOM ---- #
matrix <- as.matrix(data)

# Set grid
grid_dim <- somgrid(10, 10, "hexagonal")

# Calculate map
set.seed(123)
som_model <- som(matrix, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_model, type = "changes")

# Plot modality maps
som_model$codes

plot(som_model, type = "count")

# ------------------------------------------------------------- #
# ---- Clustering SOM: Europe ---- 
data <- read.table(file.path(path, "final", "macroinvertebrate_EUR_int.csv"), sep = ",", row.names = 1, header = TRUE)

# ---- SOM ---- #
matrix <- as.matrix(data[5:ncol(data)])

# Set grid
grid_dim <- somgrid(10, 10, "hexagonal")

# Calculate map
set.seed(123)
som_model <- som(matrix, grid = grid_dim, rlen = 1500); beep(4)

# Plot training process
plot(som_model, type = "changes")

# ---- Hierarchical clustering ---- #
# Determine optimal number of cluster
set.seed(123)

# Compute and plot wss for k = 2 to k = 10
k.max <- 10

wss_dat <- matrix(unlist(som_model$codes), nrow = 100, ncol = 9, byrow = FALSE)

wss <- sapply(1:k.max, function(k){kmeans(wss_dat, k, nstart = 50, iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters",
     ylab = "Total within-clusters sum of squares")

# Number of clusters: 5

# --- Hierarchical clustering
som_cluster <- cutree(hclust(dist(unlist(som_model$codes))), 5)

# --- Plotting
# Create categories with top 10 orders and other-typologie
order_count <- as.data.frame(table(data$order))
order_count$Var1 <- as.character(order_count$Var1)
top10 <- order_count[rev(order(order_count$Freq)),"Var1"][1:10]

data$group <- ifelse(as.character(data$order) %in% top10, as.character(data$order), "Other")
data$group <- factor(data$group, levels=c(top10, "Other"))

png(filename = "~/Schreibtisch/Thesis/data/final/plots/SOM_EUR_cluster.png")
par(mfrow = c(1,1))
plot(som_model, type = "mapping", main = "Clustering SOM: Europe", bgcol = rainbow(5, s = 0.3)[som_cluster], pch = c(15:25)[data$group]) 
legend(x = "topleft", legend = levels(data$group), pch = c(15:25), cex = 0.7, bty = "n")
add.cluster.boundaries(som_model, som_cluster, lwd = 5)
dev.off()

# --- Assign cluster levels to individuals
cluster_assignment <- som_cluster[som_model$unit.classif]
data$cluster <- cluster_assignment


# ------------------------------------------------------------- #
# ---- Clustering SOM: North America ---- 
data <- read.table(file.path(path, "final", "macroinvertebrate_NAM_int.csv"), sep = ",", row.names = 1, header = TRUE)

# ---- SOM ---- #
matrix <- as.matrix(data[5:ncol(data)])

# Set grid
grid_dim <- somgrid(10, 10, "hexagonal")

# Calculate map
set.seed(123)
som_model <- som(matrix, grid = grid_dim, rlen = 2000); beep(4)

# Plot training process
plot(som_model, type = "changes")

# ---- Hierarchical clustering ---- #
# Determine optimal number of cluster
set.seed(123)

# Compute and plot wss for k = 2 to k = 10
k.max <- 10

wss_dat <- matrix(unlist(som_model$codes), nrow = 100, ncol = 9, byrow = FALSE)

wss <- sapply(1:k.max, function(k){kmeans(wss_dat, k, nstart = 50, iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters",
     ylab = "Total within-clusters sum of squares")

# Number of clusters: 4

# ---- Hierarchical clustering ---- #
som_cluster <- cutree(hclust(dist(unlist(som_model$codes))), 4)

# --- Plotting
# Create categories with top 10 orders and other-typologie
order_count <- as.data.frame(table(data$order))
order_count$Var1 <- as.character(order_count$Var1)
top10 <- order_count[rev(order(order_count$Freq)),"Var1"][1:10]

data$group <- ifelse(as.character(data$order) %in% top10, as.character(data$order), "Other")
data$group <- factor(data$group, levels=c(top10, "Other"))

png(filename = "~/Schreibtisch/Thesis/data/final/plots/SOM_NAM_cluster.png")
par(mfrow = c(1,1))
plot(som_model, type = "mapping", main = "Clustering SOM: North America", bgcol = rainbow(7, s = 0.3)[som_cluster], pch = c(15:25)[data$group]) 
legend(x = "topleft", legend = levels(data$group), pch = c(15:25), cex = 0.7, bty = "n")
add.cluster.boundaries(som_model, som_cluster, lwd = 5)
dev.off()

# --- Assign cluster levels to individuals
cluster_assignment <- som_cluster[som_model$unit.classif]
data$cluster <- cluster_assignment

# ------------------------------------------------------------- #
# ---- Clustering SOM: Australia ---- 
data <- read.table(file.path(path, "final", "macroinvertebrate_AUS_int.csv"), sep = ",", row.names = 1, header = TRUE)

# ---- SOM ---- #
matrix <- as.matrix(data[5:ncol(data)])

# Set grid
grid_dim <- somgrid(10, 10, "hexagonal")

# Calculate map
set.seed(123)
som_model <- som(matrix, grid = grid_dim, rlen = 1000); beep(4)

# Plot training process
plot(som_model, type = "changes")

# ---- Hierarchical clustering ---- #
# Determine optimal number of cluster
set.seed(123)

# Compute and plot wss for k = 2 to k = 10
k.max <- 10

wss_dat <- matrix(unlist(som_model$codes), nrow = 100, ncol = 9, byrow = FALSE)

wss <- sapply(1:k.max, function(k){kmeans(wss_dat, k, nstart = 50, iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters",
     ylab = "Total within-clusters sum of squares")

# Number of clusters: 3

# --- Hierarchical clustering
som_cluster <- cutree(hclust(dist(unlist(som_model$codes))), 3)

# --- Plotting
# Create categories with top 10 orders and other-typologie
order_count <- as.data.frame(table(data$order))
order_count$Var1 <- as.character(order_count$Var1)
top10 <- order_count[rev(order(order_count$Freq)),"Var1"][1:10]

data$group <- ifelse(as.character(data$order) %in% top10, as.character(data$order), "Other")
data$group <- factor(data$group, levels=c(top10, "Other"))

png(filename = "~/Schreibtisch/Thesis/data/final/plots/SOM_AUS_cluster.png")
par(mfrow = c(1,1))
plot(som_model, type = "mapping", main = "Clustering SOM: Australia", bgcol = rainbow(7, s = 0.3)[som_cluster], pch = c(15:25)[data$group]) 
legend(x = "topleft", legend = levels(data$group), pch = c(15:25), cex = 0.7, bty = "n")
add.cluster.boundaries(som_model, som_cluster, lwd = 5)
dev.off()

# --- Assign cluster levels to individuals
cluster_assignment <- som_cluster[som_model$unit.classif]
data$cluster <- cluster_assignment
