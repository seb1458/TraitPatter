#####################################
#### Stats: Self Organizing Maps ####
#####################################

# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"
plot <- "~/Schreibtisch/Thesis/data/final/plots"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(kohonen)
library(RColorBrewer)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
trait_som <- read.table(file.path(path, "final", "macroinvertebrate_ALL_mod.csv"), sep = ",", row.names = 1, header = TRUE)
names(trait_som)

# --------------------------------------------------------------------------------------------------------------- #
#### Prepare Data ####

# Convert modalities to numbers
som_data <- data.frame(
  family = trait_som$family,
  region = trait_som$region,
  ph = as.numeric(trait_som$ph),
  feed_mode = as.numeric(trait_som$feed_mode),
  locomotion = as.numeric(trait_som$locomotion),
  respiration = as.numeric(trait_som$respiration),
  drift = as.numeric(trait_som$drift),
  size = as.numeric(trait_som$size),
  aquatic_stages = as.numeric(trait_som$aquatic_stages)
)


# --------------------------------------------------------------------------------------------------------------- #
#### Self Organizing Map ####

# --- Variables used are the traits
som_trait <- scale(som_data[, -c(1:3)])

# --- Set grid
grid_dim <- somgrid(xdim = 10, ydim = 10, topo = "rectangular")

# --- Calculate map
set.seed(123)
map <- som(som_trait,
           grid = grid_dim,
           alpha = c(0.05, 0.01),
           radius = 1,
           rlen = 100)

# --- Plot training process
plot(map, type = "changes")
# More iterations might be needed (argument: rlen is default 100)

# --- Plot weight vectors
plot(map)

# ---- Plot count
# Indicates how many datapoints are in each node
plot(map, type = "count")

# Plot neighbour distance
plot(map, type = "dist.neighbours")

# Plot heat map
plot(map, type = "property", property = map$codes[[1]][, 5]) # Size

