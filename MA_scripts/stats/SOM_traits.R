#####################################
#### Stats: Self Organizing Maps ####
#####################################

#### Trait SOMs ####

# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(kohonen)
library(RColorBrewer)
library(beepr)


# --------------------------------------------------------------------------------------------------------------- #
#### Preparation ####
source("~/Schreibtisch/Thesis/scripts/stats/cleanUp.R")
source("~/Schreibtisch/Thesis/scripts/stats/prepDat.R")


# --------------------------------------------------------------------------------------------------------------- #
#### Load data ####
ALL <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin_final.csv"), sep = ",", row.names = 1, header = TRUE)
ALL[is.na(ALL)] <- 0

# --------------------------------------------------------------------------------------------------------------- #
#### Number of Cells for SOM Grid
5*sqrt(length(ALL[, 4:ncol(ALL)]))
# 28 cells are sufficient

# --- Set grid
grid_dim <- somgrid(4, 4, "hexagonal")


# --------------------------------------------------------------------------------------------------------------- #
#### SOM: Europe ####
# --- Savving plots to:
setwd("~/Schreibtisch/Thesis/final_paper/Figures/results/SOM/EUR")

# --- Extract European data
EUR <- ALL[grepl("Europe", ALL$Region), ]

# Use all trait modalities
EUR_dat <- EUR[4:ncol(EUR)]
EUR_dat <- as.matrix(EUR_dat)

# --- Compute map
set.seed(123)
som_EUR <- som(EUR_dat, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_EUR, type = "changes")

# Plot count per node
pdf(file = "SOM_count.pdf", paper = "USr", width = 30, height = 20)
plot(som_EUR, type = "count", main = " ", cex = 3)
title(main = "Europe", cex.main = 5)
dev.off()

# --- Display as heatmap for different traits and their modalities
codes_EUR <- as.data.frame(getCodes(som_EUR))
names(codes_EUR)

# Plot quality
plot(som_EUR, type = "quality")


# --------------------------------------------------------- #
# pH Preference
pdf(file = "pH_acidic.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$ph_acidic, main = "")
title(main = "Acidic", cex.main = 6)
mtext("pH", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "pH_normal.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$ph_normal, main = "")
title(main = "Neutral/Alkaline", cex.main = 6)
dev.off()

# --------------------------------------------------------- #
# Feeding Mode
pdf(file = "feed_filter.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$feed_filter, main = "")
title(main = "Filter-feeder", cex.main = 6)
mtext("Feeding Mode", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "feed_shredder.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$feed_shredder, main = "")
title(main = "Shredder", cex.main = 6)
dev.off()

pdf(file = "feed_gatherer.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$feed_gatherer, main = "")
title(main = "Gatherer", cex.main = 6)
dev.off()

pdf(file = "feed_scraper.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$feed_scraper, main = "")
title(main = "Scraper", cex.main = 6)
dev.off()

pdf(file = "feed_predator.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$feed_predator, main = "")
title(main = "Predator", cex.main = 6)
dev.off()

pdf(file = "feed_parasite.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$feed_parasite, main = "")
title(main = "Parasite", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Locomotion
pdf(file = "loc_burrow.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$loc_burrow, main = "")
title(main = "Burrower", cex.main = 6)
mtext("Locomotion", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "loc_skater.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$loc_skate, main = "")
title(main = "Skater", cex.main = 6)
dev.off()

pdf(file = "loc_swimmer.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$loc_swim, main = "")
title(main = "Swimmer", cex.main = 6)
dev.off()

pdf(file = "loc_sprawl.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$loc_sprawl, main = "")
title(main = "Sprawler", cex.main = 6)
dev.off()

pdf(file = "loc_sessil.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$loc_sessil, main = "")
title(main = "Sessile", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Respiration
pdf(file = "resp_gills.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$resp_gills, main = "")
title(main = "Gills", cex.main = 6)
mtext("Respiration", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "resp_atmospheric.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$resp_atmospheric, main = "")
title(main = "Atmospheric", cex.main = 6)
dev.off()

pdf(file = "resp_spiracle.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$resp_spiracle, main = "")
title(main = "Spiracle", cex.main = 6)
dev.off()

pdf(file = "resp_plastron.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$resp_plastron, main = "")
title(main = "Plastron", cex.main = 6)
dev.off()

pdf(file = "resp_tegument.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$resp_tegument, main = "")
title(main = "Tegument", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Size
pdf(file = "size_small.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$size_small, main = "")
title(main = "Small", cex.main = 6)
mtext("Size", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "size_medium.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$size_medium, main = "")
title(main = "Medium", cex.main = 6)
dev.off()

pdf(file = "size_large.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$size_large, main = "")
title(main = "Large", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Reproduction
pdf(file = "rep_aqu.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$rep_aqu, main = "")
title(main = "Aquatic Eggs", cex.main = 6)
mtext("Reproduction", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "rep_ter.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$rep_ter, main = "")
title(main = "Terrestric Eggs", cex.main = 6)
dev.off()

pdf(file = "rep_ovo.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$rep_ovo, main = "")
title(main = "Ovoviparity", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Voltinism 
pdf(file = "volt1.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$volt1, main = "")
title(main = "Semivoltine", cex.main = 6)
mtext("Voltinism", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "volt2.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$volt2, main = "")
title(main = "Univoltine", cex.main = 6)
dev.off()

pdf(file = "volt3.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$volt3, main = "")
title(main = "Multivoltine", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Temperature Preference
pdf(file = "temp_vcold.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$temp_very_cold, main = "")
title(main = "Very Cold", cex.main = 6)
mtext("Temperature", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "temp_cold.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$temp_cold, main = "")
title(main = "Cold", cex.main = 6)
dev.off()

pdf(file = "temp_moderate.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$temp_mod, main = "")
title(main = "Moderate", cex.main = 6)
dev.off()

pdf(file = "temp_warm.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$temp_warm, main = "")
title(main = "Warm", cex.main = 6)
dev.off()

pdf(file = "temp_indifferent.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_EUR, type = "property", property = codes_EUR$temp_ind, main = "")
title(main = "Indifferent", cex.main = 6)
dev.off()


# --------------------------------------------------------------------------------------------------------------- #
#### SOM: North America ####
# --- Savving plots to:
setwd("~/Schreibtisch/Thesis/final_paper/Figures/results/SOM/NAM")

# --- Extract European data
NAM <- ALL[grepl("North America", ALL$Region), ]

# Use all trait modalities
NAM_dat <- NAM[4:ncol(NAM)]
NAM_dat <- as.matrix(NAM_dat)

# --- Compute map
set.seed(123)
som_NAM <- som(NAM_dat, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_NAM, type = "changes")

# Plot count per node
pdf(file = "SOM_count.pdf", paper = "USr", width = 30, height = 20)
plot(som_NAM, type = "count", main = " ", cex = 3)
title(main = "North America", cex.main = 5)
dev.off()

# --- Display as heatmap for different traits and their modalities
str(som_NAM)
codes_NAM <- as.data.frame(getCodes(som_NAM))
names(codes_NAM)

# Plot quality
plot(som_NAM, type = "quality")

# --------------------------------------------------------- #
# pH Preference
pdf(file = "pH_acidic.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$ph_acidic, main = "")
title(main = "Acidic", cex.main = 6)
mtext("pH", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "pH_normal.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$ph_normal, main = "")
title(main = "Neutral/Alkaline", cex.main = 6)
dev.off()

# --------------------------------------------------------- #
# Feeding Mode
pdf(file = "feed_filter.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$feed_filter, main = "")
title(main = "Filter-feeder", cex.main = 6)
mtext("Feeding Mode", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "feed_shredder.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$feed_shredder, main = "")
title(main = "Shredder", cex.main = 6)
dev.off()

pdf(file = "feed_gatherer.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$feed_gatherer, main = "")
title(main = "Gatherer", cex.main = 6)
dev.off()

pdf(file = "feed_scraper.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$feed_scraper, main = "")
title(main = "Scraper", cex.main = 6)
dev.off()

pdf(file = "feed_predator.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$feed_predator, main = "")
title(main = "Predator", cex.main = 6)
dev.off()

pdf(file = "feed_parasite.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$feed_parasite, main = "")
title(main = "Parasite", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Locomotion
pdf(file = "loc_burrow.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$loc_burrow, main = "")
title(main = "Burrower", cex.main = 6)
mtext("Locomotion", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "loc_skater.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$loc_skate, main = "")
title(main = "Skater", cex.main = 6)
dev.off()

pdf(file = "loc_swimmer.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$loc_swim, main = "")
title(main = "Swimmer", cex.main = 6)
dev.off()

pdf(file = "loc_sprawl.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$loc_sprawl, main = "")
title(main = "Sprawler", cex.main = 6)
dev.off()

pdf(file = "loc_sessil.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$loc_sessil, main = "")
title(main = "Sessile", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Respiration
pdf(file = "resp_gills.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$resp_gills, main = "")
title(main = "Gills", cex.main = 6)
mtext("Respiration", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "resp_atmospheric.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$resp_atmospheric, main = "")
title(main = "Atmospheric", cex.main = 6)
dev.off()

pdf(file = "resp_spiracle.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$resp_spiracle, main = "")
title(main = "Spiracle", cex.main = 6)
dev.off()

pdf(file = "resp_plastron.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$resp_plastron, main = "")
title(main = "Plastron", cex.main = 6)
dev.off()

pdf(file = "resp_tegument.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$resp_tegument, main = "")
title(main = "Tegument", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Size
pdf(file = "size_small.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$size_small, main = "")
title(main = "Small", cex.main = 6)
mtext("Size", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "size_medium.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$size_medium, main = "")
title(main = "Medium", cex.main = 6)
dev.off()

pdf(file = "size_large.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$size_large, main = "")
title(main = "Large", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Reproduction
pdf(file = "rep_aqu.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$rep_aqu, main = "")
title(main = "Aquatic Eggs", cex.main = 6)
mtext("Reproduction", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "rep_ter.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$rep_ter, main = "")
title(main = "Terrestric Eggs", cex.main = 6)
dev.off()

pdf(file = "rep_ovo.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$rep_ovo, main = "")
title(main = "Ovoviparity", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Voltinism 
pdf(file = "volt1.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$volt1, main = "")
title(main = "Semivoltine", cex.main = 6)
mtext("Voltinism", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "volt2.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$volt2, main = "")
title(main = "Univoltine", cex.main = 6)
dev.off()

pdf(file = "volt3.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$volt3, main = "")
title(main = "Multivoltine", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Temperature Preference
pdf(file = "temp_vcold.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$temp_very_cold, main = "")
title(main = "Very Cold", cex.main = 6)
mtext("Temperature", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "temp_cold.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$temp_cold, main = "")
title(main = "Cold", cex.main = 6)
dev.off()

pdf(file = "temp_moderate.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$temp_mod, main = "")
title(main = "Moderate", cex.main = 6)
dev.off()

pdf(file = "temp_warm.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$temp_warm, main = "")
title(main = "Warm", cex.main = 6)
dev.off()

pdf(file = "temp_indifferent.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_NAM, type = "property", property = codes_NAM$temp_ind, main = "")
title(main = "Indifferent", cex.main = 6)
dev.off()


# --------------------------------------------------------------------------------------------------------------- #
#### SOM: Australia ####
# --- Savving plots to:
setwd("~/Schreibtisch/Thesis/final_paper/Figures/results/SOM/AUS")

# --- Extract European data
AUS <- ALL[grepl("Australia", ALL$Region), ]

# Use all trait modalities
AUS_dat <- AUS[4:ncol(AUS)]
AUS_dat <- as.matrix(AUS_dat)

# --- Compute map
set.seed(123)
som_AUS <- som(AUS_dat, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_AUS, type = "changes")

# Plot count per node
pdf(file = "SOM_count.pdf", paper = "USr", width = 30, height = 20)
plot(som_AUS, type = "count", main = " ", cex = 3)
title(main = "Australia", cex.main = 5)
dev.off()

# --- Display as heatmap for different traits and their modalities
str(som_AUS)
codes_AUS <- as.data.frame(getCodes(som_AUS))
names(codes_AUS)

# Plot quality
plot(som_AUS, type = "quality")
mean(som_AUS$distances)

# --------------------------------------------------------- #
# pH Preference
pdf(file = "pH_acidic.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$ph_acidic, main = "")
title(main = "Acidic", cex.main = 6)
mtext("pH", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "pH_normal.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$ph_normal, main = "")
title(main = "Neutral/Alkaline", cex.main = 6)
dev.off()

# --------------------------------------------------------- #
# Feeding Mode
pdf(file = "feed_filter.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$feed_filter, main = "")
title(main = "Filter-feeder", cex.main = 6)
dev.off()

pdf(file = "feed_shredder.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$feed_shredder, main = "")
title(main = "Shredder", cex.main = 6)
mtext("Feeding Mode", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "feed_gatherer.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$feed_gatherer, main = "")
title(main = "Gatherer", cex.main = 6)
dev.off()

pdf(file = "feed_scraper.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$feed_scraper, main = "")
title(main = "Scraper", cex.main = 6)
dev.off()

pdf(file = "feed_predator.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$feed_predator, main = "")
title(main = "Predator", cex.main = 6)
dev.off()

pdf(file = "feed_parasite.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$feed_parasite, main = "")
title(main = "Parasite", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Locomotion
pdf(file = "loc_burrow.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$loc_burrow, main = "")
title(main = "Burrower", cex.main = 6)
mtext("Locomotion", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "loc_skater.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$loc_skate, main = "")
title(main = "Skater", cex.main = 6)
dev.off()

pdf(file = "loc_swimmer.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$loc_swim, main = "")
title(main = "Swimmer", cex.main = 6)
dev.off()

pdf(file = "loc_sprawl.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$loc_sprawl, main = "")
title(main = "Sprawler", cex.main = 6)
dev.off()

pdf(file = "loc_sessil.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$loc_sessil, main = "")
title(main = "Sessile", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Respiration
pdf(file = "resp_gills.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$resp_gills, main = "")
title(main = "Gills", cex.main = 6)
mtext("Respiration", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "resp_atmospheric.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$resp_atmospheric, main = "")
title(main = "Atmospheric", cex.main = 6)
dev.off()

pdf(file = "resp_spiracle.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$resp_spiracle, main = "")
title(main = "Spiracle", cex.main = 6)
dev.off()

pdf(file = "resp_plastron.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$resp_plastron, main = "")
title(main = "Plastron", cex.main = 6)
dev.off()

pdf(file = "resp_tegument.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$resp_tegument, main = "")
title(main = "Tegument", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Size
pdf(file = "size_small.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$size_small, main = "")
title(main = "Small", cex.main = 6)
mtext("Size", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "size_medium.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$size_medium, main = "")
title(main = "Medium", cex.main = 6)
dev.off()

pdf(file = "size_large.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$size_large, main = "")
title(main = "Large", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Reproduction
pdf(file = "rep_aqu.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$rep_aqu, main = "")
title(main = "Aquatic Eggs", cex.main = 6)
mtext("Reproduction", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "rep_ter.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$rep_ter, main = "")
title(main = "Terrestric Eggs", cex.main = 6)
dev.off()

pdf(file = "rep_ovo.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$rep_ovo, main = "")
title(main = "Ovoviparity", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Voltinism 
pdf(file = "volt1.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$volt1, main = "")
title(main = "Semivoltine", cex.main = 6)
mtext("Voltinism", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "volt2.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$volt2, main = "")
title(main = "Univoltine", cex.main = 6)
dev.off()

pdf(file = "volt3.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$volt3, main = "")
title(main = "Multivoltine", cex.main = 6)
dev.off()


# --------------------------------------------------------- #
# Temperature Preference
pdf(file = "temp_vcold.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$temp_very_cold, main = "")
title(main = "Very Cold", cex.main = 6)
mtext("Temperature", side = 2, line = 4, cex = 7)
dev.off()

pdf(file = "temp_cold.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$temp_cold, main = "")
title(main = "Cold", cex.main = 6)
dev.off()

pdf(file = "temp_moderate.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$temp_mod, main = "")
title(main = "Moderate", cex.main = 6)
dev.off()

pdf(file = "temp_warm.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$temp_warm, main = "")
title(main = "Warm", cex.main = 6)
dev.off()

pdf(file = "temp_indifferent.pdf", paper = "USr", width = 30, height = 20)
par(oma = c(1, 5, 1, 1))
plot(som_AUS, type = "property", property = codes_AUS$temp_ind, main = "")
title(main = "Indifferent", cex.main = 6)
dev.off()

# --------------------------------------------------------------------------------------------------------------- #
# Some modalities are not expressed on the aggregated level, and therefore no map could be computed.
# These traits are: 

# Europe
# Respiration: Spiracle, Plastron, Atmospheric
# Life duration: < 1 Month
# Aquatic stage: up to pupa, up to adult

# North America
# Reproduction: Ovoviparity

# Australia
# pH: Normale/alkaline preference
# Feeding mode: Parasite