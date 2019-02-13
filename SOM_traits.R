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
source("~/Schreibtisch/Thesis/scripts/stats/prepDat.R")


# --------------------------------------------------------------------------------------------------------------- #
#### Load data ####
ALL <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin_final.csv"), sep = ",", row.names = 1, header = TRUE)

# --------------------------------------------------------------------------------------------------------------- #
#### Saving plots ####
setwd("~/Schreibtisch/Thesis/final_paper/Figures/results/SOM")


# --------------------------------------------------------------------------------------------------------------- #
#### Number of Cells for SOM Grid
5*sqrt(length(ALL[, 4:ncol(ALL)]))
# 28 cells are sufficient


# --------------------------------------------------------------------------------------------------------------- #
#### SOM: Europe ####
EUR <- ALL[ALL$Region == "Europe", ]

EUR_dat <- EUR[4:ncol(EUR)]

EUR_dat <- as.matrix(EUR_dat)

# --- Set grid
grid_dim <- somgrid(6, 6, "hexagonal")

# --- Compute map
set.seed(123)
som_EUR <- som(EUR_dat, grid = grid_dim, rlen = 500); beep(4)

# Plot training process
plot(som_EUR, type = "changes")

# --- Display as heatmap for different traits and their modalities
codes_EUR <- as.data.frame(getCodes(som_EUR))
names(codes_EUR)

# --------------------------------------------------------- #
# pH Preference
pdf(file = "SOM_EUR_pH_acidic.pdf")
plot(som_EUR, type = "property", property = codes_EUR$ph_acidic, main = "Acidic")
dev.off()

pdf(file = "SOM_EUR_pH_alk.pdf")
plot(som_EUR, type = "property", property = codes_EUR$ph_normal, main = "Neutral/Alkaline")
dev.off()

# --------------------------------------------------------- #
# Feeding Mode
pdf(file = "SOM_EUR_feed_shredder.pdf")
plot(som_EUR, type = "property", property = codes_EUR$feed_shredder, main = "Shredder")
dev.off()

pdf(file = "SOM_EUR_feed_gatherer.pdf")
plot(som_EUR, type = "property", property = codes_EUR$feed_gatherer, main = "Gatherer")
dev.off()

pdf(file = "SOM_EUR_feed_filter.pdf")
plot(som_EUR, type = "property", property = codes_EUR$feed_filter, main = "Filter-feeder")
dev.off()

pdf(file = "SOM_EUR_feed_scraper.pdf")
plot(som_EUR, type = "property", property = codes_EUR$feed_scraper, main = "Scraper")
dev.off()

pdf(file = "SOM_EUR_feed_predator.pdf")
plot(som_EUR, type = "property", property = codes_EUR$feed_predator, main = "Predator")
dev.off()

pdf(file = "SOM_EUR_feed_parasite.pdf")
plot(som_EUR, type = "property", property = codes_EUR$feed_parasite, main = "Parasite")
dev.off()

# --------------------------------------------------------- #
# Locomotion
pdf(file = "SOM_EUR_loc_skater.pdf")
plot(som_EUR, type = "property", property = codes_EUR$loc_skate, main = "Skater")
dev.off()

pdf(file = "SOM_EUR_loc_swimmer.pdf")
plot(som_EUR, type = "property", property = codes_EUR$loc_swim, main = "Swimmer")
dev.off()

pdf(file = "SOM_EUR_loc_burrower.pdf")
plot(som_EUR, type = "property", property = codes_EUR$loc_burrow, main = "Burrower")
dev.off()

pdf(file = "SOM_EUR_loc_sprawler.pdf")
plot(som_EUR, type = "property", property = codes_EUR$loc_sprawl, main = "Sprawler")
dev.off()

pdf(file = "SOM_EUR_loc_sessil.pdf")
plot(som_EUR, type = "property", property = codes_EUR$loc_sessil, main = "Sessil")
dev.off()

# --------------------------------------------------------- #
# Respiration
pdf(file = "SOM_EUR_resp_tegument.pdf")
plot(som_EUR, type = "property", property = codes_EUR$resp_tegument, main = "Tegument")
dev.off()

pdf(file = "SOM_EUR_resp_gills.pdf")
plot(som_EUR, type = "property", property = codes_EUR$resp_gills, main = "Gills")
dev.off()

plot(som_EUR, type = "property", property = codes_EUR$resp_spiracle, main = "Spiracle")
pdf(file = "SOM_EUR_resp_spiracle.pdf")
dev.off()

pdf(file = "SOM_EUR_resp_plastron.pdf")
plot(som_EUR, type = "property", property = codes_EUR$resp_plastron, main = "Plastron")
dev.off()

pdf(file = "SOM_EUR_resp_atmospheric.pdf")
plot(som_EUR, type = "property", property = codes_EUR$resp_atmospheric, main = "Atmospheric")
dev.off()

# --------------------------------------------------------- #
# Size
pdf(file = "SOM_EUR_size_small.pdf")
plot(som_EUR, type = "property", property = codes_EUR$size_small, main = "Small")
dev.off()

pdf(file = "SOM_EUR_size_medium.pdf")
plot(som_EUR, type = "property", property = codes_EUR$size_medium, main = "Medium")
dev.off()

pdf(file = "SOM_EUR_size_large.pdf")
plot(som_EUR, type = "property", property = codes_EUR$size_large, main = "Large")
dev.off()

# --------------------------------------------------------- #
# Reproduction
pdf(file = "SOM_EUR_rep_aquatic.pdf")
plot(som_EUR, type = "property", property = codes_EUR$rep_aqu, main = "Aquatic Eggs")
dev.off()

pdf(file = "SOM_EUR_rep_terrestric.pdf")
plot(som_EUR, type = "property", property = codes_EUR$rep_ter, main = "Terrestric Eggs")
dev.off()

pdf(file = "SOM_EUR_rep_ovoviparity.pdf")
plot(som_EUR, type = "property", property = codes_EUR$rep_ovo, main = "Ovoviparity")
dev.off()

# --------------------------------------------------------- #
# Temperature Preference
pdf(file = "SOM_EUR_temp_vcold.pdf")
plot(som_EUR, type = "property", property = codes_EUR$temp_very_cold, main = "Very Cold")
dev.off()

pdf(file = "SOM_EUR_temp_cold.pdf")
plot(som_EUR, type = "property", property = codes_EUR$temp_cold, main = "Cold")
dev.off()

pdf(file = "SOM_EUR_temp_moderate.pdf")
plot(som_EUR, type = "property", property = codes_EUR$temp_mod, main = "Moderate")
dev.off()

pdf(file = "SOM_EUR_temp_warm.pdf")
plot(som_EUR, type = "property", property = codes_EUR$temp_warm, main = "Warm")
dev.off()

pdf(file = "SOM_EUR_temp_indifferent.pdf")
plot(som_EUR, type = "property", property = codes_EUR$temp_ind, main = "Indifferent")
dev.off()


# --------------------------------------------------------------------------------------------------------------- #
#### SOM: North America ####
NAM <- ALL[grepl("NAM", ALL$region), ]

# --- Trait used voltinism
NAM_dat <- NAM[4:ncol(NAM)]

NAM_dat <- as.matrix(NAM_dat)

# --- Set grid
grid_dim <- somgrid(6, 6, "hexagonal")

# --- Compute map
set.seed(123)
som_NAM <- som(NAM_dat, grid = grid_dim, rlen = 1800); beep(4)

# Plot training process
plot(som_NAM, type = "changes")

# --- Display as heatmap for different traits and their modalities
str(som_NAM)
codes_NAM <- as.data.frame(getCodes(som_NAM))
names(codes_NAM)


# --------------------------------------------------------- #
# pH Preference
pdf(file = "SOM_NAM_pH_acidic.pdf")
plot(som_NAM, type = "property", property = codes_NAM$ph_acidic, main = "Acidic")
dev.off()

pdf(file = "SOM_NAM_pH_alk.pdf")
plot(som_NAM, type = "property", property = codes_NAM$ph_normal, main = "Neutral/Alkaline")
dev.off()

# --------------------------------------------------------- #
# Feeding Mode
pdf(file = "SOM_NAM_feed_shredder.pdf")
plot(som_NAM, type = "property", property = codes_NAM$feed_shredder, main = "Shredder")
dev.off()

pdf(file = "SOM_NAM_feed_gatherer.pdf")
plot(som_NAM, type = "property", property = codes_NAM$feed_gatherer, main = "Gatherer")
dev.off()

pdf(file = "SOM_NAM_feed_filter.pdf")
plot(som_NAM, type = "property", property = codes_NAM$feed_filter, main = "Filter-feeder")
dev.off()

pdf(file = "SOM_NAM_feed_scraper.pdf")
plot(som_NAM, type = "property", property = codes_NAM$feed_scraper, main = "Scraper")
dev.off()

pdf(file = "SOM_NAM_feed_predator.pdf")
plot(som_NAM, type = "property", property = codes_NAM$feed_predator, main = "Predator")
dev.off()

pdf(file = "SOM_NAM_feed_parasite.pdf")
plot(som_NAM, type = "property", property = codes_NAM$feed_parasite, main = "Parasite")
dev.off()

# --------------------------------------------------------- #
# Locomotion
pdf(file = "SOM_NAM_loc_skater.pdf")
plot(som_NAM, type = "property", property = codes_NAM$loc_skate, main = "Skater")
dev.off()

pdf(file = "SOM_NAM_loc_swimmer.pdf")
plot(som_NAM, type = "property", property = codes_NAM$loc_swim, main = "Swimmer")
dev.off()

pdf(file = "SOM_NAM_loc_burrower.pdf")
plot(som_NAM, type = "property", property = codes_NAM$loc_burrow, main = "Burrower")
dev.off()

pdf(file = "SOM_NAM_loc_sprawler.pdf")
plot(som_NAM, type = "property", property = codes_NAM$loc_sprawl, main = "Sprawler")
dev.off()

pdf(file = "SOM_NAM_loc_sessil.pdf")
plot(som_NAM, type = "property", property = codes_NAM$loc_sessil, main = "Sessil")
dev.off()

# --------------------------------------------------------- #
# Respiration
pdf(file = "SOM_NAM_resp_tegument.pdf")
plot(som_NAM, type = "property", property = codes_NAM$resp_tegument, main = "Tegument")
dev.off()

pdf(file = "SOM_NAM_resp_gills.pdf")
plot(som_NAM, type = "property", property = codes_NAM$resp_gills, main = "Gills")
dev.off()

pdf(file = "SOM_NAM_resp_spiracle.pdf")
plot(som_NAM, type = "property", property = codes_NAM$resp_spiracle, main = "Spiracle")
dev.off()

pdf(file = "SOM_NAM_resp_plastron.pdf")
plot(som_NAM, type = "property", property = codes_NAM$resp_plastron, main = "Plastron")
dev.off()

pdf(file = "SOM_NAM_resp_atmospheric.pdf")
plot(som_NAM, type = "property", property = codes_NAM$resp_atmospheric, main = "Atmospheric")
dev.off()

# --------------------------------------------------------- #
# Drift
pdf(file = "SOM_NAM_drift_low.pdf")
plot(som_NAM, type = "property", property = codes_NAM$drift_low, main = "Low")
dev.off()

pdf(file = "SOM_NAM_drift_high.pdf")
plot(som_NAM, type = "property", property = codes_NAM$drift_high, main = "High")
dev.off()

# --------------------------------------------------------- #
# Life Duration
pdf(file = "SOM_NAM_life1.pdf")
plot(som_NAM, type = "property", property = codes_NAM$life1, main = "< 1 Month")
dev.off()

pdf(file = "SOM_NAM_life2.pdf")
plot(som_NAM, type = "property", property = codes_NAM$life2, main = "> 1 Month")
dev.off()

# --------------------------------------------------------- #
# Size
pdf(file = "SOM_NAM_size_small.pdf")
plot(som_NAM, type = "property", property = codes_NAM$size_small, main = "Small")
dev.off()

pdf(file = "SOM_NAM_size_medium.pdf")
plot(som_NAM, type = "property", property = codes_NAM$size_medium, main = "Medium")
dev.off()

pdf(file = "SOM_NAM_size_large.pdf")
plot(som_NAM, type = "property", property = codes_NAM$size_large, main = "Large")
dev.off()

# --------------------------------------------------------- #
# Aquatic Stage
pdf(file = "SOM_NAM_stage_eggs.pdf")
plot(som_NAM, type = "property", property = codes_NAM$stage1, main = "Just Eggs")
dev.off()

pdf(file = "SOM_NAM_stage_larva.pdf")
plot(som_NAM, type = "property", property = codes_NAM$stage2, main = "Up to Larva")
dev.off()

pdf(file = "SOM_NAM_stage_pupa.pdf")
plot(som_NAM, type = "property", property = codes_NAM$stage3, main = "Up to Pupae")
dev.off()

pdf(file = "SOM_NAM_stage_adult.pdf")
plot(som_NAM, type = "property", property = codes_NAM$stage4, main = "Up to Adult")
dev.off()

# --------------------------------------------------------- #
# Reproduction
pdf(file = "SOM_NAM_rep_aquatic.pdf")
plot(som_NAM, type = "property", property = codes_NAM$rep_aqu, main = "Aquatic Eggs")
dev.off()

pdf(file = "SOM_NAM_rep_terrestric.pdf")
plot(som_NAM, type = "property", property = codes_NAM$rep_ter, main = "Terrestric Eggs")
dev.off()

pdf(file = "SOM_NAM_rep_ovoviparity.pdf")
plot(som_NAM, type = "property", property = codes_NAM$rep_ovo, main = "Ovoviparity")
dev.off()

# --------------------------------------------------------- #
# Temperature Preference
pdf(file = "SOM_NAM_temp_vcold.pdf")
plot(som_NAM, type = "property", property = codes_NAM$temp_very_cold, main = "Very Cold")
dev.off()

pdf(file = "SOM_NAM_temp_cold.pdf")
plot(som_NAM, type = "property", property = codes_NAM$temp_cold, main = "Cold")
dev.off()

pdf(file = "SOM_NAM_temp_moderate.pdf")
plot(som_NAM, type = "property", property = codes_NAM$temp_mod, main = "Moderate")
dev.off()

pdf(file = "SOM_NAM_temp_warm.pdf")
plot(som_NAM, type = "property", property = codes_NAM$temp_warm, main = "Warm")
dev.off()

pdf(file = "SOM_NAM_temp_indifferent.pdf")
plot(som_NAM, type = "property", property = codes_NAM$temp_ind, main = "Indifferent")
dev.off()


# --------------------------------------------------------------------------------------------------------------- #
#### SOM: Australia ####
AUS <- ALL[grepl("AUS", ALL$region), ]

# --- Trait used voltinism
AUS_dat <- AUS[4:ncol(AUS)]

AUS_dat <- as.matrix(AUS_dat)

# --- Set grid
grid_dim <- somgrid(6, 6, "hexagonal")

# --- Compute map
set.seed(123)
som_AUS <- som(AUS_dat, grid = grid_dim, rlen = 1800); beep(4)

# Plot training process
plot(som_AUS, type = "changes")

# --- Display as heatmap for different traits and their modalities
str(som_AUS)
codes_AUS <- as.data.frame(getCodes(som_AUS))
names(codes_AUS)

# --------------------------------------------------------- #
# pH Preference
pdf(file = "SOM_AUS_pH_acidic.pdf")
plot(som_AUS, type = "property", property = codes_AUS$ph_acidic, main = "Acidic")
dev.off()

pdf(file = "SOM_AUS_pH_alk.pdf")
plot(som_AUS, type = "property", property = codes_AUS$ph_normal, main = "Neutral/Alkaline")
dev.off()

# --------------------------------------------------------- #
# Feeding Mode
pdf(file = "SOM_AUS_feed_shredder.pdf")
plot(som_AUS, type = "property", property = codes_AUS$feed_shredder, main = "Shredder")
dev.off()

pdf(file = "SOM_AUS_feed_gatherer.pdf")
plot(som_AUS, type = "property", property = codes_AUS$feed_gatherer, main = "Gatherer")
dev.off()

pdf(file = "SOM_AUS_feed_filter.pdf")
plot(som_AUS, type = "property", property = codes_AUS$feed_filter, main = "Filter-feeder")
dev.off()

pdf(file = "SOM_AUS_feed_scraper.pdf")
plot(som_AUS, type = "property", property = codes_AUS$feed_scraper, main = "Scraper")
dev.off()

pdf(file = "SOM_AUS_feed_predator.pdf")
plot(som_AUS, type = "property", property = codes_AUS$feed_predator, main = "Predator")
dev.off()

pdf(file = "SOM_AUS_feed_parasite.pdf")
plot(som_AUS, type = "property", property = codes_AUS$feed_parasite, main = "Parasite")
dev.off()

# --------------------------------------------------------- #
# Locomotion
pdf(file = "SOM_AUS_loc_skater.pdf")
plot(som_AUS, type = "property", property = codes_AUS$loc_skate, main = "Skater")
dev.off()

pdf(file = "SOM_AUS_loc_swimmer.pdf")
plot(som_AUS, type = "property", property = codes_AUS$loc_swim, main = "Swimmer")
dev.off()

pdf(file = "SOM_AUS_loc_burrower.pdf")
plot(som_AUS, type = "property", property = codes_AUS$loc_burrow, main = "Burrower")
dev.off()

pdf(file = "SOM_AUS_loc_sprawler.pdf")
plot(som_AUS, type = "property", property = codes_AUS$loc_sprawl, main = "Sprawler")
dev.off()

pdf(file = "SOM_AUS_loc_sessil.pdf")
plot(som_AUS, type = "property", property = codes_AUS$loc_sessil, main = "Sessil")
dev.off()

# --------------------------------------------------------- #
# Respiration
pdf(file = "SOM_AUS_resp_tegument.pdf")
plot(som_AUS, type = "property", property = codes_AUS$resp_tegument, main = "Tegument")
dev.off()

pdf(file = "SOM_AUS_resp_gills.pdf")
plot(som_AUS, type = "property", property = codes_AUS$resp_gills, main = "Gills")
dev.off()

pdf(file = "SOM_AUS_resp_spiracle.pdf")
plot(som_AUS, type = "property", property = codes_AUS$resp_spiracle, main = "Spiracle")
dev.off()

pdf(file = "SOM_AUS_resp_plastron.pdf")
plot(som_AUS, type = "property", property = codes_AUS$resp_plastron, main = "Plastron")
dev.off()

pdf(file = "SOM_AUS_resp_atmospheric.pdf")
plot(som_AUS, type = "property", property = codes_AUS$resp_atmospheric, main = "Atmospheric")
dev.off()

# --------------------------------------------------------- #
# Drift
pdf(file = "SOM_AUS_drift_low.pdf")
plot(som_AUS, type = "property", property = codes_AUS$drift_low, main = "Low")
dev.off()

pdf(file = "SOM_AUS_drift_high.pdf")
plot(som_AUS, type = "property", property = codes_AUS$drift_high, main = "High")
dev.off()

# --------------------------------------------------------- #
# Life Duration
pdf(file = "SOM_AUS_life1.pdf")
plot(som_AUS, type = "property", property = codes_AUS$life1, main = "< 1 Month")
dev.off()

pdf(file = "SOM_AUS_life2.pdf")
plot(som_AUS, type = "property", property = codes_AUS$life2, main = "> 1 Month")
dev.off()

# --------------------------------------------------------- #
# Size
pdf(file = "SOM_AUS_size_small.pdf")
plot(som_AUS, type = "property", property = codes_AUS$size_small, main = "Small")
dev.off()

pdf(file = "SOM_AUS_size_medium.pdf")
plot(som_AUS, type = "property", property = codes_AUS$size_medium, main = "Medium")
dev.off()

pdf(file = "SOM_AUS_size_large.pdf")
plot(som_AUS, type = "property", property = codes_AUS$size_large, main = "Large")
dev.off()

# --------------------------------------------------------- #
# Aquatic Stage
pdf(file = "SOM_AUS_stage_eggs.pdf")
plot(som_AUS, type = "property", property = codes_AUS$stage1, main = "Just Eggs")
dev.off()

pdf(file = "SOM_AUS_stage_larva.pdf")
plot(som_AUS, type = "property", property = codes_AUS$stage2, main = "Up to Larva")
dev.off()

pdf(file = "SOM_AUS_stage_pupa.pdf")
plot(som_AUS, type = "property", property = codes_AUS$stage3, main = "Up to Pupae")
dev.off()

pdf(file = "SOM_AUS_stage_adult.pdf")
plot(som_AUS, type = "property", property = codes_AUS$stage4, main = "Up to Adult")
dev.off()

# --------------------------------------------------------- #
# Reproduction
pdf(file = "SOM_AUS_rep_aquatic.pdf")
plot(som_AUS, type = "property", property = codes_AUS$rep_aqu, main = "Aquatic Eggs")
dev.off()

pdf(file = "SOM_AUS_rep_terrestric.pdf")
plot(som_AUS, type = "property", property = codes_AUS$rep_ter, main = "Terrestric Eggs")
dev.off()

pdf(file = "SOM_AUS_rep_ovoviparity.pdf")
plot(som_AUS, type = "property", property = codes_AUS$rep_ovo, main = "Ovoviparity")
dev.off()

# --------------------------------------------------------- #
# Temperature Preference
pdf(file = "SOM_AUS_temp_vcold.pdf")
plot(som_AUS, type = "property", property = codes_AUS$temp_very_cold, main = "Very Cold")
dev.off()

pdf(file = "SOM_AUS_temp_cold.pdf")
plot(som_AUS, type = "property", property = codes_AUS$temp_cold, main = "Cold")
dev.off()

pdf(file = "SOM_AUS_temp_moderate.pdf")
plot(som_AUS, type = "property", property = codes_AUS$temp_mod, main = "Moderate")
dev.off()

pdf(file = "SOM_AUS_temp_warm.pdf")
plot(som_AUS, type = "property", property = codes_AUS$temp_warm, main = "Warm")
dev.off()

pdf(file = "SOM_AUS_temp_indifferent.pdf")
plot(som_AUS, type = "property", property = codes_AUS$temp_ind, main = "Indifferent")
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