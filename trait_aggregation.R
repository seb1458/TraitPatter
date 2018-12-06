###################
### Aggregation ###
###################

# --------------------------------------------------------------------------------------------------------------- #
#### Working Directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(purrr)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR_harmonized.csv"))
NAM <- read.csv(file.path(path, "North America", "macroinvertebrate_NAM_harmonized.csv")) 
AUS <- read.csv(file.path(path, "Australia", "macroinvertebrate_AUS_harmonized.csv")) 

# --------------------------------------------------------------------------------------------------------------- #
# Aggregation Process
# 
# Due to wide range in the occurence of genera in all databases the modalities of each trait are weighted.
# For this all values are divided by the occurence of the genera. After this the trait modalities are aggregated
# on the family level. At last in each trait the maximum value gets assigned the respective modality.


# --------------------------------------------------------------------------------------------------------------- #
#### Europe ####
names(EUR)

# ---- EUR: Weighting of modalities ---- 
# Get list of genera ant their occurence
genus_list <- as.data.frame(table(EUR$genus))

# Merge with European database
EUR <- merge(EUR, genus_list, by.x = "genus", by.y = "Var1", all.x = TRUE, sort = FALSE)
EUR <- select(EUR, order, family, genus, everything())

# Divide each trait modality by the genus frequency
EUR <- EUR %>%
  mutate_at(vars(ph_acidic:stage4), funs(./Freq)) %>%
  select(-Freq)

# ---- EUR: Aggregate on family level ----
# Replace NAs with zeroes
cols <- purrr::rerun(length(EUR[6:ncol(EUR)]), 0) %>% purrr::set_names(names(EUR[6:ncol(EUR)]))
EUR <- replace_na(EUR, cols)

EUR_agg <- EUR %>%
  group_by(order, family, genus) %>%
  summarise_at(vars(ph_acidic:stage4), funs(sum)) %>%
  group_by(order, family) %>%
  summarise_at(vars(ph_acidic:stage4), funs(sum))


# ---- EUR: Get modalities for the maxima ----
trait_fin_EUR <- data.frame(
  region = c(rep("EUR", nrow(EUR_agg))),
  family = EUR_agg$family,
  ph = grep("ph_", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("ph_", names(EUR_agg))], 1, which.max)],
  feed_mode = grep("feed", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("feed", names(EUR_agg))], 1, which.max)],
  locomotion = grep("loc", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("loc", names(EUR_agg))], 1, which.max)],
  respiration = grep("resp", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("resp", names(EUR_agg))], 1, which.max)],
  drift = grep("drift", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("drift", names(EUR_agg))], 1, which.max)],
  size = grep("size", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("size", names(EUR_agg))], 1, which.max)],
  aquatic_stages = grep("stage", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("stage", names(EUR_agg))], 1, which.max)]
)

# ---- EUR: Final table ----
write.table(trait_fin_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR_agg.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### North America ####
names(NAM)

# ---- NAM: Weighting of modalities ---- 
# Get list of genera ant their occurence
genus_list <- as.data.frame(table(NAM$Genus))

# Merge with European database
NAM <- merge(NAM, genus_list, by.x = "Genus", by.y = "Var1", all.x = TRUE, sort = FALSE)
NAM <- select(NAM, Taxa, Family, Genus, everything())

# Divide each trait modality by the genus frequency
NAM <- NAM %>%
  mutate_at(vars(ph_acidic:stage4), funs(./Freq)) %>%
  select(-Freq)

# ---- NAM: Aggregate on family level ----
# Replace NAs with zeroes
cols <- purrr::rerun(length(NAM[5:ncol(NAM)]), 0) %>% purrr::set_names(names(NAM[5:ncol(NAM)]))
NAM <- replace_na(NAM, cols)

NAM_agg <- NAM %>%
  group_by(Family, Genus) %>%
  summarise_at(vars(ph_acidic:stage4), funs(sum)) %>%
  group_by(Family) %>%
  summarise_at(vars(ph_acidic:stage4), funs(sum))


# ---- NAM: Get modalities for the maxima ----
trait_fin_NAM <- data.frame(
  region = c(rep("NAM", nrow(NAM_agg))),
  family = NAM_agg$Family,
  ph = grep("ph_", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("ph_", names(NAM_agg))], 1, which.max)],
  feed_mode = grep("feed", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("feed", names(NAM_agg))], 1, which.max)],
  locomotion = grep("loc", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("loc", names(NAM_agg))], 1, which.max)],
  respiration = grep("resp", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("resp", names(NAM_agg))], 1, which.max)],
  drift = grep("drift", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("drift", names(NAM_agg))], 1, which.max)],
  size = grep("size", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("size", names(NAM_agg))], 1, which.max)],
  aquatic_stages = grep("stage", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("stage", names(NAM_agg))], 1, which.max)]
)

# ---- NAM: Final table ----
write.table(trait_fin_NAM, file = "~/Schreibtisch/Thesis/data/North America/macroinvertebrate_NAM_agg.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### Australia ####
names(AUS)

# ---- AUS: Weighting of modalities ---- 
# Get list of genera ant their occurence
genus_list <- as.data.frame(table(AUS$Genus))

# Merge with European database
AUS <- merge(AUS, genus_list, by.x = "Genus", by.y = "Var1", all.x = TRUE, sort = FALSE)
AUS <- select(AUS, Order, Family, Genus, everything())

# Divide each trait modality by the genus frequency
AUS <- AUS %>%
  mutate_at(vars(ph_acidic:stage4), funs(./Freq)) %>%
  select(-Freq)

# ---- AUS: Aggregate on family level ----
# Replace NAs with zeroes
cols <- purrr::rerun(length(AUS[5:ncol(AUS)]), 0) %>% purrr::set_names(names(AUS[5:ncol(AUS)]))
AUS <- replace_na(AUS, cols)

AUS_agg <- AUS %>%
  group_by(Family, Genus) %>%
  summarise_at(vars(ph_acidic:stage4), funs(sum)) %>%
  group_by(Family) %>%
  summarise_at(vars(ph_acidic:stage4), funs(sum))


# ---- AUS: Get modalities for the maxima ----
trait_fin_AUS <- data.frame(
  region = c(rep("AUS", nrow(AUS_agg))),
  family = AUS_agg$Family,
  ph = grep("ph_", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("ph_", names(AUS_agg))], 1, which.max)],
  feed_mode = grep("feed", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("feed", names(AUS_agg))], 1, which.max)],
  locomotion = grep("loc", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("loc", names(AUS_agg))], 1, which.max)],
  respiration = grep("resp", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("resp", names(AUS_agg))], 1, which.max)],
  drift = grep("drift", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("drift", names(AUS_agg))], 1, which.max)],
  size = grep("size", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("size", names(AUS_agg))], 1, which.max)],
  aquatic_stages = grep("stage", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("stage", names(AUS_agg))], 1, which.max)]
)

# ---- AUS: Final table ----
write.table(trait_fin_AUS, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_agg.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### Final Table ####
# This table contains all traits and all regions
trait_fin_ALL <- rbind(trait_fin_EUR, trait_fin_NAM, trait_fin_AUS)

# Write .csv
write.table(trait_fin_AUS, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL.csv", sep = ",")
