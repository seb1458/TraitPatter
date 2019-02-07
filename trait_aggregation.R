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
library(taxize)
library(stringr)


# --------------------------------------------------------------------------------------------------------------- #
#### Source Scripts ####

# Preparation of Europe
source("~/Schreibtisch/Thesis/scripts/europe/trait_pre_EUR.R")
source("~/Schreibtisch/Thesis/scripts/europe/fuzzy_trans_EUR.R")

# Preparation of North America
source("~/Schreibtisch/Thesis/scripts/america/trait_pre_NOA.R")

# Preparation of Australia
source("~/Schreibtisch/Thesis/scripts/australia/trait_pre_AST.R")
source("~/Schreibtisch/Thesis/scripts/australia/fuzzy_trans_AST.R")

# Trait harmonization
source("~/Schreibtisch/Thesis/scripts/aggregation/trait_harmonization.R")


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

# ---- Replace NAs with zeroes ----
cols <- purrr::rerun(length(EUR[6:ncol(EUR)]), 0) %>% purrr::set_names(names(EUR[6:ncol(EUR)]))
EUR <- replace_na(EUR, cols)

# ---- EUR: Aggregate on genus level ---- 
# Get list of species frequencies
species_freq <- as.data.frame(table(EUR$genus))

# Merge with European database
EUR <- merge(EUR, species_freq, by.x = "genus", by.y = "Var1", all.x = TRUE, sort = FALSE)

EUR <- select(EUR, order, family, genus, everything())

# Divide each trait modality by the species frequency and group by family and genus
EUR <- EUR %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(order, family, genus) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum)) %>%
  ungroup()

# ---- EUR: Aggregate on family level ----
# Get list of genera frequencies
genera_freq <- as.data.frame(table(EUR$family))

# Merge with European database
EUR <- merge(EUR, genera_freq, by.x = "family", by.y = "Var1", all.x = TRUE, sort = FALSE)

EUR <- select(EUR, order, family, genus, everything())

# Divide each trait modality by the genera frequency and group by family
EUR_agg <- EUR %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(order, family) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum)) %>%
  ungroup()

# Assign binary coding to traits, where maxima get 1, and non-maxima get 0
for (i in 1:nrow(EUR_agg)) {
  
  # pH 
  x <- which(EUR_agg[i, grep("ph_", names(EUR_agg))] != max(EUR_agg[i, grep("ph_", names(EUR_agg))]))
  EUR_agg[i, grep("ph_", names(EUR_agg))[x]] <- 0
  
  # Feeding mode
  x <- which(EUR_agg[i, grep("feed_", names(EUR_agg))] != max(EUR_agg[i, grep("feed_", names(EUR_agg))]))
  EUR_agg[i, grep("feed_", names(EUR_agg))[x]] <- 0
  
  # Locomotion
  x <- which(EUR_agg[i, grep("loc_", names(EUR_agg))] != max(EUR_agg[i, grep("loc_", names(EUR_agg))]))
  EUR_agg[i, grep("loc_", names(EUR_agg))[x]] <- 0
  
  # Respiration
  x <- which(EUR_agg[i, grep("resp_", names(EUR_agg))] != max(EUR_agg[i, grep("resp_", names(EUR_agg))]))
  EUR_agg[i, grep("resp_", names(EUR_agg))[x]] <- 0
  
  # Drift
  x <- which(EUR_agg[i, grep("drift_", names(EUR_agg))] != max(EUR_agg[i, grep("drift_", names(EUR_agg))]))
  EUR_agg[i, grep("drift_", names(EUR_agg))[x]] <- 0
  
  # Life duration
  x <- which(EUR_agg[i, grep("life", names(EUR_agg))] != max(EUR_agg[i, grep("life", names(EUR_agg))]))
  EUR_agg[i, grep("life", names(EUR_agg))[x]] <- 0
  
  # Size
  x <- which(EUR_agg[i, grep("size_", names(EUR_agg))] != max(EUR_agg[i, grep("size_", names(EUR_agg))]))
  EUR_agg[i, grep("size_", names(EUR_agg))[x]] <- 0
  
  # Voltinism
  x <- which(EUR_agg[i, grep("volt", names(EUR_agg))] != max(EUR_agg[i, grep("volt", names(EUR_agg))]))
  EUR_agg[i, grep("volt", names(EUR_agg))[x]] <- 0
  
  # Aquatic stages
  x <- which(EUR_agg[i, grep("stage", names(EUR_agg))] != max(EUR_agg[i, grep("stage", names(EUR_agg))]))
  EUR_agg[i, grep("stage", names(EUR_agg))[x]] <- 0
  
  # Reproduction
  x <- which(EUR_agg[i, grep("rep_", names(EUR_agg))] != max(EUR_agg[i, grep("rep_", names(EUR_agg))]))
  EUR_agg[i, grep("rep_", names(EUR_agg))[x]] <- 0
  
  # Temperature
  x <- which(EUR_agg[i, grep("temp_", names(EUR_agg))] != max(EUR_agg[i, grep("temp_", names(EUR_agg))]))
  EUR_agg[i, grep("temp_", names(EUR_agg))[x]] <- 0
}

# Other values are maxima
EUR_agg <- EUR_agg %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(replace(., . != 0, 1))) %>%
  
  # Add regions column
  mutate(region = "EUR") %>%
  select(family, region, everything())

EUR_agg <- EUR_agg[!is.na(EUR_agg$family), ]

# ---- EUR: Get modalities for the maxima ----
# Replace NAs with zeroes
cols <- purrr::rerun(length(EUR_agg[1:ncol(EUR_agg)]), 0) %>% purrr::set_names(names(EUR_agg[1:ncol(EUR_agg)]))
EUR_agg <- replace_na(EUR_agg, cols)

trait_fin_EUR <- data.frame(
  order = EUR_agg$order,
  family = EUR_agg$family,
  region = "EUR",
  ph = grep("ph_", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("ph_", names(EUR_agg))], 1, which.max)],
  temperature = grep("temp_", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("temp_", names(EUR_agg))], 1, which.max)],
  feed_mode = grep("feed", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("feed", names(EUR_agg))], 1, which.max)],
  locomotion = grep("loc", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("loc", names(EUR_agg))], 1, which.max)],
  respiration = grep("resp", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("resp", names(EUR_agg))], 1, which.max)],
  drift = grep("drift", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("drift", names(EUR_agg))], 1, which.max)],
  life = grep("life", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("life", names(EUR_agg))], 1, which.max)],
  size = grep("size", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("size", names(EUR_agg))], 1, which.max)],
  reproduction = grep("rep_", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("rep_", names(EUR_agg))], 1, which.max)],
  aquatic_stages = grep("stage", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("stage", names(EUR_agg))], 1, which.max)],
  voltinism = grep("volt", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("volt", names(EUR_agg))], 1, which.max)]
)

# ---- EUR: Modalities as integers ----
# Replace NAs with zeroes

trait_fin_EUR_int <- data.frame(
  order = EUR_agg$order,
  family = EUR_agg$family,
  region = "EUR",
  ph = apply(EUR_agg[, grep("ph_", names(EUR_agg))], 1, which.max),
  temperature = apply(EUR_agg[, grep("temp_", names(EUR_agg))], 1, which.max),
  feed_mode = apply(EUR_agg[, grep("feed", names(EUR_agg))], 1, which.max),
  locomotion = apply(EUR_agg[, grep("loc", names(EUR_agg))], 1, which.max),
  respiration = apply(EUR_agg[, grep("resp", names(EUR_agg))], 1, which.max),
  drift = apply(EUR_agg[, grep("drift", names(EUR_agg))], 1, which.max),
  life = apply(EUR_agg[, grep("life", names(EUR_agg))], 1, which.max),
  size = apply(EUR_agg[, grep("size", names(EUR_agg))], 1, which.max),
  reproduction = apply(EUR_agg[, grep("rep_", names(EUR_agg))], 1, which.max),
  aquatic_stages = apply(EUR_agg[, grep("stage", names(EUR_agg))], 1, which.max),
  voltinism = apply(EUR_agg[, grep("volt", names(EUR_agg))], 1, which.max)
)


# ---- EUR: Final tables ----
write.table(EUR_agg, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_EUR_bin.csv", sep = ",")
write.table(trait_fin_EUR, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_EUR_mod.csv", sep = ",")
write.table(trait_fin_EUR_int, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_EUR_int.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### North America ####
names(NAM)

# Lots of genera (442) missing
NAM <- NAM[!is.na(NAM$Genus), ]
NAM <- NAM[!is.na(NAM$Family), ]

# ---- Replace NAs with zeroes ----
cols <- purrr::rerun(length(NAM[6:ncol(NAM)]), 0) %>% purrr::set_names(names(NAM[6:ncol(NAM)]))
NAM <- replace_na(NAM, cols)

# ---- NAM: Aggregate on genus level ---- 
# Get list of species frequencies
species_freq <- as.data.frame(table(NAM$Genus))

# Merge with European database
NAM <- merge(NAM, species_freq, by.x = "Genus", by.y = "Var1", all.x = TRUE, sort = FALSE)

NAM <- select(NAM, Order, Family, Genus, everything())

# NAs in AUS$Freq converted to 1 for proper aggregation
NAM$Freq <- ifelse(is.na(NAM$Freq), 1, NAM$Freq)

# Divide each trait modality by the species frequency and group by family and genus
NAM <- NAM %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(Order, Family, Genus) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum)) %>%
  ungroup()

# ---- NAM: Aggregate on family level ----
# Get list of genera frequencies
genera_freq <- as.data.frame(table(NAM$Family))

# Merge with European database
NAM <- merge(NAM, genera_freq, by.x = "Family", by.y = "Var1", all.x = TRUE, sort = FALSE)

NAM <- select(NAM, Order, Family, Genus, everything())

# Divide each trait modality by the genera frequency and group by family
NAM_agg <-NAM %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(Order, Family) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum)) %>%
  ungroup()

# Assign binary coding to traits, where maxima get 1, and non-maxima get 0
for (i in 1:nrow(NAM_agg)) {
  
  # pH 
  x <- which(NAM_agg[i, grep("ph_", names(NAM_agg))] != max(NAM_agg[i, grep("ph_", names(NAM_agg))]))
  NAM_agg[i, grep("ph_", names(NAM_agg))[x]] <- 0
  
  # Feeding mode
  x <- which(NAM_agg[i, grep("feed_", names(NAM_agg))] != max(NAM_agg[i, grep("feed_", names(NAM_agg))]))
  NAM_agg[i, grep("feed_", names(NAM_agg))[x]] <- 0
  
  # Locomotion
  x <- which(NAM_agg[i, grep("loc_", names(NAM_agg))] != max(NAM_agg[i, grep("loc_", names(NAM_agg))]))
  NAM_agg[i, grep("loc_", names(NAM_agg))[x]] <- 0
  
  # Respiration
  x <- which(NAM_agg[i, grep("resp_", names(NAM_agg))] != max(NAM_agg[i, grep("resp_", names(NAM_agg))]))
  NAM_agg[i, grep("resp_", names(NAM_agg))[x]] <- 0
  
  # Drift
  x <- which(NAM_agg[i, grep("drift_", names(NAM_agg))] != max(NAM_agg[i, grep("drift_", names(NAM_agg))]))
  NAM_agg[i, grep("drift_", names(NAM_agg))[x]] <- 0
  
  # Life duration
  x <- which(NAM_agg[i, grep("life", names(NAM_agg))] != max(NAM_agg[i, grep("life", names(NAM_agg))]))
  NAM_agg[i, grep("life", names(NAM_agg))[x]] <- 0
  
  # Size
  x <- which(NAM_agg[i, grep("size_", names(NAM_agg))] != max(NAM_agg[i, grep("size_", names(NAM_agg))]))
  NAM_agg[i, grep("size_", names(NAM_agg))[x]] <- 0
  
  # Voltinism
  x <- which(NAM_agg[i, grep("volt", names(NAM_agg))] != max(NAM_agg[i, grep("volt", names(NAM_agg))]))
  NAM_agg[i, grep("volt", names(NAM_agg))[x]] <- 0
  
  # Aquatic stages
  x <- which(NAM_agg[i, grep("stage", names(NAM_agg))] != max(NAM_agg[i, grep("stage", names(NAM_agg))]))
  NAM_agg[i, grep("stage", names(NAM_agg))[x]] <- 0
  
  # Reproduction
  x <- which(NAM_agg[i, grep("rep_", names(NAM_agg))] != max(NAM_agg[i, grep("rep_", names(NAM_agg))]))
  NAM_agg[i, grep("rep_", names(NAM_agg))[x]] <- 0
  
  # Temperature
  x <- which(NAM_agg[i, grep("temp_", names(NAM_agg))] != max(NAM_agg[i, grep("temp_", names(NAM_agg))]))
  NAM_agg[i, grep("temp_", names(NAM_agg))[x]] <- 0
}

# Other values are maxima
NAM_agg <- NAM_agg %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(replace(., . != 0, 1))) %>%
  
  # Add regions column
  mutate(region = "NAM") %>%
  select(Family, region, everything())

# ---- NAM: Get modalities for the maxima ----
# Replace NAs with zeroes
cols <- purrr::rerun(length(NAM_agg[1:ncol(NAM_agg)]), 0) %>% purrr::set_names(names(NAM_agg[1:ncol(NAM_agg)]))
NAM_agg <- replace_na(NAM_agg, cols)

trait_fin_NAM <- data.frame(
  order = NAM_agg$Order,
  family = NAM_agg$Family,
  region = "NAM",
  ph = grep("ph_", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("ph_", names(NAM_agg))], 1, which.max)],
  temperature = grep("temp_", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("temp_", names(NAM_agg))], 1, which.max)],
  feed_mode = grep("feed", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("feed", names(NAM_agg))], 1, which.max)],
  locomotion = grep("loc", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("loc", names(NAM_agg))], 1, which.max)],
  respiration = grep("resp", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("resp", names(NAM_agg))], 1, which.max)],
  drift = grep("drift", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("drift", names(NAM_agg))], 1, which.max)],
  life = grep("life", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("life", names(NAM_agg))], 1, which.max)],
  size = grep("size", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("size", names(NAM_agg))], 1, which.max)],
  reproduction = grep("rep_", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("rep_", names(NAM_agg))], 1, which.max)],
  aquatic_stages = grep("stage", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("stage", names(NAM_agg))], 1, which.max)],
  voltinism = grep("volt", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("volt", names(NAM_agg))], 1, which.max)]
)

# ---- NAM: Modalities as integers ----
# Replace NAs with zeroes

trait_fin_NAM_int <- data.frame(
  order = NAM_agg$Order,
  family = NAM_agg$Family,
  region = "NAM",
  ph = apply(NAM_agg[, grep("ph_", names(NAM_agg))], 1, which.max),
  temperature = apply(NAM_agg[, grep("temp_", names(NAM_agg))], 1, which.max),
  feed_mode = apply(NAM_agg[, grep("feed", names(NAM_agg))], 1, which.max),
  locomotion = apply(NAM_agg[, grep("loc", names(NAM_agg))], 1, which.max),
  respiration = apply(NAM_agg[, grep("resp", names(NAM_agg))], 1, which.max),
  drift = apply(NAM_agg[, grep("drift", names(NAM_agg))], 1, which.max),
  life = apply(NAM_agg[, grep("life", names(NAM_agg))], 1, which.max),
  size = apply(NAM_agg[, grep("size", names(NAM_agg))], 1, which.max),
  reproduction = apply(NAM_agg[, grep("rep_", names(NAM_agg))], 1, which.max),
  aquatic_stages = apply(NAM_agg[, grep("stage", names(NAM_agg))], 1, which.max),
  voltinism = apply(NAM_agg[, grep("volt", names(NAM_agg))], 1, which.max)
)

# ---- NAM: Final table ----
write.table(NAM_agg, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_NAM_bin.csv", sep = ",")
write.table(trait_fin_NAM, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_NAM_mod.csv", sep = ",")
write.table(trait_fin_NAM_int, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_NAM_int.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### Australia ####
names(AUS)

# Nothing missing


# ---- Replace NAs with zeroes ----
cols <- purrr::rerun(length(AUS[5:ncol(AUS)]), 0) %>% purrr::set_names(names(AUS[5:ncol(AUS)]))
AUS <- replace_na(AUS, cols)

# ---- AUS: Aggregate on genus level ---- 
# Get list of species frequencies
species_freq <- as.data.frame(table(AUS$Genus))

# Merge with Australian database
AUS <- merge(AUS, species_freq, by.x = "Genus", by.y = "Var1", all.x = TRUE, sort = FALSE)

# Divide each trait modality by the species frequency and group by family and genus
AUS <- AUS %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(Order, Family, Genus) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum)) %>%
  ungroup()

# ---- AUS: Aggregate on family level ----
# Get list of genera frequencies
genera_freq <- as.data.frame(table(AUS$Family))

# Merge with European database
AUS <- merge(AUS, genera_freq, by.x = "Family", by.y = "Var1", all.x = TRUE, sort = FALSE)

AUS <- select(AUS, Order, Family, Genus, everything())

# Divide each trait modality by the genera frequency and group by family
AUS_agg <-AUS %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(Order, Family) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum)) %>%
  ungroup()

# Assign binary coding to traits, where maxima get 1, and non-maxima get 0
for (i in 1:nrow(AUS_agg)) {
  
  # pH 
  x <- which(AUS_agg[i, grep("ph_", names(AUS_agg))] != max(AUS_agg[i, grep("ph_", names(AUS_agg))]))
  AUS_agg[i, grep("ph_", names(AUS_agg))[x]] <- 0
  
  # Feeding mode
  x <- which(AUS_agg[i, grep("feed_", names(AUS_agg))] != max(AUS_agg[i, grep("feed_", names(AUS_agg))]))
  AUS_agg[i, grep("feed_", names(AUS_agg))[x]] <- 0
  
  # Locomotion
  x <- which(AUS_agg[i, grep("loc_", names(AUS_agg))] != max(AUS_agg[i, grep("loc_", names(AUS_agg))]))
  AUS_agg[i, grep("loc_", names(AUS_agg))[x]] <- 0
  
  # Respiration
  x <- which(AUS_agg[i, grep("resp_", names(AUS_agg))] != max(AUS_agg[i, grep("resp_", names(AUS_agg))]))
  AUS_agg[i, grep("resp_", names(AUS_agg))[x]] <- 0
  
  # Drift
  x <- which(AUS_agg[i, grep("drift_", names(AUS_agg))] != max(AUS_agg[i, grep("drift_", names(AUS_agg))]))
  AUS_agg[i, grep("drift_", names(AUS_agg))[x]] <- 0
  
  # Life duration
  x <- which(AUS_agg[i, grep("life", names(AUS_agg))] != max(AUS_agg[i, grep("life", names(AUS_agg))]))
  AUS_agg[i, grep("life", names(AUS_agg))[x]] <- 0
  
  # Size
  x <- which(AUS_agg[i, grep("size_", names(AUS_agg))] != max(AUS_agg[i, grep("size_", names(AUS_agg))]))
  AUS_agg[i, grep("size_", names(AUS_agg))[x]] <- 0
  
  # Voltinism
  x <- which(AUS_agg[i, grep("volt", names(AUS_agg))] != max(AUS_agg[i, grep("volt", names(AUS_agg))]))
  AUS_agg[i, grep("volt", names(AUS_agg))[x]] <- 0
  
  # Aquatic stages
  x <- which(AUS_agg[i, grep("stage", names(AUS_agg))] != max(AUS_agg[i, grep("stage", names(AUS_agg))]))
  AUS_agg[i, grep("stage", names(AUS_agg))[x]] <- 0
  
  # Reproduction
  x <- which(AUS_agg[i, grep("rep_", names(AUS_agg))] != max(AUS_agg[i, grep("rep_", names(AUS_agg))]))
  AUS_agg[i, grep("rep_", names(AUS_agg))[x]] <- 0
  
  # Temperature
  x <- which(AUS_agg[i, grep("temp_", names(AUS_agg))] != max(AUS_agg[i, grep("temp_", names(AUS_agg))]))
  AUS_agg[i, grep("temp_", names(AUS_agg))[x]] <- 0
}

# Other values are maxima
AUS_agg <- AUS_agg %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(replace(., . != 0, 1))) %>%
  
  # Add regions column
  mutate(region = "AUS") %>%
  select(Family, region, everything())


# ---- AUS: Get modalities for the maxima ----
# Replace NAs with zeroes
cols <- purrr::rerun(length(AUS_agg[1:ncol(AUS_agg)]), 0) %>% purrr::set_names(names(AUS_agg[1:ncol(AUS_agg)]))
AUS_agg <- replace_na(AUS_agg, cols)

trait_fin_AUS <- data.frame(
  order = AUS_agg$Order,
  family = AUS_agg$Family,
  region = "AUS",
  ph = grep("ph_", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("ph_", names(AUS_agg))], 1, which.max)],
  temperature = grep("temp_", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("temp_", names(AUS_agg))], 1, which.max)],
  feed_mode = grep("feed", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("feed", names(AUS_agg))], 1, which.max)],
  locomotion = grep("loc", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("loc", names(AUS_agg))], 1, which.max)],
  respiration = grep("resp", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("resp", names(AUS_agg))], 1, which.max)],
  drift = grep("drift", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("drift", names(AUS_agg))], 1, which.max)],
  life = grep("life", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("life", names(AUS_agg))], 1, which.max)],
  size = grep("size", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("size", names(AUS_agg))], 1, which.max)],
  reproduction = grep("rep_", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("rep_", names(AUS_agg))], 1, which.max)],
  aquatic_stages = grep("stage", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("stage", names(AUS_agg))], 1, which.max)],
  voltinism = grep("volt", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("volt", names(AUS_agg))], 1, which.max)]
)


# ---- AUS: Modalities as integers ----
# Replace NAs with zeroes

trait_fin_AUS_int <- data.frame(
  order = AUS_agg$Order,
  family = AUS_agg$Family,
  region = "AUS",
  ph = apply(AUS_agg[, grep("ph_", names(AUS_agg))], 1, which.max),
  temperature = apply(AUS_agg[, grep("temp_", names(AUS_agg))], 1, which.max),
  feed_mode = apply(AUS_agg[, grep("feed", names(AUS_agg))], 1, which.max),
  locomotion = apply(AUS_agg[, grep("loc", names(AUS_agg))], 1, which.max),
  respiration = apply(AUS_agg[, grep("resp", names(AUS_agg))], 1, which.max),
  drift = apply(AUS_agg[, grep("drift", names(AUS_agg))], 1, which.max),
  life = apply(AUS_agg[, grep("life", names(AUS_agg))], 1, which.max),
  size = apply(AUS_agg[, grep("size", names(AUS_agg))], 1, which.max),
  reproduction = apply(AUS_agg[, grep("rep_", names(AUS_agg))], 1, which.max),
  aquatic_stages = apply(AUS_agg[, grep("stage", names(AUS_agg))], 1, which.max),
  voltinism = apply(AUS_agg[, grep("volt", names(AUS_agg))], 1, which.max)
)

# ---- AUS: Final table ----
write.table(AUS_agg, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_AUS_bin.csv", sep = ",")
write.table(trait_fin_AUS, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_AUS_mod.csv", sep = ",")
write.table(trait_fin_AUS_int, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_AUS_int.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### Final Table ####
names(EUR_agg); names(NAM_agg); names(AUS_agg)

EUR_agg <- rename(EUR_agg, Family = family, Order = order)

# This table contains all traits from all regions as binary codes
ALL_agg <- rbind(EUR_agg, NAM_agg, AUS_agg)
ALL_agg <- select(ALL_agg, Order, Family, everything())

# Write as .csv
write.table(ALL_agg, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_bin.csv", sep = ",")


# This table contains all traits and all regions as categories
trait_fin_ALL <- rbind(trait_fin_EUR, trait_fin_NAM, trait_fin_AUS)

# Write as .csv
write.table(trait_fin_ALL, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_mod.csv", sep = ",")


# This table contains all traits as integers
trait_fin_ALL <- rbind(trait_fin_EUR_int, trait_fin_NAM_int, trait_fin_AUS_int)

# Write as .csv
write.table(trait_fin_ALL, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_int.csv", sep = ",")
