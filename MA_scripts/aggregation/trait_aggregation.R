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
# cols <- purrr::rerun(length(EUR[6:ncol(EUR)]), 0) %>% purrr::set_names(names(EUR[6:ncol(EUR)]))
# EUR <- replace_na(EUR, cols)

# ---- EUR: Aggregate on genus level ---- 
# Get list of species frequencies
species_freq <- as.data.frame(table(EUR$genus))

# Merge with European database
EUR <- merge(EUR, species_freq, by.x = "genus", by.y = "Var1", all.x = TRUE, sort = FALSE)

EUR <- select(EUR, order, family, genus, everything())

# Divide each trait modality by the species frequency and group by order, family and genus
EUR <- EUR %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(order, family, genus) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# ---- EUR: Aggregate on family level ----
# Get list of genera frequencies
genera_freq <- as.data.frame(table(EUR$family))

# Merge with European database
EUR <- merge(EUR, genera_freq, by.x = "family", by.y = "Var1", all.x = TRUE, sort = FALSE)

EUR <- select(EUR, order, family, genus, everything())

# Divide each trait modality by the genera frequency and group by order and family
EUR_agg <- EUR %>%
  mutate_at(vars(ph_acidic:temp_ind), funs(./Freq)) %>%
  group_by(order, family) %>%
  summarise_at(vars(ph_acidic:temp_ind), funs(sum(., na.rm = TRUE))) %>%
  ungroup()


# pH preference
EUR_agg$ph_max <- apply(EUR_agg[, grep("ph_", names(EUR_agg))], 1, max, na.rm = TRUE)

EUR_agg <- EUR_agg %>%
  mutate(ph_acidic = ifelse(ph_acidic == ph_max, 1, NA),
         ph_normal = ifelse(ph_normal == ph_max & is.na(ph_acidic), 1, NA)) %>%
  select(-ph_max)

# Feeding mode
EUR_agg$feed_max <- apply(EUR_agg[, grep("feed_", names(EUR_agg))], 1, max, na.rm = TRUE)
EUR_agg$feed_max <- ifelse(EUR_agg$feed_max == 0, NA, EUR_agg$feed_max)

EUR_agg <- EUR_agg %>%
  mutate(feed_shredder = ifelse(feed_shredder == feed_max, 1, NA),
         feed_gatherer = ifelse(feed_gatherer == feed_max & is.na(feed_shredder), 1, NA),
         feed_filter = ifelse(feed_filter == feed_max & is.na(feed_shredder) & is.na(feed_gatherer), 1, NA),
         feed_scraper = ifelse(feed_scraper == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter), 1, NA),
         feed_predator = ifelse(feed_predator == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter) & is.na(feed_scraper), 1, NA),
         feed_parasite = ifelse(feed_parasite == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter) & is.na(feed_scraper) & is.na(feed_predator), 1, NA)) %>%
  select(-feed_max)

# Locomotion
EUR_agg$loc_max <- apply(EUR_agg[, grep("loc_", names(EUR_agg))], 1, max, na.rm = TRUE)
EUR_agg$loc_max <- ifelse(EUR_agg$loc_max == 0, NA, EUR_agg$loc_max)

EUR_agg <- EUR_agg %>%
  mutate(loc_skate = ifelse(loc_skate == loc_max, 1, NA),
         loc_swim = ifelse(loc_swim == loc_max & is.na(loc_skate), 1, NA),
         loc_burrow = ifelse(loc_burrow == loc_max & is.na(loc_skate) & is.na(loc_swim), 1, NA),
         loc_sprawl = ifelse(loc_sprawl == loc_max & is.na(loc_skate) & is.na(loc_swim) & is.na(loc_burrow), 1, NA),
         loc_sessil = ifelse(loc_sessil == loc_max & is.na(loc_skate) & is.na(loc_swim) & is.na(loc_burrow) & is.na(loc_sprawl), 1, NA)) %>%
  select(-loc_max)

# Respiration
EUR_agg$resp_max <- apply(EUR_agg[, grep("resp_", names(EUR_agg))], 1, max, na.rm = TRUE)
EUR_agg$resp_max <- ifelse(EUR_agg$resp_max == 0, NA, EUR_agg$resp_max)

EUR_agg <- EUR_agg %>%
  mutate(resp_tegument = ifelse(resp_tegument == resp_max, 1, NA),
         resp_gills = ifelse(resp_gills == resp_max & is.na(resp_tegument), 1, NA),
         resp_spiracle = ifelse(resp_spiracle == resp_max & is.na(resp_tegument) & is.na(resp_gills), 1, NA),
         resp_plastron = ifelse(resp_plastron == resp_max & is.na(resp_tegument) & is.na(resp_gills) & is.na(resp_spiracle), 1, NA),
         resp_atmospheric = ifelse(resp_atmospheric == resp_max & is.na(resp_tegument) & is.na(resp_gills) & is.na(resp_spiracle) & is.na(resp_plastron), 1, NA)) %>%
  select(-resp_max)

# Size
EUR_agg$size_max <- apply(EUR_agg[, grep("size_", names(EUR_agg))], 1, max, na.rm = TRUE)
EUR_agg$size_max <- ifelse(EUR_agg$size_max == 0, NA, EUR_agg$size_max)

EUR_agg <- EUR_agg %>%
  mutate(size_small = ifelse(size_small == size_max, 1, NA),
         size_medium = ifelse(size_medium == size_max & is.na(size_small), 1, NA),
         size_large = ifelse(size_large == size_max & is.na(size_small) & is.na(size_medium), 1, NA)) %>%
  select(-size_max)

# Voltinism
EUR_agg$volt_max <- apply(EUR_agg[, grep("volt", names(EUR_agg))], 1, max, na.rm = TRUE)
EUR_agg$volt_max <- ifelse(EUR_agg$volt_max == 0, NA, EUR_agg$volt_max)

EUR_agg <- EUR_agg %>%
  mutate(volt1 = ifelse(volt1 == volt_max, 1, NA),
         volt2 = ifelse(volt2 == volt_max & is.na(volt1), 1, NA),
         volt3 = ifelse(volt3 == volt_max & is.na(volt1) & is.na(volt2), 1, NA)) %>%
  select(-volt_max)

# Reproduction
EUR_agg$rep_max <- apply(EUR_agg[, grep("rep_", names(EUR_agg))], 1, max, na.rm = TRUE)
EUR_agg$rep_max <- ifelse(EUR_agg$rep_max == 0, NA, EUR_agg$rep_max)

EUR_agg <- EUR_agg %>%
  mutate(rep_aqu = ifelse(rep_aqu == rep_max, 1, NA),
         rep_ter = ifelse(rep_ter == rep_max & is.na(rep_aqu), 1, NA),
         rep_ovo = ifelse(rep_ovo == rep_max & is.na(rep_aqu) & is.na(rep_ter), 1, NA)) %>%
  select(-rep_max)

# Temperature preference
EUR_agg$temp_max <- apply(EUR_agg[, grep("temp_", names(EUR_agg))], 1, max, na.rm = TRUE)
EUR_agg$temp_max <- ifelse(EUR_agg$temp_max == 0, NA, EUR_agg$temp_max)

EUR_agg <- EUR_agg %>%
  mutate(temp_very_cold = ifelse(temp_very_cold == temp_max, 1, NA),
         temp_cold = ifelse(temp_cold == temp_max & is.na(temp_very_cold), 1, NA),
         temp_mod = ifelse(temp_mod == temp_max & is.na(temp_very_cold) & is.na(temp_cold), 1, NA),
         temp_warm = ifelse(temp_warm == temp_max & is.na(temp_very_cold) & is.na(temp_cold) & is.na(temp_mod), 1, NA),
         temp_ind = ifelse(temp_ind == temp_max & is.na(temp_very_cold) & is.na(temp_cold) & is.na(temp_mod) & is.na(temp_warm), 1, NA)) %>%
  select(-temp_max)


# Add region column
EUR_agg <- EUR_agg %>%
  mutate(region = "EUR") %>%
  select(order, family, region, everything())


# ---- EUR: Remove all rows, were a trait is not coded
# If a trait has has no modality expresses, this family needs to be removed.
# 11 traits = row sum has to be 11 for complete information

EUR_agg <- EUR_agg %>%
  mutate(total_sum = rowSums(.[4:ncol(EUR_agg)], na.rm = TRUE)) %>%
  filter(total_sum == 8) %>%
  select(-total_sum)


# ---- EUR: Get modalities for the maxima ----
trait_fin_EUR <- data.frame(
  order = EUR_agg$order,
  family = EUR_agg$family,
  region = "EUR",
  ph = grep("ph_", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("ph_", names(EUR_agg))], 1, which.max)],
  temperature = grep("temp_", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("temp_", names(EUR_agg))], 1, which.max)],
  feed_mode = grep("feed", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("feed", names(EUR_agg))], 1, which.max)],
  locomotion = grep("loc", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("loc", names(EUR_agg))], 1, which.max)],
  respiration = grep("resp", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("resp", names(EUR_agg))], 1, which.max)],
  size = grep("size", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("size", names(EUR_agg))], 1, which.max)],
  reproduction = grep("rep_", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("rep_", names(EUR_agg))], 1, which.max)],
  voltinism = grep("volt", names(EUR_agg), value = TRUE)[apply(EUR_agg[, grep("volt", names(EUR_agg))], 1, which.max)]
)

# ---- EUR: Modalities as integers ----
trait_fin_EUR_int <- data.frame(
  order = EUR_agg$order,
  family = EUR_agg$family,
  region = "EUR",
  ph = apply(EUR_agg[, grep("ph_", names(EUR_agg))], 1, which.max),
  temperature = apply(EUR_agg[, grep("temp_", names(EUR_agg))], 1, which.max),
  feed_mode = apply(EUR_agg[, grep("feed", names(EUR_agg))], 1, which.max),
  locomotion = apply(EUR_agg[, grep("loc", names(EUR_agg))], 1, which.max),
  respiration = apply(EUR_agg[, grep("resp", names(EUR_agg))], 1, which.max),
  size = apply(EUR_agg[, grep("size", names(EUR_agg))], 1, which.max),
  reproduction = apply(EUR_agg[, grep("rep_", names(EUR_agg))], 1, which.max),
  voltinism = apply(EUR_agg[, grep("volt", names(EUR_agg))], 1, which.max)
)


# ---- EUR: Final tables ----
write.table(EUR_agg, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_EUR_bin.csv", sep = ",")
write.table(trait_fin_EUR, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_EUR_mod.csv", sep = ",")
write.table(trait_fin_EUR_int, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_EUR_int.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### North America ####
names(NAM)

# ---- Replace NAs with zeroes ----
# cols <- purrr::rerun(length(NAM[6:ncol(NAM)]), 0) %>% purrr::set_names(names(NAM[6:ncol(NAM)]))
# NAM <- replace_na(NAM, cols)

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
  summarise_at(vars(ph_acidic:temp_ind), funs(sum(., na.rm = TRUE))) %>%
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
  summarise_at(vars(ph_acidic:temp_ind), funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# pH preference
NAM_agg$ph_max <- apply(NAM_agg[, grep("ph_", names(NAM_agg))], 1, max, na.rm = TRUE)

NAM_agg <- NAM_agg %>%
  mutate(ph_acidic = ifelse(ph_acidic == ph_max, 1, NA),
         ph_normal = ifelse(ph_normal == ph_max & is.na(ph_acidic), 1, NA)) %>%
  select(-ph_max)

# Feeding mode
NAM_agg$feed_max <- apply(NAM_agg[, grep("feed_", names(NAM_agg))], 1, max, na.rm = TRUE)
NAM_agg$feed_max <- ifelse(NAM_agg$feed_max == 0, NA, NAM_agg$feed_max)

NAM_agg <- NAM_agg %>%
  mutate(feed_shredder = ifelse(feed_shredder == feed_max, 1, NA),
         feed_gatherer = ifelse(feed_gatherer == feed_max & is.na(feed_shredder), 1, NA),
         feed_filter = ifelse(feed_filter == feed_max & is.na(feed_shredder) & is.na(feed_gatherer), 1, NA),
         feed_scraper = ifelse(feed_scraper == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter), 1, NA),
         feed_predator = ifelse(feed_predator == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter) & is.na(feed_scraper), 1, NA),
         feed_parasite = ifelse(feed_parasite == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter) & is.na(feed_scraper) & is.na(feed_predator), 1, NA)) %>%
  select(-feed_max)

# Locomotion
NAM_agg$loc_max <- apply(NAM_agg[, grep("loc_", names(NAM_agg))], 1, max, na.rm = TRUE)
NAM_agg$loc_max <- ifelse(NAM_agg$loc_max == 0, NA, NAM_agg$loc_max)

NAM_agg <- NAM_agg %>%
  mutate(loc_skate = ifelse(loc_skate == loc_max, 1, NA),
         loc_swim = ifelse(loc_swim == loc_max & is.na(loc_skate), 1, NA),
         loc_burrow = ifelse(loc_burrow == loc_max & is.na(loc_skate) & is.na(loc_swim), 1, NA),
         loc_sprawl = ifelse(loc_sprawl == loc_max & is.na(loc_skate) & is.na(loc_swim) & is.na(loc_burrow), 1, NA),
         loc_sessil = ifelse(loc_sessil == loc_max & is.na(loc_skate) & is.na(loc_swim) & is.na(loc_burrow) & is.na(loc_sprawl), 1, NA)) %>%
  select(-loc_max)

# Respiration
NAM_agg$resp_max <- apply(NAM_agg[, grep("resp_", names(NAM_agg))], 1, max, na.rm = TRUE)
NAM_agg$resp_max <- ifelse(NAM_agg$resp_max == 0, NA, NAM_agg$resp_max)

NAM_agg <- NAM_agg %>%
  mutate(resp_tegument = ifelse(resp_tegument == resp_max, 1, NA),
         resp_gills = ifelse(resp_gills == resp_max & is.na(resp_tegument), 1, NA),
         resp_spiracle = ifelse(resp_spiracle == resp_max & is.na(resp_tegument) & is.na(resp_gills), 1, NA),
         resp_plastron = ifelse(resp_plastron == resp_max & is.na(resp_tegument) & is.na(resp_gills) & is.na(resp_spiracle), 1, NA),
         resp_atmospheric = ifelse(resp_atmospheric == resp_max & is.na(resp_tegument) & is.na(resp_gills) & is.na(resp_spiracle) & is.na(resp_plastron), 1, NA)) %>%
  select(-resp_max)

# Size
NAM_agg$size_max <- apply(NAM_agg[, grep("size_", names(NAM_agg))], 1, max, na.rm = TRUE)
NAM_agg$size_max <- ifelse(NAM_agg$size_max == 0, NA, NAM_agg$size_max)

NAM_agg <- NAM_agg %>%
  mutate(size_small = ifelse(size_small == size_max, 1, NA),
         size_medium = ifelse(size_medium == size_max & is.na(size_small), 1, NA),
         size_large = ifelse(size_large == size_max & is.na(size_small) & is.na(size_medium), 1, NA)) %>%
  select(-size_max)

# Voltinism
NAM_agg$volt_max <- apply(NAM_agg[, grep("volt", names(NAM_agg))], 1, max, na.rm = TRUE)
NAM_agg$volt_max <- ifelse(NAM_agg$volt_max == 0, NA, NAM_agg$volt_max)

NAM_agg <- NAM_agg %>%
  mutate(volt1 = ifelse(volt1 == volt_max, 1, NA),
         volt2 = ifelse(volt2 == volt_max & is.na(volt1), 1, NA),
         volt3 = ifelse(volt3 == volt_max & is.na(volt1) & is.na(volt2), 1, NA)) %>%
  select(-volt_max)

# Reproduction
NAM_agg$rep_max <- apply(NAM_agg[, grep("rep_", names(NAM_agg))], 1, max, na.rm = TRUE)
NAM_agg$rep_max <- ifelse(NAM_agg$rep_max == 0, NA, NAM_agg$rep_max)

NAM_agg <- NAM_agg %>%
  mutate(rep_aqu = ifelse(rep_aqu == rep_max, 1, NA),
         rep_ter = ifelse(rep_ter == rep_max & is.na(rep_aqu), 1, NA),
         rep_ovo = ifelse(rep_ovo == rep_max & is.na(rep_aqu) & is.na(rep_ter), 1, NA)) %>%
  select(-rep_max)

# Temperature preference
NAM_agg$temp_max <- apply(NAM_agg[, grep("temp_", names(NAM_agg))], 1, max, na.rm = TRUE)
NAM_agg$temp_max <- ifelse(NAM_agg$temp_max == 0, NA, NAM_agg$temp_max)

NAM_agg <- NAM_agg %>%
  mutate(temp_very_cold = ifelse(temp_very_cold == temp_max, 1, NA),
         temp_cold = ifelse(temp_cold == temp_max & is.na(temp_very_cold), 1, NA),
         temp_mod = ifelse(temp_mod == temp_max & is.na(temp_very_cold) & is.na(temp_cold), 1, NA),
         temp_warm = ifelse(temp_warm == temp_max & is.na(temp_very_cold) & is.na(temp_cold) & is.na(temp_mod), 1, NA),
         temp_ind = ifelse(temp_ind == temp_max & is.na(temp_very_cold) & is.na(temp_cold) & is.na(temp_mod) & is.na(temp_warm), 1, NA)) %>%
  select(-temp_max)


# Add region column
NAM_agg <- NAM_agg %>%
  mutate(region = "NAM") %>%
  select(Order, Family, region, everything())


# ---- EUR: Remove all rows, were a trait is not coded
# If a trait has has no modality expresses, this family needs to be removed.
# 11 traits = row sum has to be 11 for complete information

NAM_agg <- NAM_agg %>%
  mutate(total_sum = rowSums(.[4:ncol(NAM_agg)], na.rm = TRUE)) %>%
  filter(total_sum == 8) %>%
  select(-total_sum)


# ---- NAM: Get modalities for the maxima ----
trait_fin_NAM <- data.frame(
  order = NAM_agg$Order,
  family = NAM_agg$Family,
  region = "NAM",
  ph = grep("ph_", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("ph_", names(NAM_agg))], 1, which.max)],
  temperature = grep("temp_", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("temp_", names(NAM_agg))], 1, which.max)],
  feed_mode = grep("feed", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("feed", names(NAM_agg))], 1, which.max)],
  locomotion = grep("loc", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("loc", names(NAM_agg))], 1, which.max)],
  respiration = grep("resp", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("resp", names(NAM_agg))], 1, which.max)],
  size = grep("size", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("size", names(NAM_agg))], 1, which.max)],
  reproduction = grep("rep_", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("rep_", names(NAM_agg))], 1, which.max)],
  voltinism = grep("volt", names(NAM_agg), value = TRUE)[apply(NAM_agg[, grep("volt", names(NAM_agg))], 1, which.max)]
)

# ---- NAM: Modalities as integers ----
trait_fin_NAM_int <- data.frame(
  order = NAM_agg$Order,
  family = NAM_agg$Family,
  region = "NAM",
  ph = apply(NAM_agg[, grep("ph_", names(NAM_agg))], 1, which.max),
  temperature = apply(NAM_agg[, grep("temp_", names(NAM_agg))], 1, which.max),
  feed_mode = apply(NAM_agg[, grep("feed", names(NAM_agg))], 1, which.max),
  locomotion = apply(NAM_agg[, grep("loc", names(NAM_agg))], 1, which.max),
  respiration = apply(NAM_agg[, grep("resp", names(NAM_agg))], 1, which.max),
  size = apply(NAM_agg[, grep("size", names(NAM_agg))], 1, which.max),
  reproduction = apply(NAM_agg[, grep("rep_", names(NAM_agg))], 1, which.max),
  voltinism = apply(NAM_agg[, grep("volt", names(NAM_agg))], 1, which.max)
)

# ---- NAM: Final table ----
write.table(NAM_agg, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_NAM_bin.csv", sep = ",")
write.table(trait_fin_NAM, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_NAM_mod.csv", sep = ",")
write.table(trait_fin_NAM_int, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_NAM_int.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### Australia ####
names(AUS)

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
  summarise_at(vars(ph_acidic:temp_ind), funs(sum(., na.rm = TRUE))) %>%
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
  summarise_at(vars(ph_acidic:temp_ind), funs(sum(., na.rm = TRUE))) %>%
  ungroup()

# pH preference
AUS_agg$ph_max <- apply(AUS_agg[, grep("ph_", names(AUS_agg))], 1, max, na.rm = TRUE)

AUS_agg <- AUS_agg %>%
  mutate(ph_acidic = ifelse(ph_acidic == ph_max, 1, NA),
         ph_normal = ifelse(ph_normal == ph_max & is.na(ph_acidic), 1, NA)) %>%
  select(-ph_max)

# Feeding mode
AUS_agg$feed_max <- apply(AUS_agg[, grep("feed_", names(AUS_agg))], 1, max, na.rm = TRUE)
AUS_agg$feed_max <- ifelse(AUS_agg$feed_max == 0, NA, AUS_agg$feed_max)

AUS_agg <- AUS_agg %>%
  mutate(feed_shredder = ifelse(feed_shredder == feed_max, 1, NA),
         feed_gatherer = ifelse(feed_gatherer == feed_max & is.na(feed_shredder), 1, NA),
         feed_filter = ifelse(feed_filter == feed_max & is.na(feed_shredder) & is.na(feed_gatherer), 1, NA),
         feed_scraper = ifelse(feed_scraper == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter), 1, NA),
         feed_predator = ifelse(feed_predator == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter) & is.na(feed_scraper), 1, NA),
         feed_parasite = ifelse(feed_parasite == feed_max & is.na(feed_shredder) & is.na(feed_gatherer) & is.na(feed_filter) & is.na(feed_scraper) & is.na(feed_predator), 1, NA)) %>%
  select(-feed_max)

# Locomotion
AUS_agg$loc_max <- apply(AUS_agg[, grep("loc_", names(AUS_agg))], 1, max, na.rm = TRUE)
AUS_agg$loc_max <- ifelse(AUS_agg$loc_max == 0, NA, AUS_agg$loc_max)

AUS_agg <- AUS_agg %>%
  mutate(loc_skate = ifelse(loc_skate == loc_max, 1, NA),
         loc_swim = ifelse(loc_swim == loc_max & is.na(loc_skate), 1, NA),
         loc_burrow = ifelse(loc_burrow == loc_max & is.na(loc_skate) & is.na(loc_swim), 1, NA),
         loc_sprawl = ifelse(loc_sprawl == loc_max & is.na(loc_skate) & is.na(loc_swim) & is.na(loc_burrow), 1, NA),
         loc_sessil = ifelse(loc_sessil == loc_max & is.na(loc_skate) & is.na(loc_swim) & is.na(loc_burrow) & is.na(loc_sprawl), 1, NA)) %>%
  select(-loc_max)

# Locomotion
AUS_agg$resp_max <- apply(AUS_agg[, grep("resp_", names(AUS_agg))], 1, max, na.rm = TRUE)
AUS_agg$resp_max <- ifelse(AUS_agg$resp_max == 0, NA, AUS_agg$resp_max)

AUS_agg <- AUS_agg %>%
  mutate(resp_tegument = ifelse(resp_tegument == resp_max, 1, NA),
         resp_gills = ifelse(resp_gills == resp_max & is.na(resp_tegument), 1, NA),
         resp_spiracle = ifelse(resp_spiracle == resp_max & is.na(resp_tegument) & is.na(resp_gills), 1, NA),
         resp_plastron = ifelse(resp_plastron == resp_max & is.na(resp_tegument) & is.na(resp_gills) & is.na(resp_spiracle), 1, NA),
         resp_atmospheric = ifelse(resp_atmospheric == resp_max & is.na(resp_tegument) & is.na(resp_gills) & is.na(resp_spiracle) & is.na(resp_plastron), 1, NA)) %>%
  select(-resp_max)

# Size
AUS_agg$size_max <- apply(AUS_agg[, grep("size_", names(AUS_agg))], 1, max, na.rm = TRUE)
AUS_agg$size_max <- ifelse(AUS_agg$size_max == 0, NA, AUS_agg$size_max)

AUS_agg <- AUS_agg %>%
  mutate(size_small = ifelse(size_small == size_max, 1, NA),
         size_medium = ifelse(size_medium == size_max & is.na(size_small), 1, NA),
         size_large = ifelse(size_large == size_max & is.na(size_small) & is.na(size_medium), 1, NA)) %>%
  select(-size_max)

# Voltinism
AUS_agg$volt_max <- apply(AUS_agg[, grep("volt", names(AUS_agg))], 1, max, na.rm = TRUE)
AUS_agg$volt_max <- ifelse(AUS_agg$volt_max == 0, NA, AUS_agg$volt_max)

AUS_agg <- AUS_agg %>%
  mutate(volt1 = ifelse(volt1 == volt_max, 1, NA),
         volt2 = ifelse(volt2 == volt_max & is.na(volt1), 1, NA),
         volt3 = ifelse(volt3 == volt_max & is.na(volt1) & is.na(volt2), 1, NA)) %>%
  select(-volt_max)

# Reproduction
AUS_agg$rep_max <- apply(AUS_agg[, grep("rep_", names(AUS_agg))], 1, max, na.rm = TRUE)
AUS_agg$rep_max <- ifelse(AUS_agg$rep_max == 0, NA, AUS_agg$rep_max)

AUS_agg <- AUS_agg %>%
  mutate(rep_aqu = ifelse(rep_aqu == rep_max, 1, NA),
         rep_ter = ifelse(rep_ter == rep_max & is.na(rep_aqu), 1, NA),
         rep_ovo = ifelse(rep_ovo == rep_max & is.na(rep_aqu) & is.na(rep_ter), 1, NA)) %>%
  select(-rep_max)

# Temperature preference
AUS_agg$temp_max <- apply(AUS_agg[, grep("temp_", names(AUS_agg))], 1, max, na.rm = TRUE)
AUS_agg$temp_max <- ifelse(AUS_agg$temp_max == 0, NA, AUS_agg$temp_max)

AUS_agg <- AUS_agg %>%
  mutate(temp_very_cold = ifelse(temp_very_cold == temp_max, 1, NA),
         temp_cold = ifelse(temp_cold == temp_max & is.na(temp_very_cold), 1, NA),
         temp_mod = ifelse(temp_mod == temp_max & is.na(temp_very_cold) & is.na(temp_cold), 1, NA),
         temp_warm = ifelse(temp_warm == temp_max & is.na(temp_very_cold) & is.na(temp_cold) & is.na(temp_mod), 1, NA),
         temp_ind = ifelse(temp_ind == temp_max & is.na(temp_very_cold) & is.na(temp_cold) & is.na(temp_mod) & is.na(temp_warm), 1, NA)) %>%
  select(-temp_max)


# Add region column
AUS_agg <- AUS_agg %>%
  mutate(region = "AUS") %>%
  select(Order, Family, region, everything())


# ---- AUS: Remove all rows, were a trait is not coded
# If a trait has has no modality expresses, this family needs to be removed.
# 11 traits = row sum has to be 11 for complete information

AUS_agg <- AUS_agg %>%
  mutate(total_sum = rowSums(.[4:ncol(AUS_agg)], na.rm = TRUE)) %>%
  filter(total_sum == 8) %>%
  select(-total_sum)


# ---- AUS: Get modalities for the maxima ----
trait_fin_AUS <- data.frame(
  order = AUS_agg$Order,
  family = AUS_agg$Family,
  region = "AUS",
  ph = grep("ph_", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("ph_", names(AUS_agg))], 1, which.max)],
  temperature = grep("temp_", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("temp_", names(AUS_agg))], 1, which.max)],
  feed_mode = grep("feed", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("feed", names(AUS_agg))], 1, which.max)],
  locomotion = grep("loc", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("loc", names(AUS_agg))], 1, which.max)],
  respiration = grep("resp", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("resp", names(AUS_agg))], 1, which.max)],
  size = grep("size", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("size", names(AUS_agg))], 1, which.max)],
  reproduction = grep("rep_", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("rep_", names(AUS_agg))], 1, which.max)],
  voltinism = grep("volt", names(AUS_agg), value = TRUE)[apply(AUS_agg[, grep("volt", names(AUS_agg))], 1, which.max)]
)


# ---- AUS: Modalities as integers ----
trait_fin_AUS_int <- data.frame(
  order = AUS_agg$Order,
  family = AUS_agg$Family,
  region = "AUS",
  ph = apply(AUS_agg[, grep("ph_", names(AUS_agg))], 1, which.max),
  temperature = apply(AUS_agg[, grep("temp_", names(AUS_agg))], 1, which.max),
  feed_mode = apply(AUS_agg[, grep("feed", names(AUS_agg))], 1, which.max),
  locomotion = apply(AUS_agg[, grep("loc", names(AUS_agg))], 1, which.max),
  respiration = apply(AUS_agg[, grep("resp", names(AUS_agg))], 1, which.max),
  size = apply(AUS_agg[, grep("size", names(AUS_agg))], 1, which.max),
  reproduction = apply(AUS_agg[, grep("rep_", names(AUS_agg))], 1, which.max),
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
trait_fin_ALL_int <- rbind(trait_fin_EUR_int, trait_fin_NAM_int, trait_fin_AUS_int)

# Write as .csv
write.table(trait_fin_ALL_int, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_int.csv", sep = ",")
