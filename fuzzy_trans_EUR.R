#############################
### Europe DB Fuzzy Codes ###
#############################

# --------------------------------------------------------------------------------------------------------------- #
#### Working Directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(purrr)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR_trait.csv"))

names(df_EUR)
df_EUR <- select(df_EUR, -grep("microhab_|current_|^sal_|^res_|oxy_|dissem_|emerge_|unknown", names(df_EUR)))

# --------------------------------------------------------------------------------------------------------------- #
#### Transform Fuzzy Codes ####
# To compare the different databases the coding must be homogenous across all databases. Since fuzzy codes are
# more complex than binary codes the European codes are transformed to binaries.

# Steps for the transformation:
# 1. Replace all NAs with zeroes
# 2. Find the maximum value for each trait seperately. The maximum is stored in a new column
# 3. Transform each modality for each trait according to the maximum column for the respective trait. Each trait
# is changed to NAs and 1s according which modalitiy obtains the maximum

# Traits used:
# pH, feeding mode, locomotion, respiration, drift, life duration, size, voltinism, aquatic stages

names(df_EUR)
df_EUR[5:ncol(df_EUR)] %>% mutate_all(as.factor) %>% sapply(levels)


# ---- 1. Replace NAs with zeroes ----
cols <- purrr::rerun(length(df_EUR[5:ncol(df_EUR)]), 0) %>% purrr::set_names(names(df_EUR[5:ncol(df_EUR)]))
df_EUR <- replace_na(df_EUR, cols)


# ---- 2. Find the maximumg for each row and each trait ----
df_EUR <- df_EUR %>%
  mutate(ph_max = apply(df_EUR[grepl("ph_", names(df_EUR))], 1, max),
         temp_max = apply(df_EUR[grepl("temp_", names(df_EUR))], 1, max),
         feed_max = apply(df_EUR[grepl("feed_", names(df_EUR))], 1, max),
         loc_max = apply(df_EUR[grepl("locom_", names(df_EUR))], 1, max),
         resp_max = apply(df_EUR[grepl("resp_", names(df_EUR))], 1, max),
         drift_max = apply(df_EUR[grepl("dispersal_", names(df_EUR))], 1, max),
         life_max = apply(df_EUR[grepl("lifedur_", names(df_EUR))], 1, max),
         rep_max = apply(df_EUR[grepl("rep_", names(df_EUR))], 1, max),
         size_max = apply(df_EUR[grepl("size_", names(df_EUR))], 1, max),
         volt_max = apply(df_EUR[grepl("volt_", names(df_EUR))], 1, max),
         stage_max = apply(df_EUR[grepl("stage_", names(df_EUR))], 1, max))


# ---- 3. Transform modalities         
df_EUR <- df_EUR %>%
  
  # pH
  # Already binary
  
  # Temperature
  mutate(temp_very_cold = ifelse(temp_very_cold == temp_max & temp_very_cold != 0, 1, NA),
         temp_cold = ifelse(temp_cold == temp_max & temp_cold != 0, 1, NA),
         temp_moderate = ifelse(temp_moderate == temp_max & temp_moderate != 0, 1, NA),
         temp_warm = ifelse(temp_warm == temp_max & temp_warm != 0, 1, NA),
         temp_eurytherm = ifelse(temp_eurytherm == temp_max & temp_eurytherm != 0, 1, NA)) %>%
  
  # Feeding mode
  mutate(feed_grazer = ifelse(feed_grazer == feed_max & feed_grazer != 0, 1, NA),
         feed_miner = ifelse(feed_miner == feed_max & feed_miner != 0, 1, NA),
         feed_xylo = ifelse(feed_xylo == feed_max & feed_xylo != 0, 1, NA),
         feed_shred = ifelse(feed_shred == feed_max & feed_shred != 0, 1, NA),
         feed_gath = ifelse(feed_gath == feed_max & feed_gath != 0, 1, NA),
         feed_active_filter = ifelse(feed_active_filter == feed_max & feed_active_filter != 0, 1, NA),
         feed_passive_filter = ifelse(feed_passive_filter == feed_max & feed_passive_filter != 0, 1, NA),
         feed_predator = ifelse(feed_predator == feed_max & feed_predator != 0, 1, NA),
         feed_parasite = ifelse(feed_parasite == feed_max & feed_parasite != 0, 1, NA),
         feed_other = ifelse(feed_other == feed_max & feed_other != 0, 1, NA)) %>%
  
  # Locomotion
  mutate(locom_swim_skate = ifelse(locom_swim_skate == loc_max & locom_swim_skate != 0, 1, NA),
         locom_swim_dive = ifelse(locom_swim_dive == loc_max & locom_swim_dive != 0, 1, NA),
         locom_burrow = ifelse(locom_burrow == loc_max & locom_burrow != 0, 1, NA),
         locom_sprawl = ifelse(locom_sprawl == loc_max & locom_sprawl != 0, 1, NA),
         locom_sessil = ifelse(locom_sessil == loc_max & locom_sessil != 0, 1, NA),
         locom_other = ifelse(locom_other == loc_max & locom_other != 0, 1, NA)) %>%
  
  # Respiration
  # Already binary
  
  # Dispersal
  # Already binary
  
  # Life duration
  # Already binary
  
  # Aquatic stages
  mutate(stage_egg = ifelse(stage_egg == stage_max & stage_egg != 0, 1, NA),
         stage_larva = ifelse(stage_larva == stage_max & stage_larva != 0, 1, NA),
         stage_pupa = ifelse(stage_pupa == stage_max & stage_pupa != 0, 1, NA),
         stage_adult = ifelse(stage_adult == stage_max & stage_adult != 0, 1, NA)) %>%
  
  # Reproduction
  # Already binary
  
  # Size
  mutate(size_1 = ifelse(size_1 == size_max & size_1 != 0, 1, NA),
         size_2 = ifelse(size_2 == size_max & size_2 != 0, 1, NA),
         size_3 = ifelse(size_3 == size_max & size_3 != 0, 1, NA),
         size_4 = ifelse(size_4 == size_max & size_4 != 0, 1, NA),
         size_5 = ifelse(size_5 == size_max & size_5 != 0, 1, NA),
         size_6 = ifelse(size_6 == size_max & size_6 != 0, 1, NA),
         size_7 = ifelse(size_7 == size_max & size_7 != 0, 1, NA)) %>%
  
  # Voltinism
  # Already binary
  
  # Delete columns with the respective maximum
  select(-(ph_max:stage_max)) %>%
  
  # Return zeroes to NAs
  na_if(0)


# --------------------------------------------------------------------------------------------------------------- #
#### Final Table ####
# Write .csv
write.table(df_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR.csv", sep = ",")
