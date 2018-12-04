#############################
### Europe DB Fuzzy Codes ###
#############################

# ------------------------------------------------------------------------------------------------------------------------- #
#### Working Directory ####
path <- "~/Schreibtisch/Thesis/data"


# ------------------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(purrr)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR_trait.csv"))

names(df_EUR)
df_EUR <- select(df_EUR, -grep("microhab_|current_|temp_|^sal_|^res_|oxy_|dissem_|emerge_|rep_|unknown", names(df_EUR)))

# ------------------------------------------------------------------------------------------------------------------------- #
#### Transform Fuzzy Codes ####
# To compare the different databases the coding must be homogenous across all databases. Since fuzzy codes are
# more complex than binary codes the European codes are transformed to binaries.

# Steps for the transformation
# 1. Replace all NAs with zeroes
# 2. Find the maximum value for each row and each trait seperately. The maximum is stored in a new column
# 3. Transform each modality for each trait according to the maximum column for the respective trait. Each trait
# is changed to NAs and 1s according which modalitiy obtains the maximum

# Traits used:
# pH, feeding mode, locomotion, respiration, drift, life duration, size, voltinism, aquatic stages

names(df_EUR)

# ---- 1. Replace NAs with zeroes ----
cols <- purrr::rerun(length(df_EUR[6:ncol(df_EUR)]), 0) %>% purrr::set_names(names(df_EUR[6:ncol(df_EUR)]))
df_EUR <- replace_na(df_EUR, cols)


# ---- 2. Find the maximumg for each row and each trait ----
df_EUR <- df_EUR %>%
  mutate(ph_max = apply(df_EUR[grepl("ph_", names(df_EUR))], 1, max),
         feed_max = apply(df_EUR[grepl("feed_", names(df_EUR))], 1, max),
         loc_max = apply(df_EUR[grepl("locom_", names(df_EUR))], 1, max),
         resp_max = apply(df_EUR[grepl("resp_", names(df_EUR))], 1, max),
         drift_max = apply(df_EUR[grepl("dispersal_", names(df_EUR))], 1, max),
         life_max = apply(df_EUR[grepl("lifedur_", names(df_EUR))], 1, max),
         size_max = apply(df_EUR[grepl("size_", names(df_EUR))], 1, max),
         volt_max = apply(df_EUR[grepl("volt_", names(df_EUR))], 1, max),
         stage_max = apply(df_EUR[grepl("stage_", names(df_EUR))], 1, max))


# ---- 3. Transform modalities         
df_EUR <- df_EUR %>%
  
  # pH
  mutate(ph_acidic = ifelse(ph_acidic == ph_max & ph_acidic != 0, 1, NA),
         ph_neutral_alk = ifelse(ph_neutral_alk == ph_max & ph_neutral_alk != 0, 1, NA),
         ph_ind = ifelse(ph_ind == ph_max & ph_ind != 0, 1, NA)) %>%
  
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
  mutate(resp_teg = ifelse(resp_teg == resp_max & resp_teg != 0, 1, NA),
         resp_gil = ifelse(resp_gil == resp_max & resp_gil != 0, 1, NA),
         resp_pls = ifelse(resp_pls == resp_max & resp_pls != 0, 1, NA),
         resp_spi = ifelse(resp_spi == resp_max & resp_spi != 0, 1, NA),
         resp_ves = ifelse(resp_ves == resp_max & resp_ves != 0, 1, NA),
         resp_tap = ifelse(resp_tap == resp_max & resp_tap != 0, 1, NA),
         resp_sur = ifelse(resp_sur == resp_max & resp_sur != 0, 1, NA)) %>%
  
  # Dispersal
  mutate(dispersal_high = ifelse(dispersal_high == drift_max & dispersal_high != 0, 1, NA),
         dispersal_low = ifelse(dispersal_low == drift_max & dispersal_low != 0, 1, NA)) %>%
  
  # Life duration
  mutate(lifedur_one_yr = ifelse(lifedur_one_yr == life_max & lifedur_one_yr != 0, 1, NA),
         lifedur_more_yr = ifelse(lifedur_more_yr == life_max & lifedur_more_yr != 0, 1, NA)) %>%
  
  # Aquatic stages
  mutate(stage_egg = ifelse(stage_egg == stage_max & stage_egg != 0, 1, NA),
         stage_larva = ifelse(stage_larva == stage_max & stage_larva != 0, 1, NA),
         stage_nymph = ifelse(stage_nymph == stage_max & stage_nymph != 0, 1, NA),
         stage_pupa = ifelse(stage_pupa == stage_max & stage_pupa != 0, 1, NA),
         stage_adult = ifelse(stage_adult == stage_max & stage_adult != 0, 1, NA)) %>%
  
  # Size
  mutate(size_1 = ifelse(size_1 == size_max & size_1 != 0, 1, NA),
         size_2 = ifelse(size_2 == size_max & size_2 != 0, 1, NA),
         size_3 = ifelse(size_3 == size_max & size_3 != 0, 1, NA),
         size_4 = ifelse(size_4 == size_max & size_4 != 0, 1, NA),
         size_5 = ifelse(size_5 == size_max & size_5 != 0, 1, NA),
         size_6 = ifelse(size_6 == size_max & size_6 != 0, 1, NA),
         size_7 = ifelse(size_7 == size_max & size_7 != 0, 1, NA)) %>%
  
  # Voltinism
  mutate(volt_1 = ifelse(volt_1 == volt_max & volt_1 != 0, 1, NA),
         volt_2 = ifelse(volt_2 == volt_max & volt_2 != 0, 1, NA),
         volt_3 = ifelse(volt_3 == volt_max & volt_3 != 0, 1, NA)) %>%
  
  # Delete columns with the respective maximum
  select(-(ph_max:stage_max)) %>%
  
  # Return zeroes to NAs
  na_if(0)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Final Table ####

# Write .csv
write.table(df_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR.csv", sep = ",")
