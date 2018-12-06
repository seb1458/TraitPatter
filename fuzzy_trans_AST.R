#################################
### Australian DB Fuzzy Codes ###
#################################

# --------------------------------------------------------------------------------------------------------------- #
#### Working Directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(purrr)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_AUS <- read.csv(file.path(path, "Australia", "macroinvertebrate_AUS_trait.csv"))

# Check levels of traits and their modalities
df_AUS[5:ncol(df_AUS)] %>% mutate_all(as.factor) %>% sapply(levels)
str(df_AUS)

# --------------------------------------------------------------------------------------------------------------- #
#### Transform Fuzzy Codes ####
# Some of the codes in the Australian database are fuzzy coded. To compare databases the coding must be uniform.
# So the fuzzy codes for the respective traits are transformed.

# Steps for the transformation:
# 1. Replace all NAs with zeroes
# 2. Find the maximum value for each row and each trait seperately. The maximum is stored in a new column
# 3. Transform each modality for each trait according to the maximum column for the respective trait. Each trait
# is changed to NAs and 1s according which modalitiy obtains the maximum

# Traits used:
# voltinism, feeding mode, respiration, locomotion, life duration, size

# The following traits are already binary coded: Reproduction, drift, pH, temperature preference

# The modalities for aquatic stages are basically binary coded. 
# Entries are NA or '3'. So coding is changed to NA and '1'


# ---- 1. Replace NAs with zeroes
cols <- purrr::rerun(length(df_AUS[5:ncol(df_AUS)]), 0) %>% purrr::set_names(names(df_AUS[5:ncol(df_AUS)]))
df_AUS <- replace_na(df_AUS, cols)

# Check levels again
df_AUS[5:ncol(df_AUS)] %>% mutate_all(as.factor) %>% sapply(levels)
str(df_AUS)


# ---- 2. Find the maximumg for each row and each trait
df_AUS <- df_AUS %>%
  mutate(feed_max = apply(df_AUS[grepl("feed_", names(df_AUS))], 1, max),
         sub_max = apply(df_AUS[grepl("sub_", names(df_AUS))], 1, max),
         resp_max = apply(df_AUS[grepl("resp_", names(df_AUS))], 1, max),
         life_max = apply(df_AUS[grepl("life_", names(df_AUS))], 1, max),
         size_max = apply(df_AUS[grepl("size_", names(df_AUS))], 1, max),
         volt_max = apply(df_AUS[grepl("volt", names(df_AUS))], 1, max),
         stage_max = apply(df_AUS[grepl("aquatic_", names(df_AUS))], 1, max))


# ---- 3. Transform modalities         
df_AUS <- df_AUS %>%
  
  # Voltinism
  mutate(volt1 = ifelse(volt1 == volt_max & volt1 != 0, 1, 0),
         volt2 = ifelse(volt2 == volt_max & volt2 != 0, 1, 0),
         volt3 = ifelse(volt3 == volt_max & volt3 != 0, 1, 0)) %>%
  
  # Feeding mode
  mutate(feed_gath = ifelse(feed_gath == feed_max & feed_gath != 0, 1, 0),
         feed_filt = ifelse(feed_filt == feed_max & feed_filt != 0, 1, 0),
         feed_scrap = ifelse(feed_scrap == feed_max & feed_scrap != 0, 1, 0),
         feed_pred = ifelse(feed_pred == feed_max & feed_pred != 0, 1, 0),
         feed_shred = ifelse(feed_shred == feed_max & feed_shred != 0, 1, 0),
         feed_para = ifelse(feed_para == feed_max & feed_para != 0, 1, 0)) %>%
  
  # Respiration
  mutate(resp_teg = ifelse(resp_teg == resp_max & resp_teg != 0, 1, 0),
         resp_gil = ifelse(resp_gil == resp_max & resp_gil != 0, 1, 0),
         resp_plas = ifelse(resp_plas == resp_max & resp_plas != 0, 1, 0),
         resp_atmos = ifelse(resp_atmos == resp_max & resp_atmos != 0, 1, 0),
         resp_spir = ifelse(resp_spir == resp_max & resp_spir != 0, 1, 0)) %>%
  
  # Substrate relation
  mutate(sub_burrow = ifelse(sub_burrow == sub_max & sub_burrow != 0, 1, 0),
         sub_climb = ifelse(sub_climb == sub_max & sub_climb != 0, 1, 0),
         sub_sprawl = ifelse(sub_sprawl == sub_max & sub_sprawl != 0, 1, 0),
         sub_cling = ifelse(sub_cling == sub_max & sub_cling != 0, 1, 0),
         sub_swim = ifelse(sub_swim == sub_max & sub_swim != 0, 1, 0),
         sub_skate = ifelse(sub_skate == sub_max & sub_skate != 0, 1, 0),
         sub_attached_temp = ifelse(sub_attached_temp == sub_max & sub_attached_temp != 0, 1, 0),
         sub_attached_perm = ifelse(sub_attached_perm == sub_max & sub_attached_perm != 0, 1, 0)) %>%
  
  # Life duration
  mutate(life_1 = ifelse(life_1 == life_max & life_1 != 0, 1, 0),
         life_2 = ifelse(life_2 == life_max & life_1 != 0, 1, 0)) %>%
  
  # Size
  mutate(size_s = ifelse(size_s == size_max & size_s != 0, 1, 0),
         size_m = ifelse(size_m == size_max & size_m != 0, 1, 0),
         size_l = ifelse(size_l == size_max & size_l != 0, 1, 0)) %>%
  
  # Delete columns with the respective maximum
  select(-(feed_max:stage_max))


# ---- 4. Replace values in the respiration trait with zeroes and ones
df_AUS <- df_AUS %>%
  mutate_at(vars(grep("aquatic", names(df_AUS), value = TRUE)), funs(replace(., . == 3, 1))) %>%
  
  # Replace zeroes with NA
  na_if(0)

# --------------------------------------------------------------------------------------------------------------- #
#### Final table ####

# Write .csv
write.table(df_AUS, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_AUS.csv", sep = ",")
