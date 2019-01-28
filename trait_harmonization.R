###########################
### Trait Harmonization ###
###########################

# --------------------------------------------------------------------------------------------------------------- #
#### Working Directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR.csv"))
NAM <- read.csv(file.path(path, "North America", "macroinvertebrate_NAM_trait.csv")) 
AUS <- read.csv(file.path(path, "Australia", "macroinvertebrate_AUS.csv")) 

fin_EUR <- select(EUR, order:Taxon)
fin_NAM <- select(NAM, Order:Taxa)
fin_AUS <- select(AUS, Order:Species)

# --------------------------------------------------------------------------------------------------------------- #
#### Traits ####
names(EUR); names(NAM); names(AUS)

# ------------------------------------------------------- # 
# ---- pH Preference ----
# Modalities:
# ph_acidic: Acidic pH
# ph_normal: Neutral to alkaline pH

# --- Europe
# pH_ind (indifferent) is dismissed from database
grep("^ph", names(EUR), ignore.case = TRUE, value = TRUE)

ph_EUR <- EUR %>% 
  mutate(ph_acidic = ph_acidic, ph_normal = ph_neutral_alk) %>%
  select(ph_acidic, ph_normal)

# --- North America
grep("^ph", names(NAM), ignore.case = TRUE, value = TRUE)

ph_NAM <- NAM %>% 
  mutate(ph_acidic = ph_acid, ph_normal = ph_norm) %>%
  select(ph_acidic, ph_normal)

# --- Australia
grep("^ph", names(AUS), ignore.case = TRUE, value = TRUE)

ph_AUS <- AUS %>% 
  mutate(ph_acidic = ph_acidic, ph_normal = ph_neut_alk) %>%
  select(ph_acidic, ph_normal)

# ------------------------------------------------------- # 
# ---- Feed Mode ----
# Modalities:
# feed_shredder: shredder (chewers, miners, xylophagus, herbivore piercers)
# feed_gatherer: collector gatherer (gatherers, detritivores)
# feed_filter: collector filterer (active filterers, passive filterers, absorbers)
# feed_scraper: scraper (grazer)
# feed_predator: predator
# feed_parasite: parasite


# --- Europe
grep("feed", names(EUR), ignore.case = TRUE, value = TRUE)

EUR %>% 
  select(grep("feed", names(EUR), ignore.case = TRUE, value = TRUE)) %>%
  sapply(table)

# QUESTION: Delete feed_other (128 entries)
feed_EUR <- EUR %>%
  mutate(feed_shredder = coalesce(feed_shred, feed_miner, feed_xylo),
         feed_gatherer = feed_gath,
         feed_filter = coalesce(feed_active_filter, feed_passive_filter),
         feed_scraper = feed_grazer,
         feed_predator = feed_predator,
         feed_parasite = feed_parasite) %>%
  select(feed_shredder, feed_gatherer, feed_filter, feed_scraper, feed_predator, feed_parasite)

# --- North America
grep("feed", names(NAM), ignore.case = TRUE, value = TRUE)

NAM %>% 
  select(grep("feed", names(NAM), ignore.case = TRUE, value = TRUE)) %>%
  sapply(table)

feed_NAM <- NAM %>%
  mutate_all(as.integer) %>%
  mutate(feed_shredder = feed_shred,
         feed_gatherer = feed_gath,
         feed_filter = coalesce(feed_filt, feed_abs),
         feed_scraper = coalesce(feed_scrap, feed_piercer),
         feed_predator = feed_pred,
         feed_parasite = feed_para) %>%
  select(feed_shredder:feed_parasite)

# --- Australia
# QUESTION: What to do with feed_herb?
grep("feed", names(AUS), ignore.case = TRUE, value = TRUE)  

AUS %>% 
  select(grep("feed", names(AUS), ignore.case = TRUE, value = TRUE)) %>%
  sapply(table)

feed_AUS <- AUS %>%
  mutate(feed_shredder = feed_shred,
         feed_gatherer = feed_gath,
         feed_filter = feed_filt,
         feed_scraper = feed_scrap,
         feed_predator = feed_pred,
         feed_parasite = feed_para) %>%
  select(feed_shredder:feed_parasite)

# ------------------------------------------------------- # 
# ---- Locomotion/Substrate Relation ----
# Modalities:
# loc_skate: skater
# loc_swim:  swimmer
# loc_burrow: burrower
# loc_sprawl: sprawler
# loc_sessil: sessil

# --- Europe
grep("locom", names(EUR), ignore.case = TRUE, value = TRUE)
  
locomotion_EUR <- EUR %>%
  mutate(loc_skate = locom_swim_skate,
         loc_swim = locom_swim_dive,
         loc_burrow = locom_burrow,
         loc_sprawl = locom_sprawl,
         loc_sessil = locom_sessil) %>%
  select(loc_skate:loc_sessil)

# --- North America
# plaktonic and swimmer become loc_swim
# What happens to loc_climber and loc_clinger
grep("locom", names(NAM), ignore.case = TRUE, value = TRUE)

locomotion_NAM <- NAM %>%
  mutate_all(as.integer) %>%
  mutate(loc_skate = locom_skater,
         loc_swim = coalesce(locom_swimmer, locom_planktonic),
         loc_burrow = locom_burrow,
         loc_sprawl = coalesce(locom_sprawler, locom_climber),
         loc_sessil = locom_sessil) %>%
  select(loc_skate:loc_sessil)

# --- Australia
# attached_temp and attached_perm become loc_sessil
# What happens to sub_climber and sub_clinger
grep("sub", names(AUS), ignore.case = TRUE, value = TRUE)

locomotion_AUS <- AUS %>%
  mutate_all(as.integer) %>%
  mutate(loc_skate = sub_skate,
         loc_swim = sub_swim,
         loc_burrow = sub_burrow,
         loc_sprawl = coalesce(sub_sprawl, sub_climb),
         loc_sessil = coalesce(sub_attached_temp, as.integer(sub_attached_perm))) %>%
  select(loc_skate:loc_sessil)

# ------------------------------------------------------- # 
# ---- Respiration ----
# Modalities:
# resp_tegument: cutaneous/tegument
# resp_gills: gills
# resp_spiracle: spiracle
# resp_plastron: plastron
# resp_atmospheric: atmospheric breathers

# --- Europe
# Vesicle and tapping as respiration modalities are not included
grep("resp", names(EUR), ignore.case = TRUE, value = TRUE)

resp_EUR <- EUR %>%
  mutate(resp_tegument = resp_teg,
         resp_gills = resp_gil,
         resp_spiracle = resp_spi,
         resp_plastron = resp_pls,
         resp_atmospheric = resp_sur) %>%
  select(resp_tegument:resp_atmospheric)

# --- North America
grep("resp", names(NAM), ignore.case = TRUE, value = TRUE)

resp_NAM <- NAM %>%
  mutate(resp_tegument = resp_teg,
         resp_gills = resp_gil,
         resp_spiracle = resp_spi,
         resp_plastron = resp_pls,
         resp_atmospheric = resp_atm) %>%
  select(resp_tegument:resp_atmospheric)

# --- Australia
grep("resp", names(AUS), ignore.case = TRUE, value = TRUE)

resp_AUS <- AUS %>%
  mutate(resp_tegument = resp_teg,
         resp_gills = resp_gil,
         resp_spiracle = resp_spir,
         resp_plastron = resp_plas,
         resp_atmospheric = resp_atmos) %>%
  select(resp_tegument:resp_atmospheric)

# ------------------------------------------------------- # 
# ---- Drift ----
# EUR with fewer levels in DISPERSAL (!). If other DBs are tranformed maybe: drift1 = high + medium, drift2 = low
# Modalities:
# drift_high
# drift_low

# --- Europe
grep("dispersal", names(EUR), ignore.case = TRUE, value = TRUE)

drift_EUR <- EUR %>%
  mutate(drift_low = dispersal_low,
         drift_high = dispersal_high) %>%
  select(drift_low:drift_high)

# --- North America
grep("drift", names(NAM), ignore.case = TRUE, value = TRUE)

drift_NAM <- NAM %>%
  mutate_all(as.integer) %>%
  mutate(drift_low = drift1,
         drift_high = coalesce(drift2, drift3)) %>%
  select(drift_low:drift_high)

# --- Australia
grep("drift", names(AUS), ignore.case = TRUE, value = TRUE)
AUS <- rename(AUS, drift_l = drift_low, drift_m = drift_medium, drift_h = drift_high)

drift_AUS <- AUS %>%
  mutate_all(as.integer) %>%
  mutate(drift_low = drift_l, 
         drift_high = coalesce(drift_m), drift_h) %>%
  select(drift_low:drift_high)

# ------------------------------------------------------- # 
# ---- Life Duration ----
# Assumption: life duration in NAM DB can be divided in "< month" with life_hours, life_days and life_weeks.
# Problem: EUR DB with coarse resolution 
# Modalities:
# life1: < 1 month
# life2: > 1 month


# --- Europe
grep("life", names(EUR), ignore.case = TRUE, value = TRUE)

life_EUR <- EUR %>%
  mutate_all(as.integer) %>%
  mutate(life1 = NA,
         life2 = coalesce(lifedur_one_yr, lifedur_more_yr)) %>%
  select(life1:life2)

# --- North America
grep("life", names(NAM), ignore.case = TRUE, value = TRUE)

life_NAM <- NAM %>%
  mutate_all(as.integer) %>%
  mutate(life1 = life_hours,
         life2 = coalesce(life_days, life_weeks, life_months)) %>%
  select(life1:life2)

# --- Australia
grep("life", names(AUS), ignore.case = TRUE, value = TRUE)

life_AUS <- AUS %>%  
  mutate(life1 = life_1,
         life2 = life_2) %>%
  select(life1:life2)


# ------------------------------------------------------- # 
# ---- Size ----
# Due to problems in converting the different categories, the EU/Tachet DB is converted differently.

# Modalities:
# size_small: size < 9 mm (EU: size < 10 mm)
# size_medium: 9 mm < size > 16 mm (EU: 10 mm < size > 20 mm)
# size_large: size > 16 mm (EU: size > 20 mm)


# --- Europe
grep("size", names(EUR), ignore.case = TRUE, value = TRUE)

size_EUR <- EUR %>%
  mutate_all(as.integer) %>%
  mutate(size_small = coalesce(size_1, size_2, size_3),
         size_medium = size_4,
         size_large = coalesce(size_5, size_6, size_7)) %>%
  select(size_small:size_large)


# --- North America
grep("size", names(NAM), ignore.case = TRUE, value = TRUE)

size_NAM <- NAM %>%  
  mutate(size_small = size1,
         size_medium = size2,
         size_large = size3) %>%
  select(size_small:size_large)

# --- Australia
grep("size", names(AUS), ignore.case = TRUE, value = TRUE)

size_AUS <- AUS %>% 
  mutate(size_small = size_s,
         size_medium = size_m,
         size_large = size_l) %>%
  select(size_small:size_large)

# ------------------------------------------------------- # 
# ---- Voltinism ----
# Modalities:
# volt1: < 1 reproductive cycle per year
# volt2: = 1 reproductive cycle per year
# volt3: > 1 reproductive cycle per year

# --- Europe
# What to do with fuzzy codes?
grep("volt", names(EUR), ignore.case = TRUE, value = TRUE)

volt_EUR <- EUR %>%
  mutate(volt1 = volt_1,
         volt2 = volt_2,
         volt3 = volt_3) %>%
  select(volt1:volt3)

# --- North America
grep("volt", names(NAM), ignore.case = TRUE, value = TRUE)

volt_NAM <- NAM %>%
  mutate(volt1 = volt_1,
         volt2 = volt_2,
         volt3 = volt_3) %>%
  select(volt1:volt3)

# --- Australia
volt_AUS <- AUS %>%
  select(grep("volt", names(AUS), ignore.case = TRUE, value = TRUE))

# ------------------------------------------------------- # 
# ---- Aquatic Stages ----
# Modalities: 
# stage1: egg
# stage2: larva and/or nymph
# stage3: pupa
# stage4: adult

# Aquatic stages for larvae and nymphae of EUR and AUS are combined into one column


# --- Europe
grep("stage", names(EUR), ignore.case = TRUE, value = TRUE)

stage_EUR <- EUR %>%
  mutate_all(as.integer) %>%
  mutate(stage1 = stage_egg,
         stage2 = coalesce(stage_larva, stage_nymph),
         stage3 = stage_pupa,
         stage4 = stage_adult) %>%
  select(stage1:stage4)

# --- North America
grep("stage", names(NAM), ignore.case = TRUE, value = TRUE)

stage_NAM <- NAM %>%
  mutate(stage1 = stage_ln,
         stage2 = stage_e,
         stage3 = stage_p,
         stage4 = stage_a) %>%
  select(stage1:stage4)

# --- Australia
grep("aquatic", names(AUS), ignore.case = TRUE, value = TRUE)

stage_AUS <- AUS %>%
  mutate_all(as.integer) %>%
  rename(stage_egg = aquatic_egg,
         stage_larva = aquatic_larva,
         stage_nymph = aquatic_nymph,
         stage_pupa = aquatic_pupa,
         stage_adult = aquatic_adult) %>%
  mutate(stage1 = stage_egg,
         stage2 = coalesce(stage_larva, stage_nymph),
         stage3 = stage_pupa,
         stage4 = stage_adult) %>%
  select(stage1:stage4)


# ------------------------------------------------------- # 
# ---- Reproduction ----
# Modalities
# rep_aqu: Reproduction via aquatic eggs
# rep_ter: Reproduction via terrestric eggs
# rep_ovo: Reproduction via ovoviparity


# --- Europe
grep("rep", names(EUR), ignore.case = TRUE, value = TRUE)

rep_EUR <- EUR %>%
  mutate_all(as.integer) %>%
  mutate(rep_aqu = coalesce(rep_egg_cem_iso, rep_egg_free_iso, rep_clutch_free, rep_clutch_fixed),
         rep_ter = rep_clutch_veg,
         rep_ovo = coalesce(rep_parasitic, rep_ovovipar)) %>%
  select(rep_aqu:rep_ovo)

# --- North America
grep("rep", names(NAM), ignore.case = TRUE, value = TRUE)

rep_NAM <- select(NAM, rep_aqu:rep_ovo)

# --- Australia 
grep("rep", names(AUS), ignore.case = TRUE, value = TRUE)

rep_AUS <- select(AUS, rep_aqu:rep_ovo)

# ------------------------------------------------------- # 
# ---- Temperature ----
# Modalities

# --- Europe
grep("temp", names(EUR), ignore.case = TRUE, value = TRUE)

temp_EUR <- EUR %>%
  rename(temp_ind = temp_eurytherm, temp_mod = temp_moderate) %>%
  select(temp_very_cold:temp_ind)

# --- North America
grep("temp", names(NAM), ignore.case = TRUE, value = TRUE)

temp_NAM <- select(NAM, temp_very_cold:temp_ind)
         
         
# --- Australia 
grep("temp_", names(AUS), ignore.case = TRUE, value = TRUE)

temp_AUS <- AUS %>%
  rename(temp_ind = temp_eurytherm) %>%
  select(temp_very_cold:temp_ind)
         

# --------------------------------------------------------------------------------------------------------------- #
#### Final Table ####
EUR <- cbind(fin_EUR, ph_EUR, feed_EUR, locomotion_EUR, resp_EUR, drift_EUR,
             life_EUR, size_EUR, volt_EUR, stage_EUR, rep_EUR, temp_EUR)
NAM <- cbind(fin_NAM, ph_NAM, feed_NAM, locomotion_NAM, resp_NAM, drift_NAM,
             life_NAM, size_NAM, volt_NAM, stage_NAM, rep_NAM, temp_NAM)
AUS <- cbind(fin_AUS, ph_AUS, feed_AUS, locomotion_AUS, resp_AUS, drift_AUS,
             life_AUS, size_AUS, volt_AUS, stage_AUS, rep_AUS, temp_AUS)

# Write .csv
write.table(EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR_harmonized.csv", sep = ",")
write.table(NAM, file = "~/Schreibtisch/Thesis/data/North America/macroinvertebrate_NAM_harmonized.csv", sep = ",")
write.table(AUS, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_harmonized.csv", sep = ",")
