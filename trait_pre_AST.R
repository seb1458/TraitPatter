#### Preparation: Australia database ####
# ---- Trait Information Preprocessing ----

# ------------------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# ------------------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(readxl)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Load data ####
df_AUS <- read.csv(file.path(path, "Australia", "macroinvertebrate_AUS_tax.csv"), stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Query traits to keep ####
# Database Schäfer
(names_shafer <- grep("shafer", names(df_AUS), ignore.case = TRUE, value = TRUE))

keep_shafer <- c(grep("(mS/cm)_Shafer|per_year_Shafer|type_Shafer|capacity_Shafer|group_Shafer|number_Shafer|Respiration_Shafer",
                      names_shafer, value = TRUE, ignore.case = TRUE))

# Keeping: Salinity toelrance, number of generations per year, reproduction type, dispersal capacity, max body size, respiration


# Database gbr
(names_gbr <- grep("gbr", names(df_AUS), ignore.case = TRUE, value = TRUE))

keep_gbr <- c(grep("(ms/cm)_bugs_gbr|per_year_bugs_gbr|type_bugs_gbr|capacity_bugs_gbr|group_bugs_gbr|number_bugs_gbr|Respiration_bugs_gbr",
                   names_gbr, value = TRUE))

# Keeping: Salinity toelrance, number of generations per year, reproduction type, dispersal capacity, max body size, respiration

dismiss_gbr <- grep("gbr", names(df_AUS), ignore.case = TRUE, value = TRUE)[!grep("gbr", names(df_AUS), ignore.case = TRUE, value = TRUE) %in% keep_gbr]


# Database VicEPA
(names_vicepa <- grep("VicEPA", names(df_AUS), ignore.case = TRUE, value = TRUE))

keep_vicepa <- c(grep("max|voltinism|attach|feeding|respiration|resisant|total|dispersal|aquatic", names_vicepa, value = TRUE, ignore.case = TRUE))

# Keeping: Body size, voltinism, substrate relation, feeding group, respiration, resistant form, total life duration, dispersal, aquatic stages


# Database Chessman
names_chessman <- grep("chessman", names(df_AUS), ignore.case = TRUE, value = TRUE)
dismiss_chessman <- grep("ref|comment", names_chessman, ignore.case = TRUE, value = TRUE)
(names_chessman <- names_chessman[!names_chessman %in% dismiss_chessman])

keep_chessman <- c(grep("thermophily|shredder|scraper|predator|gatherer|filterer|aquatic_stages|length|ph_minimum",
                        names_chessman, value = TRUE, ignore.case = TRUE))
# Keep: Thermophily, feeding group, respiration, body size, ph preference


# Database Bowte
(names_bowte <- grep("botwe", names(df_AUS), ignore.case = TRUE, value = TRUE))

keep_bowte <- c(grep("Volt|Life|Disp|Drft|Armr|Resp|Size|Ther|Habi|Trop|Sal|Rep",
                     names_bowte, value = TRUE))
# Keep: Voltinism, life duration, dispersal, drift, armor, respiration, body size, thermophily, habitat preference, trophic status, salinity preference, reproduction


# Database Maxwell
(names_maxwell <- grep("maxwell", names(df_AUS), ignore.case = TRUE, value = TRUE))

keep_maxwell <- c(grep("EC|repro|resp|volt|disp|C_Maxwell|P_Maxwell|SH|C,SH|SC|PA|C,SC|F",
                       names_maxwell, value = TRUE))
# Keep: Salinity preference, reproduction, respiration, voltinism, dispersal, feeding group


# Database Marchant
(names_marchant <- grep("marchant", names(df_AUS), ignore.case = TRUE, value = TRUE))

keep_marchant <- names_marchant

# Final columns to keep
fin_AUS <- df_AUS %>%
  select(id_join,
         keep_shafer,
         keep_gbr,
         keep_vicepa,
         keep_chessman,
         keep_bowte,
         keep_maxwell,
         keep_marchant)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Format trait information ####
# ---- Voltinism ----
# Explanation
# 1. volt1: < 1 reproductive cycle per year
# 2. volt2: = 1 reproductive cycle per year
# 3. volt3: > 1 reproductive cycle per year

# Transforming Shafer and gbr data to described modalities 
# For both transforming processes the maximal amount of generations per year was anticipated.
# E.g. number of generations per year = 0.5-1 translates to 1 generation per year (modality: volt2)
fin_AUS <- fin_AUS %>%
  mutate(volt2_shafer = ifelse(grepl("1$", df_AUS$Number_of_generations_per_year_Shafer), 1, 0),
         volt3_shafer = ifelse(!grepl("1$", df_AUS$Number_of_generations_per_year_Shafer) & !is.na(df_AUS$Number_of_generations_per_year_Shafer), 1, 0))
# No species with voltinism < 1 
# "1$|NA" anything else besides 0.5-1, 1 and NA

fin_AUS <- fin_AUS %>%
  mutate(volt1_gbr = ifelse(grepl("5$", df_AUS$Number_of_generations_per_year_bugs_gbr), 1, 0),
         volt2_gbr = ifelse(grepl("0.5-1|<=|^1$", df_AUS$Number_of_generations_per_year_bugs_gbr), 1, 0),
         volt3_gbr = ifelse(!grepl("5$|0.5-1|<=|^1$", df_AUS$Number_of_generations_per_year_bugs_gbr) & !is.na(df_AUS$Number_of_generations_per_year_bugs_gbr), 1, 0))


# Starting with the data from Botwe, and adding more information from the other databases
# Chessman has no voltinism data 
# Maxwell has no data for voltinism = 1
# Shafer has no data for voltinism < 1

(volt_names <- grep("volt", names(fin_AUS), value = TRUE, ignore.case = TRUE))

voltinism <- select(fin_AUS, volt_names)

voltinism <- voltinism %>%
  na_if(0) %>%
  mutate(volt1 = Volt1_botwe, volt2 = Volt2_botwe, volt3 = Volt3_botwe) %>%
  mutate(volt1 = ifelse(is.na(volt1), volt1_Maxwell, volt1),
         volt2 = ifelse(is.na(volt3), volt2_Maxwell, volt2),
         volt3 = ifelse(is.na(volt3), volt3_Maxwell, volt3),
         volt3 = ifelse(is.na(volt3), volt4_Maxwell, volt3)) %>%
  mutate(volt1 = ifelse(is.na(volt1), Voltinism_less_than_1_VicEPA, volt1),
         volt2 = ifelse(is.na(volt2), Voltinism_1_VicEPA, volt2),
         volt3 = ifelse(is.na(volt3), Voltinism_2_VicEPA, volt3),
         volt3 = ifelse(is.na(volt3), Voltinism_more_than_2_VicEPA, volt3)) %>%
  mutate(volt2 = ifelse(is.na(volt2), volt2_shafer, volt2),
         volt3 = ifelse(is.na(volt3), volt3_shafer, volt3)) %>%
  mutate(volt1 = ifelse(is.na(volt1), volt1_gbr, volt1),
         volt2 = ifelse(is.na(volt2), volt2_gbr, volt2),
         volt3 = ifelse(is.na(volt3), volt3_gbr, volt3)) %>%
  select(volt1:volt3) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# ---- Reproduction ----
# Explanation
# 1. rep1: aquatic eggs
# 2. rep2: terrestrial eggs
# 3. rep3: ovoviparity

# Transforming Shafer, GBR and Maxwell data to described modalities 
# NOTE: "Eggs inside plants/objects in or near water" could also be terrestrial
# In data from shafer only "some taxa terrestrial eggs" was transformed to rep2
fin_AUS <- fin_AUS %>%
  mutate(rep1_shafer = ifelse(grepl("aquatic|plants|substrate|free", fin_AUS$Reproduction_type_Shafer, ignore.case = TRUE), 1, 0),
         rep2_shafer = ifelse(grepl("terrestrial|above", fin_AUS$Reproduction_type_Shafer, ignore.case = TRUE), 1, 0),
         rep3_shafer = ifelse(grepl("ovo", fin_AUS$Reproduction_type_Shafer, ignore.case = TRUE), 1, 0))

# In data from GBR everything with "terrestrial" was transformed to rep2
fin_AUS <- fin_AUS %>%
  mutate(rep1_gbr = ifelse(grepl("aquatic|shallow|stones|algae|adults|plants|substrate|free", fin_AUS$Reproduction_type_bugs_gbr, ignore.case = TRUE), 1, 0),
         rep2_gbr = ifelse(grepl("terrestrial", fin_AUS$Reproduction_type_bugs_gbr, ignore.case = TRUE), 1, 0),
         rep3_gbr = ifelse(grepl("ovo", fin_AUS$Reproduction_type_bugs_gbr, ignore.case = TRUE), 1, 0))

# In data from Maxwell rep4 (eggs attached to male adults) was missing.

(rep_names <- grep("rep", names(fin_AUS), value = TRUE, ignore.case = TRUE))

reproduction <- select(fin_AUS, rep_names)

reproduction <- reproduction %>%
  na_if(0) %>%
  mutate(rep1 = Rep1_botwe, rep2 = Rep2_botwe, rep3 = Rep3_botwe) %>%
  mutate(rep1 = ifelse(is.na(rep1), repro1_Maxwell, rep1),
         rep2 = ifelse(is.na(rep2), repro2_Maxwell, rep2),
         rep3 = ifelse(is.na(rep3), repro3_Maxwell, rep3)) %>%
  mutate(rep1 = ifelse(is.na(rep1), rep1_shafer, rep1),
         rep2 = ifelse(is.na(rep2), rep2_shafer, rep2),
         rep3 = ifelse(is.na(rep3), rep3_shafer, rep3)) %>%
  mutate(rep1 = ifelse(is.na(rep1), rep1_gbr, rep1),
         rep2 = ifelse(is.na(rep2), rep2_gbr, rep2),
         rep3 = ifelse(is.na(rep3), rep3_gbr, rep3)) %>%
  select(rep1:rep3) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# ---- Feed Mode ----
# Explanation
# 1. feed_gatherer: Collector-gatherer
# 2. feed_filter: Collector-filterer
# 3. feed_herbivor: Herbivor (scraper, shredder, piercer)
# 4. feed_predator: Predator (piercer, engulfer)
# 5. feed_shredder: Shredder (detritivore)
# 6. feed_parasite: Parasite

# Transforming Shafer, GBR and Maxwell data to described modalities 

# --- Shafer
# Only "Detritivores" was assigned to modality feed5.
fin_AUS <- fin_AUS %>%
  mutate(feed3_shafer = ifelse(grepl("herbivor", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, 0),
         feed4_shafer = ifelse(grepl("predator", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, 0),
         feed5_shafer = ifelse(grepl("detritivores$", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, 0),
         feed6_shafer = ifelse(grepl("parasite", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, 0))

# --- GBR
# Transformation of "Scavenger" unclear
# Transformation of "larvea predators, adults herbivores" unclear
# Snails -> Predators
fin_AUS <- fin_AUS %>%
  mutate(feed3_gbr = ifelse(grepl("algae|herbivor|plants", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, 0),
         feed4_gbr = ifelse(grepl("predator|snails", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, 0),
         feed5_gbr = ifelse(grepl("detritivores$", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, 0),
         feed6_gbr = ifelse(grepl("parasite", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, 0))

# --- Maxwell
# feed1 = Collector + Collector/Shredder + Collector/Scraper ("C_Maxwell", "C,SH_Maxwell", "C,SC_Maxwell")
# feed2 = Filter feeder ("F_Maxwell")
# feed3 = Scraper (SC_Maxwell)
# feed4 = Predator (P_Maxwell)
# feed5 = Shredder (SH_Maxwell)
# feed6 = Parasite (PA_Maxwell)
fin_AUS <- fin_AUS %>%
  mutate(feed1_maxwell = C_Maxwell,
         feed2_maxwell = F_Maxwell,
         feed3_maxwell = SC_Maxwell,
         feed4_maxwell = P_Maxwell,
         feed5_maxwell = SH_Maxwell,
         feed6_maxwell = PA_Maxwell)

fin_AUS$feed1_maxwell <- ifelse(grepl("0", fin_AUS$feed1_maxwell) | is.na(fin_AUS$feed1_maxwell), fin_AUS$C.SH_Maxwell, fin_AUS$feed1_maxwell)
fin_AUS$feed1_maxwell <- ifelse(grepl("0", fin_AUS$feed1_maxwell) | is.na(fin_AUS$feed1_maxwell), fin_AUS$C.SC_Maxwell, fin_AUS$feed1_maxwell)

(feed_names <- grep("feed|trop|marchant", names(fin_AUS), value = TRUE, ignore.case = TRUE))

feeding <- select(fin_AUS, feed_names)

feeding <- feeding %>%
  na_if(0) %>%
  mutate(feed_gatherer = Trop1_botwe, feed_filter = Trop2_botwe, feed_herbivore = Trop3_botwe,
         feed_predator = Trop4_botwe, feed_shredder = Trop5_botwe, feed_parasite = feed6_maxwell) %>%
  mutate(feed_gatherer = ifelse(is.na(feed_gatherer), feed1_maxwell, feed_gatherer),
         feed_filter = ifelse(is.na(feed_filter), feed2_maxwell, feed_filter),
         feed_herbivore = ifelse(is.na(feed_herbivore), feed3_maxwell, feed_herbivore),
         feed_predator = ifelse(is.na(feed_predator), feed4_maxwell, feed_predator),
         feed_shredder = ifelse(is.na(feed_shredder), feed5_maxwell, feed_shredder)) %>%
  mutate(feed_gatherer = ifelse(is.na(feed_gatherer), Feeding_filterers_VicEPA, feed_gatherer),
         feed_filter = ifelse(is.na(feed_filter), Feeding_deposit_grazer_VicEPA, feed_filter),
         feed_herbivore = ifelse(is.na(feed_herbivore), Feeding_scrapers_VicEPA, feed_herbivore),
         feed_predator = ifelse(is.na(feed_predator), Feeding_predators_VicEPA, feed_predator),
         feed_shredder = ifelse(is.na(feed_shredder), Feeding_shredders_VicEPA, feed_shredder),
         feed_parasite = ifelse(is.na(feed_parasite), Feeding_parasite_VicEPA, feed_parasite)) %>%
  mutate(feed_gatherer = ifelse(is.na(feed_gatherer), Filterer_.proportion_of_feeding._fam_Chessman2017, feed_gatherer),
         feed_filter = ifelse(is.na(feed_filter), Gatherer_.proportion_of_feeding._fam_Chessman2017, feed_filter),
         feed_herbivore = ifelse(is.na(feed_herbivore), Scraper_.proportion_of_feeding._fam_Chessman2017, feed_herbivore),
         feed_predator = ifelse(is.na(feed_predator), Predator_.proportion_of_feeding._fam_Chessman2017, feed_predator),
         feed_shredder = ifelse(is.na(feed_shredder), Shredder_.proportion_of_feeding._fam_Chessman2017, feed_shredder)) %>%
  mutate(feed_gatherer = ifelse(is.na(feed_gatherer), Filterer..proportion.of.feeding._genus_Chessman2017, feed_gatherer),
         feed_filter = ifelse(is.na(feed_filter), Gatherer..proportion.of.feeding._genus_Chessman2017, feed_filter),
         feed_herbivore = ifelse(is.na(feed_herbivore), Scraper..proportion.of.feeding._genus_Chessman2017, feed_herbivore),
         feed_predator = ifelse(is.na(feed_predator), Predator..proportion.of.feeding._genus_Chessman2017, feed_predator),
         feed_shredder = ifelse(is.na(feed_shredder), Shredder..proportion.of.feeding._genus_Chessman2017, feed_shredder)) %>%
  mutate(feed_herbivore = ifelse(is.na(feed_herbivore), feed3_shafer, feed_herbivore),
         feed_predator = ifelse(is.na(feed_predator), feed4_shafer, feed_predator),
         feed_shredder = ifelse(is.na(feed_shredder), feed5_shafer, feed_shredder),
         feed_parasite = ifelse(is.na(feed_parasite), feed6_shafer, feed_parasite)) %>%
  mutate(feed_herbivore = ifelse(is.na(feed_herbivore), feed3_gbr, feed_herbivore),
         feed_predator = ifelse(is.na(feed_predator), feed4_gbr, feed_predator),
         feed_shredder = ifelse(is.na(feed_shredder), feed5_gbr, feed_shredder),
         feed_parasite = ifelse(is.na(feed_parasite), feed6_gbr, feed_parasite)) %>%
  mutate(feed_shredder = ifelse(is.na(feed_shredder), shredder_Marchant, feed_shredder),
         feed_gatherer = ifelse(is.na(feed_gatherer), detritivore_Marchant, feed_gatherer),
         feed_filter = ifelse(is.na(feed_filter), filterer_Marchant, feed_filter),
         feed_herbivore = ifelse(is.na(feed_herbivore), grazer_Marchant, feed_herbivore),
         feed_predator = ifelse(is.na(feed_predator), predator_Marchant, feed_predator))
  


# Last changes
# 1. Assign missing piercer-group of VicEPA to feed3
feeding$feed_herbivore <- ifelse(grepl("0", feeding$feed_herbivore), feeding$Feeding_piercers_VicEPA, feeding$feed_herbivore)

# 2. Change Chessman values (0.25, 0.3333, 0.5) to 1
replace <- c(0.2, 0.25, 0.333333333333333, 0.5)
for (i in seq_along(feeding)) {
  feeding[[i]][feeding[[i]] %in% replace] <- 1
}
feeding %>% mutate_all(as.factor) %>% sapply(levels)

feeding <- feeding %>%
  select(feed_gatherer:feed_parasite) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

# Note: All shredder modalities were assigned to feed3 (hebivor)
# Missing: "Feeding_absorber_VicEPA", 


# ---- Respiration ----
# Explanation
# 1. resp_tegument: Tegument
# 2. resp_gills: Gills
# 3. resp_plastron: Plastron
# 4. resp_atmospheric: Air/Atmospheric
# 5. resp_spiracle: Spiracle

# --- Shafer
# Shafer contains "pneumostome" (2 times) which is the respiratory system for (land)snails. Keep as new modality or dismiss?
# Shafer contains "air-breathing" (2 times). Keep as new modality or dismiss? (NOTE: Included as resp4)
# Modality "Gills (larvae), air-breathing (adult)" transformed to resp2
# Modality "Plastron and gills" transformed to resp3
fin_AUS <- fin_AUS %>%
  mutate(resp1_shafer.new = ifelse(grepl("cutaneous", fin_AUS$Respiration_Shafer, ignore.case = TRUE), 1, 0),
         resp2_shafer.new = ifelse(grepl("Gills", fin_AUS$Respiration_Shafer), 1, 0),
         resp3_shafer.new = ifelse(grepl("plastron", fin_AUS$Respiration_Shafer, ignore.case = TRUE), 1, 0),
         resp4_shafer.new = ifelse(grepl("breathing", fin_AUS$Respiration_Shafer, ignore.case = TRUE), 1, 0))


# --- GBR
# GBR contains "pneumostome" (6 times) which is the respiratory system for (land)snails. Keep as new modality or dismiss?
# GBR contains "air-breathing" (~150 times). Keep as new modality or dismiss? (NOTE: Included as resp4)
# Modality "Gills (larvae), air-breathing (adult)" transformed to resp2
# Modality "Plastron and gills" transformed to resp3
fin_AUS <- fin_AUS %>%
  mutate(resp1_gbr.new = ifelse(grepl("cutaneous", fin_AUS$Respiration_bugs_gbr, ignore.case = TRUE), 1, 0),
         resp2_gbr.new = ifelse(grepl("Gills", fin_AUS$Respiration_bugs_gbr), 1, 0),
         resp3_gbr.new = ifelse(grepl("plastron", fin_AUS$Respiration_bugs_gbr, ignore.case = TRUE), 1, 0),
         resp4_gbr.new = ifelse(grepl("surface|breathing$", fin_AUS$Respiration_bugs_gbr, ignore.case = TRUE), 1, 0))


# --- Maxwell
# Maxwell contains "pneumostome" (31 times) which is the respiratory system for (land)snails. Keep as new modality or dismiss?
# Maxwell contains "air-breathing" (140 times). Keep as new modality or dismiss? (NOTE: Included as resp4)
# Maxwell contains "air (plants)" (49 times). Keep as new modality or dismiss?
# resp7 ("Spiracle") added to resp2 ("Gills")
fin_AUS <- fin_AUS %>%
  mutate(resp1_maxwell.new = resp3_Maxwell,
         resp2_maxwell.new = resp4_Maxwell,
         resp3_maxwell.new = resp5_Maxwell,
         resp4_maxwell.new = resp1_Maxwell,
         resp5_maxwell.new = resp7_Maxwell)

fin_AUS$resp2_maxwell.new <- ifelse(grepl("0|NA", fin_AUS$resp2_maxwell.new), fin_AUS$resp7_Maxwell, fin_AUS$resp2_maxwell.new)

# --- VicEPA
fin_AUS <- fin_AUS %>%
  mutate(resp1_vicepa.new = Respiration_tegument_VicEPA,
         resp2_vicepa.new = Respiration_gills_VicEPA,
         resp3_vicepa.new = Respiration_plastron_VicEPA,
         resp5_vicepa.new = Respiration_spiracle_VicEPA)

# --- Chessman
fin_AUS <- fin_AUS %>%
  mutate(resp2_chessman.new = Gills_.aquatic_stages._fam_Chessman2017,
         resp4_chessman.new = Air_respiration_.aquatic_stages._fam_Chessman2017,
         resp5_chessman.new = Functional_spiracles_.aquatic_stages._fam_Chessman2017)


(resp_names <- grep("new|Resp._botwe", names(fin_AUS), value = TRUE, ignore.case = TRUE))

respiration <- fin_AUS %>% select(resp_names)

respiration <- respiration %>%
  na_if(0) %>%
  mutate(resp_tegument = Resp1_botwe, resp_gills = Resp2_botwe, resp_plastron = Resp3_botwe,
         resp_atmospheric = resp4_maxwell.new, resp_spiracle = resp5_maxwell.new) %>%
  mutate(resp_tegument = ifelse(is.na(resp_tegument), resp1_maxwell.new, resp_tegument),
         resp_gills = ifelse(is.na(resp_gills), resp2_maxwell.new, resp_gills),
         resp_plastron = ifelse(is.na(resp_plastron), resp3_maxwell.new, resp_plastron)) %>%
  mutate(resp_tegument = ifelse(is.na(resp_tegument), resp1_vicepa.new, resp_tegument),
         resp_gills = ifelse(is.na(resp_gills), resp2_vicepa.new, resp_gills),
         resp_plastron = ifelse(is.na(resp_plastron), resp3_vicepa.new, resp_plastron),
         resp_spiracle = ifelse(is.na(resp_spiracle), resp5_vicepa.new, resp_spiracle)) %>%
  mutate(resp_gills = ifelse(is.na(resp_gills), resp2_chessman.new, resp_gills),
         resp_atmospheric = ifelse(is.na(resp_atmospheric), resp4_chessman.new, resp_atmospheric),
         resp_spiracle = ifelse(is.na(resp_spiracle), resp5_chessman.new, resp_spiracle)) %>%
  mutate(resp_tegument = ifelse(is.na(resp_tegument), resp1_shafer.new, resp_tegument),
         resp_gills = ifelse(is.na(resp_gills), resp2_shafer.new, resp_gills),
         resp_plastron = ifelse(is.na(resp_plastron), resp3_shafer.new, resp_plastron),
         resp_atmospheric = ifelse(is.na(resp_atmospheric), resp4_shafer.new, resp_atmospheric)) %>%
  mutate(resp_tegument = ifelse(is.na(resp_tegument), resp1_gbr.new, resp_tegument),
         resp_gills = ifelse(is.na(resp_gills), resp2_gbr.new, resp_gills),
         resp_plastron = ifelse(is.na(resp_plastron), resp3_gbr.new, resp_plastron),
         resp_atmospheric = ifelse(is.na(resp_atmospheric), resp4_gbr.new, resp_atmospheric)) %>%
  select(resp_tegument:resp_spiracle) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# ---- Dispersal ----
# Only (aquatic) drift data was summarised
# 1. drift_low: low
# 2. drift_medium: medium
# 3. drift_high: high
# NOTE: Maybe a combined mobility trait would make sense

# --- VicEPA
# Fuzzy code was transformed into corresponding modalities
fin_AUS <- fin_AUS %>%
  mutate(drift1_vicepa.new = ifelse(grepl("1", Dispersal_aquatic_VicEPA), 1, 0),
         drift2_vicepa.new = ifelse(grepl("2", Dispersal_aquatic_VicEPA), 1, 0),
         drift3_vicepa.new = ifelse(grepl("3", Dispersal_aquatic_VicEPA), 1, 0))

# --- Shafer
# Only "low", "some strong drifting taxa" and "strong drifting" were took into account
# Therefore no modality for medium drift
fin_AUS <- fin_AUS %>%
  mutate(drift1_shafer.new = ifelse(grepl("low$", Dispersal_capacity_Shafer), 1, 0),
         drift3_shafer.new = ifelse(grepl("strong", Dispersal_capacity_Shafer), 1, 0))

# --- GBR
# Modalities were took into account only when 'drift' appeared
# Presumption: highest order of dispersal is the one found in the corresponding taxa (e.g. "low-mod" dispersal = moderate/medium)
# drift1 = ?
# drift2 = "mod in drift, not far as adults"
# drift3 = "some strong drifting taxa", "some strong drifting taxa (Baetis sp.; Nouisa sp.); flight more important?", "strong drifting"
fin_AUS <- fin_AUS %>%
  mutate(drift2_gbr.new = ifelse(grepl("mod in drift", Dispersal_capacity_bugs_gbr), 1, 0),
         drift3_gbr.new = ifelse(grepl("strong drifting", Dispersal_capacity_bugs_gbr), 1, 0))

# --- Maxwell
# disp1_Maxwell was converted to drift1_maxwell.new
# disp2_Maxwell was converted to drift3_maxwell.new
fin_AUS <- fin_AUS %>%
  mutate(drift1_maxwell.new = disp_low_Maxwell,
         drift3_maxwell.new = disp_drift_Maxwell)

# --- Botwe
fin_AUS <- fin_AUS %>%
  mutate(drift1_botwe.new = Drft1_botwe,
         drift2_botwe.new = Drft2_botwe,
         drift3_botwe.new = Drft3_botwe)

(drift_names <- grepl("drift", names(fin_AUS), ignore.case = TRUE) & grepl("new", names(fin_AUS), ignore.case = TRUE))

drift <- fin_AUS[drift_names]

drift <- drift %>%
  na_if(0) %>%
  mutate(drift_low = drift1_botwe.new, drift_medium = drift2_botwe.new, drift_high = drift3_botwe.new) %>%
  mutate(drift_low = ifelse(is.na(drift_low), drift1_vicepa.new, drift_low),
         drift_medium = ifelse(is.na(drift_medium), drift2_vicepa.new, drift_medium),
         drift_high = ifelse(is.na(drift_high), drift3_vicepa.new, drift_high)) %>%
  mutate(drift_low = ifelse(is.na(drift_low), drift1_shafer.new, drift_low),
         drift_high = ifelse(is.na(drift_high), drift3_shafer.new, drift_high)) %>%
  mutate(drift_medium = ifelse(is.na(drift_medium), drift2_gbr.new, drift_medium),
         drift_high = ifelse(is.na(drift_high), drift3_gbr.new, drift_high)) %>%
  mutate(drift_low = ifelse(is.na(drift_low), drift1_maxwell.new, drift_low),
         drift_high = ifelse(is.na(drift_high), drift3_maxwell.new, drift_high)) %>%
  select(drift_low:drift_high) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

  
# ---- Substrate Relation ----
# Definition: Substrate relation OR attachment OR (micro)habitat
# Only Botwe and VicEPA with data
# Sprawler = Crawler (?) -> "Attach_crawl_VicEPA" becomes substrate3
# Explanation:
# 1. sub_burrow: Burrow
# 2. sub_climb: Climb
# 3. sub_sprawl: Sprawl
# 4. sub_cling: Cling
# 5. sub_swim: Swim
# 6. sub_skate: Skate
# 7. sub_attached_temp: Attached (temporary)
# 8. sub_attached_perm: Attached (permanent)

sub_names <- grepl("habi", names(fin_AUS), ignore.case = TRUE) | grepl("attach", names(fin_AUS), ignore.case = TRUE)

substrate <- fin_AUS[sub_names]

substrate <- substrate %>%
  na_if(0) %>%
  mutate(sub_burrow = Habi1_botwe,
         sub_climb = Habi2_botwe,
         sub_sprawl = Habi3_botwe,
         sub_cling = Habi4_botwe,
         sub_swim = Habi5_botwe,
         sub_skate = Habi6_botwe,
         sub_attached_temp = Attach_temp_VicEPA,
         sub_attached_perm = Attach_perm_VicEPA) %>%
  mutate(sub_swim = ifelse(is.na(sub_swim), Attach_swim_VicEPA, sub_swim),
         sub_burrow = ifelse(is.na(sub_burrow), Attach_burrow_VicEPA, sub_burrow),
         sub_sprawl = ifelse(is.na(sub_sprawl), Attach_crawl_VicEPA, sub_sprawl)) %>%
  select(sub_burrow:sub_attached_perm) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# ---- Salinity Preference ----
# Only Botwe and VicEPA with data
# Modality "EC4_Maxwell" is missing in database
# Also: Not salinity preference rather than salinity sensitivity

# sal_names <- grepl("^sal", names(fin_AUS), ignore.case = TRUE) | grepl("^EC", names(fin_AUS), ignore.case = TRUE)
# 
# salinity <- fin_AUS[sal_names]
# 
# salinity <- salinity %>%
#   mutate(salinity1 = Sal1_botwe,
#          salinity2 = Sal2_botwe,
#          salinity3 = Sal3_botwe,
#          salinity4 = Sal4_botwe) %>%
#   mutate(salinity1 = ifelse(is.na(salinity1), EC1_Maxwell, salinity1),
#          salinity2 = ifelse(is.na(salinity2), EC2_Maxwell, salinity2),
#          salinity3 = ifelse(is.na(salinity3), EC3_Maxwell, salinity3)) %>%
#   select(salinity1:salinity4)


# ---- pH Preference ----
# Only Chessman with data
# Explanation (same levels as for Europe):
# 1. ph1: Acidic (ph < 7)
# 2. ph_neut_alk: Neutral to alkaline (ph >= 7)

ph_names <- grepl("^ph", names(fin_AUS), ignore.case = TRUE)
(ph <- fin_AUS[ph_names])

# Transform pH values to numeric 
ph <- transform(ph, ph = as.numeric(pH_minimum_fam_Chessman2017)) %>%
  select(ph)

ph <- ph %>%
  na_if(0) %>%
  mutate(ph_acidic = ifelse(ph < 7, 1, 0),
         ph_neut_alk = ifelse(ph >= 7, 1, 0)) %>%
  select(ph_acidic, ph_neut_alk) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# ---- Temperature Preference ----
# Only Chessman and Botwe with data
# Chessman with numeric data can be transformed to ranges. Botwe data is mostly eurytherm.
# Ther1_botwe is excluded because of stenothermal AND eurythermal character

# Explanation (same levels as for Europe):  
# temp_very_cold: very cold (< 6 °C)
# temp_cold: cold (>=6 °C & < 10 °C)
# temp_mod: moderate (>=10 °C & < 18 °C)
# temp_warm: warm (> 18 °C)
# temp_eurytherm: eurytherm (no specifice preference)


temp_names <- grepl("^ther", names(fin_AUS), ignore.case = TRUE)
(temperature <- fin_AUS[temp_names])

# Transform "Thermophily_Chessman2017" values to numeric 
temperature <- transform(temperature, temperature = as.numeric(Thermophily_Chessman2017)) %>%
  select(Ther1_botwe:Ther3_botwe, temperature)

temperature <- temperature %>%
  na_if(0) %>%
  mutate(temp_very_cold = ifelse(temperature < 6, 1, 0),
         temp_cold = ifelse(temperature >= 6 & temperature < 10, 1, 0),
         temp_mod = ifelse(temperature >= 10 & temperature < 18, 1, 0),
         temp_warm = ifelse(temperature > 18, 1, 0),
         temp_eurytherm = NA) %>%
  mutate(temp_eurytherm = ifelse(is.na(temp_eurytherm), Ther2_botwe, temp_eurytherm), 
         temp_eurytherm = ifelse(is.na(temp_eurytherm), Ther3_botwe, temp_eurytherm)) %>%
  select(temp_very_cold:temp_eurytherm) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# ---- Life Duration ----
# Only Botwe and VicEPA with data
# Explanation:
# 1. life_1: < 1 month
# 3. life_2: > 1 month

(life <- fin_AUS[grepl("life", names(fin_AUS), ignore.case = TRUE)])

life <- life %>%
  na_if(0) %>%
  mutate(life_1 = Life1_botwe, life_2 = Life3_botwe) %>%
  mutate(life_1 = ifelse(is.na(life_1), Life2_botwe, life_1),
         life_1 = ifelse(is.na(life_1), Total_lifespan_less_than_1m_VicEPA, life_1)) %>%
  mutate(life_2 = ifelse(is.na(life_2), Total_lifespan_1_to_3m_VicEPA, life_1),
         life_2 = ifelse(is.na(life_2), Total_lifespan_3_to_12m_VicEPA, life_1),
         life_2 = ifelse(is.na(life_2), Total_lifespan_more_than_1y_VicEPA, life_1)) %>%
  select(life_1:life_2) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# ---- Size ----
# Size in categories seems best option but problems with categories of VicEPA and Botwe
# Shafer and GBR with specific size in mm
# Explanation:
# size_small: small (< 9 mm)
# size_medium: medium (9 - 16 mm)
# size_large: large (> 16 mm)

# NOTE: The categories for VicEPA where transformed differently. Up to 10 mm -> small, 10 - 20 mm -> medium, > 20 mm -> large

# Transform Shafer data to categories
fin_AUS <- fin_AUS %>%
  mutate(size1_shafer = ifelse(Max_body_size_.mm._._number_Shafer > 0 & Max_body_size_.mm._._number_Shafer < 9, 1, 0),
         size2_shafer = ifelse(Max_body_size_.mm._._number_Shafer >= 9 & Max_body_size_.mm._._number_Shafer <= 16, 1, 0),
         size3_shafer = ifelse(Max_body_size_.mm._._number_Shafer < 16, 1, 0))

# Transform Shafer data to categories
fin_AUS <- fin_AUS %>%
  mutate(size1_gbr = ifelse(Max_body_size_.mm._._number_bugs_gbr > 0 & Max_body_size_.mm._._number_bugs_gbr < 9, 1, 0),
         size2_gbr = ifelse(Max_body_size_.mm._._number_bugs_gbr >= 9 & Max_body_size_.mm._._number_bugs_gbr <= 16, 1, 0),
         size3_gbr = ifelse(Max_body_size_.mm._._number_bugs_gbr < 16, 1, 0))

(size <- fin_AUS[grepl("size", names(fin_AUS), ignore.case = TRUE)])
size <- select(size, -(Max_body_size_.mm._._number_Shafer:Max_body_size_.mm._._number_bugs_gbr))

size <- size %>%
  na_if(0) %>%
  mutate(size_small = Size1_botwe, size_medium = Size2_botwe, size_large = Size3_botwe) %>%
  mutate(size_small = ifelse(is.na(size_small), Max_size_less_than_5_VicEPA, size_small),
         size_small = ifelse(is.na(size_small), Max_size_less_than_5_VicEPA, size_small),
         size_small = ifelse(is.na(size_small), size1_shafer, size_small),
         size_small = ifelse(is.na(size_small), size1_gbr, size_small)) %>%
  mutate(size_medium = ifelse(is.na(size_medium), Max_size_10_to_20_VicEPA, size_medium),
         size_medium = ifelse(is.na(size_medium), size2_shafer, size_medium),
         size_medium = ifelse(is.na(size_medium), size2_gbr, size_medium)) %>%
  mutate(size_large = ifelse(is.na(size_large), Max_size_20_to_40_VicEPA, size_large),
         size_large = ifelse(is.na(size_large), Max_size_more_than_40_VicEPA, size_large),
         size_large = ifelse(is.na(size_large), size3_shafer, size_large),
         size_large = ifelse(is.na(size_large), size3_gbr, size_large)) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))
  

# ---- Aquatic Stages ----
# Only VicEPA with data on aquatic stages of taxa

aquatic <- fin_AUS[grepl("Aquatic", names(fin_AUS))] 
aquatic <- aquatic %>%
  na_if(0) %>%
  rename(aquatic_egg = Aquatic_eggs_VicEPA, aquatic_nymph = Aquatic_nymph_VicEPA,
         aquatic_larva = Aquatic_larva_VicEPA, aquatic_pupa = Aquatic_pupa_VicEPA, aquatic_adult = Aquatic_imago_adult_VicEPA) %>%
  mutate_all(funs(replace(.,. == 3, 1))) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))


# NOTE: Information about emergence flight, resistance form, saprobity and dissemination strategy
# are not included in any database!


# ------------------------------------------------------------------------------------------------------------------------- #
#### Combine all traits with names_AUS ####

# --- Combine trait information and add join ID
trait_AUS <- cbind(voltinism, reproduction, feeding, respiration, drift, substrate, salinity, ph, temperature, life)
trait_AUS$id_join <- 1:nrow(trait_AUS)

# --- Taxon information from df_AUS
names_AUS <- df_AUS %>% select(Order:Species, id_join)

# --- Merge names_AUS with trait_AUS via id_join
df_AUS_compl <- merge(x = names_AUS, y = trait_AUS, by = "id_join", all.x = TRUE)
df_AUS_compl <- select(df_AUS_compl, -id_join)

# --- Remove rows with all NAs in trait columns
df_AUS_compl <- df_AUS_compl[rowSums(is.na(df_AUS_compl[5:ncol(df_AUS_compl)])) < ncol(df_AUS_compl[5:ncol(df_AUS_compl)]), ] 

# --- Save the database as .csv
write.table(df_AUS_compl, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS.csv", sep = ",")

