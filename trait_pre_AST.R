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
df_AUS <- na_if(df_AUS, 0)


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
  mutate(volt2_shafer = ifelse(grepl("1$", df_AUS$Number_of_generations_per_year_Shafer), 1, NA),
         volt3_shafer = ifelse(!grepl("1$", df_AUS$Number_of_generations_per_year_Shafer) & !is.na(df_AUS$Number_of_generations_per_year_Shafer), 1, NA))
# No species with voltinism < 1 
# "1$|NA" anything else besides 0.5-1, 1 and NA

fin_AUS <- fin_AUS %>%
  mutate(volt1_gbr = ifelse(grepl("5$", df_AUS$Number_of_generations_per_year_bugs_gbr), 1, NA),
         volt2_gbr = ifelse(grepl("0.5-1|<=|^1$", df_AUS$Number_of_generations_per_year_bugs_gbr), 1, NA),
         volt3_gbr = ifelse(!grepl("5$|0.5-1|<=|^1$", df_AUS$Number_of_generations_per_year_bugs_gbr) & !is.na(df_AUS$Number_of_generations_per_year_bugs_gbr), 1, NA))


# Starting with the data from Botwe, and adding more information from the other databases
# Chessman has no voltinism data 
# Maxwell has no data for voltinism = 1
# Shafer has no data for voltinism < 1

(volt_names <- grep("volt", names(fin_AUS), value = TRUE, ignore.case = TRUE))

voltinism <- select(fin_AUS, volt_names)

voltinism <- voltinism %>%
  select(-Ref_Voltinism_VicEPA) %>%
  mutate_all(as.integer) %>%
  mutate(volt1 = coalesce(Volt1_botwe, volt1_Maxwell, Voltinism_less_than_1_VicEPA, volt1_gbr),
         volt2 = coalesce(Volt2_botwe, volt2_Maxwell, Voltinism_1_VicEPA, volt2_shafer, volt2_gbr), 
         volt3 = coalesce(Volt3_botwe, volt3_Maxwell, volt4_Maxwell, Voltinism_2_VicEPA, Voltinism_more_than_2_VicEPA, volt3_shafer, volt3_gbr)) %>%
  select(volt1:volt3)


# ---- Reproduction ----
# Explanation
# 1. rep1: aquatic eggs
# 2. rep2: terrestrial eggs
# 3. rep3: ovoviparity

# Transforming Shafer, GBR and Maxwell data to described modalities 
# NOTE: "Eggs inside plants/objects in or near water" could also be terrestrial
# In data from shafer only "some taxa terrestrial eggs" was transformed to rep2
fin_AUS <- fin_AUS %>%
  mutate(rep1_shafer = ifelse(grepl("aquatic|plants|substrate|free", fin_AUS$Reproduction_type_Shafer, ignore.case = TRUE), 1, NA),
         rep2_shafer = ifelse(grepl("terrestrial|above", fin_AUS$Reproduction_type_Shafer, ignore.case = TRUE), 1, NA),
         rep3_shafer = ifelse(grepl("ovo", fin_AUS$Reproduction_type_Shafer, ignore.case = TRUE), 1, NA))

# In data from GBR everything with "terrestrial" was transformed to rep2
fin_AUS <- fin_AUS %>%
  mutate(rep1_gbr = ifelse(grepl("aquatic|shallow|stones|algae|adults|plants|substrate|free", fin_AUS$Reproduction_type_bugs_gbr, ignore.case = TRUE), 1, NA),
         rep2_gbr = ifelse(grepl("terrestrial", fin_AUS$Reproduction_type_bugs_gbr, ignore.case = TRUE), 1, NA),
         rep3_gbr = ifelse(grepl("ovo", fin_AUS$Reproduction_type_bugs_gbr, ignore.case = TRUE), 1, NA))

# In data from Maxwell rep4 (eggs attached to male adults) was missing.

(rep_names <- grep("rep", names(fin_AUS), value = TRUE, ignore.case = TRUE))

reproduction <- select(fin_AUS, rep_names)
reproduction <- select(reproduction, -c(1:4))

reproduction <- reproduction %>%
  mutate_all(as.integer) %>%
  mutate(rep_aqu = coalesce(Rep1_botwe, repro1_Maxwell, rep1_shafer, rep1_gbr),
         rep_ter = coalesce(Rep2_botwe, repro2_Maxwell, rep2_shafer, rep1_gbr), 
         rep_ovo = coalesce(Rep3_botwe, repro3_Maxwell, rep3_shafer, rep3_gbr)) %>%
  select(rep_aqu:rep_ovo)


# ---- Feed Mode ----
# Explanation
# 1. feed_gath: Collector-gatherer
# 2. feed_filt: Collector-filterer
# 3. feed_herb: Herbivore (scraper, shredder, piercer)
# 4. feed_pred: Predator (piercer, engulfer)
# 5. feed_shred: Shredder (detritivore)
# 6. feed_para: Parasite

# Transforming Shafer, GBR and Maxwell data to described modalities 

# --- Shafer
# Only "Detritivores" was assigned to modality feed5.
fin_AUS <- fin_AUS %>%
  mutate(feed3_shafer = ifelse(grepl("herbivor", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, NA),
         feed4_shafer = ifelse(grepl("predator", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, NA),
         feed5_shafer = ifelse(grepl("detritivores$", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, NA),
         feed6_shafer = ifelse(grepl("parasite", fin_AUS$Feeding_group_Shafer, ignore.case = TRUE), 1, NA))

# --- GBR
# Transformation of "Scavenger" unclear
# Transformation of "larvea predators, adults herbivores" unclear
# Snails -> Predators
fin_AUS <- fin_AUS %>%
  mutate(feed3_gbr = ifelse(grepl("algae|herbivor|plants", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, NA),
         feed4_gbr = ifelse(grepl("predator|snails", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, NA),
         feed5_gbr = ifelse(grepl("detritivores$", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, NA),
         feed6_gbr = ifelse(grepl("parasite", fin_AUS$Feeding_group_bugs_gbr, ignore.case = TRUE), 1, NA))

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

fin_AUS$feed1_maxwell <- coalesce(fin_AUS$C.SH_Maxwell, fin_AUS$C.SC_Maxwell, fin_AUS$feed1_maxwell)

(feeding <- fin_AUS[grepl("feed|trop|marchant", names(fin_AUS), ignore.case = TRUE) &
    !grepl("ref|group", names(fin_AUS), ignore.case = TRUE)])

feeding <- feeding %>%
  mutate_all(as.integer) %>%
  mutate(feed_gath = coalesce(Trop1_botwe, feed1_maxwell, Gatherer_.proportion_of_feeding._fam_Chessman2017,
                              Gatherer..proportion.of.feeding._genus_Chessman2017, detritivore_Marchant),
         
         feed_filt = coalesce(Trop2_botwe, feed2_maxwell, Filterer_.proportion_of_feeding._fam_Chessman2017,
                              Filterer..proportion.of.feeding._genus_Chessman2017, filterer_Marchant,
                              Feeding_filterers_VicEPA),
         
         feed_scrap = coalesce(Trop3_botwe, feed3_maxwell, Scraper_.proportion_of_feeding._fam_Chessman2017,
                              Scraper..proportion.of.feeding._genus_Chessman2017, feed3_shafer, feed3_gbr,
                              grazer_Marchant, Feeding_scrapers_VicEPA, Feeding_deposit_grazer_VicEPA,
                              feeding$Feeding_piercers_VicEPA),
         
         feed_pred = coalesce(Trop4_botwe, feed4_maxwell, Predator_.proportion_of_feeding._fam_Chessman2017,
                              Predator..proportion.of.feeding._genus_Chessman2017, feed4_shafer, feed4_gbr,
                              predator_Marchant, Feeding_predators_VicEPA),
         
         feed_shred = coalesce(Trop5_botwe, feed5_maxwell, Shredder_.proportion_of_feeding._fam_Chessman2017,
                               Shredder..proportion.of.feeding._genus_Chessman2017, feed5_shafer, feed5_gbr,
                               shredder_Marchant, Feeding_shredders_VicEPA),
         
         feed_para = coalesce(feed6_maxwell, feed6_shafer, feed6_gbr, Feeding_parasite_VicEPA)) %>%
  select(feed_gath:feed_para)
  
# Change Chessman values (0.25, 0.3333, 0.5) to 1
replace <- c(0.2, 0.25, 0.333333333333333, 0.5)
for (i in seq_along(feeding)) {
  feeding[[i]][feeding[[i]] %in% replace] <- 1
}

# feeding %>% mutate_all(as.factor) %>% sapply(levels)

# Note: All shredder modalities were assigned to feed3 (hebivor)
# Missing: "Feeding_absorber_VicEPA", 


# ---- Respiration ----
# Explanation
# 1. resp_teg: Tegument
# 2. resp_gil: Gills
# 3. resp_plas: Plastron
# 4. resp_atmos: Air/Atmospheric
# 5. resp_spir: Spiracle

# --- Shafer
# Shafer contains "pneumostome" (2 times) which is the respiratory system for (land)snails. Keep as new modality or dismiss?
# Shafer contains "air-breathing" (2 times). Keep as new modality or dismiss? (NOTE: Included as resp4)
# Modality "Gills (larvae), air-breathing (adult)" transformed to resp2
# Modality "Plastron and gills" transformed to resp3
fin_AUS <- fin_AUS %>%
  mutate(resp1_shafer.new = ifelse(grepl("cutaneous", fin_AUS$Respiration_Shafer, ignore.case = TRUE), 1, NA),
         resp2_shafer.new = ifelse(grepl("Gills", fin_AUS$Respiration_Shafer), 1, NA),
         resp3_shafer.new = ifelse(grepl("plastron", fin_AUS$Respiration_Shafer, ignore.case = TRUE), 1, NA),
         resp4_shafer.new = ifelse(grepl("breathing", fin_AUS$Respiration_Shafer, ignore.case = TRUE), 1, NA))


# --- GBR
# GBR contains "pneumostome" (6 times) which is the respiratory system for (land)snails. Keep as new modality or dismiss?
# GBR contains "air-breathing" (~15NA times). Keep as new modality or dismiss? (NOTE: Included as resp4)
# Modality "Gills (larvae), air-breathing (adult)" transformed to resp2
# Modality "Plastron and gills" transformed to resp3
fin_AUS <- fin_AUS %>%
  mutate(resp1_gbr.new = ifelse(grepl("cutaneous", fin_AUS$Respiration_bugs_gbr, ignore.case = TRUE), 1, NA),
         resp2_gbr.new = ifelse(grepl("Gills", fin_AUS$Respiration_bugs_gbr), 1, NA),
         resp3_gbr.new = ifelse(grepl("plastron", fin_AUS$Respiration_bugs_gbr, ignore.case = TRUE), 1, NA),
         resp4_gbr.new = ifelse(grepl("surface|breathing$", fin_AUS$Respiration_bugs_gbr, ignore.case = TRUE), 1, NA))


# --- Maxwell
# Maxwell contains "pneumostome" (31 times) which is the respiratory system for (land)snails. Keep as new modality or dismiss?
# Maxwell contains "air-breathing" (14NA times). Keep as new modality or dismiss? (NOTE: Included as resp4)
# Maxwell contains "air (plants)" (49 times). Keep as new modality or dismiss?
# resp7 ("Spiracle") added to resp2 ("Gills")
fin_AUS <- fin_AUS %>%
  mutate(resp1_maxwell.new = resp3_Maxwell,
         resp2_maxwell.new = resp4_Maxwell,
         resp3_maxwell.new = resp5_Maxwell,
         resp4_maxwell.new = resp1_Maxwell,
         resp5_maxwell.new = resp7_Maxwell)

fin_AUS$resp2_maxwell.new <- coalesce(fin_AUS$resp7_Maxwell, fin_AUS$resp2_maxwell.new)

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
  mutate_all(as.integer) %>%
  mutate(resp_teg = coalesce(Resp1_botwe, resp1_maxwell.new, resp1_vicepa.new, resp1_shafer.new, resp1_gbr.new),
         resp_gil = coalesce(Resp2_botwe, resp2_maxwell.new, resp2_vicepa.new, resp2_chessman.new, resp2_shafer.new, resp2_gbr.new),
         resp_plas = coalesce(Resp3_botwe, resp3_maxwell.new, resp3_vicepa.new, resp3_shafer.new, resp3_gbr.new),
         resp_atmos = coalesce(resp4_maxwell.new, resp4_chessman.new, resp4_shafer.new, resp4_gbr.new),
         resp_spir = coalesce(resp5_maxwell.new, resp5_vicepa.new, resp5_chessman.new)) %>%
  select(resp_teg:resp_spir)


# ---- Dispersal ----
# Only (aquatic) drift data was summarised
# 1. drift_low: low
# 2. drift_medium: medium
# 3. drift_high: high
# NOTE: Maybe a combined mobility trait would make sense

# --- VicEPA
# Fuzzy code was transformed into corresponding modalities
fin_AUS <- fin_AUS %>%
  mutate(drift1_vicepa.new = ifelse(grepl("1", Dispersal_aquatic_VicEPA), 1, NA),
         drift2_vicepa.new = ifelse(grepl("2", Dispersal_aquatic_VicEPA), 1, NA),
         drift3_vicepa.new = ifelse(grepl("3", Dispersal_aquatic_VicEPA), 1, NA))

# --- Shafer
# Only "low", "some strong drifting taxa" and "strong drifting" were took into account
# Therefore no modality for medium drift
fin_AUS <- fin_AUS %>%
  mutate(drift1_shafer.new = ifelse(grepl("low$", Dispersal_capacity_Shafer), 1, NA),
         drift3_shafer.new = ifelse(grepl("strong", Dispersal_capacity_Shafer), 1, NA))

# --- GBR
# Modalities were took into account only when 'drift' appeared
# Presumption: highest order of dispersal is the one found in the corresponding taxa (e.g. "low-mod" dispersal = moderate/medium)
# drift1 = ?
# drift2 = "mod in drift, not far as adults"
# drift3 = "some strong drifting taxa", "some strong drifting taxa (Baetis sp.; Nouisa sp.); flight more important?", "strong drifting"
fin_AUS <- fin_AUS %>%
  mutate(drift2_gbr.new = ifelse(grepl("mod in drift", Dispersal_capacity_bugs_gbr), 1, NA),
         drift3_gbr.new = ifelse(grepl("strong drifting", Dispersal_capacity_bugs_gbr), 1, NA))

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

drift <- fin_AUS[grepl("drift", names(fin_AUS), ignore.case = TRUE) & grepl("new", names(fin_AUS), ignore.case = TRUE)]
str(drift)

drift <- drift %>%
  mutate_all(as.integer) %>%
  mutate(drift_low = coalesce(drift1_botwe.new, drift1_vicepa.new, drift1_shafer.new, drift1_maxwell.new),
         drift_medium = coalesce(drift2_botwe.new, drift2_vicepa.new, drift2_gbr.new),
         drift_high = coalesce(drift3_botwe.new, drift3_vicepa.new, drift3_shafer.new, drift3_gbr.new, drift3_maxwell.new)) %>%
  select(drift_low:drift_high)


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

substrate <- fin_AUS[grepl("habi", names(fin_AUS), ignore.case = TRUE) | grepl("attach", names(fin_AUS), ignore.case = TRUE)]

substrate <- substrate %>%
  mutate_all(as.integer) %>%
  mutate(sub_burrow = coalesce(Habi1_botwe, Attach_burrow_VicEPA), 
         sub_climb = Habi2_botwe,
         sub_sprawl = coalesce(Habi3_botwe, Attach_crawl_VicEPA),
         sub_cling = Habi4_botwe,
         sub_swim = coalesce(Habi5_botwe, Attach_swim_VicEPA), 
         sub_skate = Habi6_botwe,
         sub_attached_temp = Attach_temp_VicEPA,
         sub_attached_perm = Attach_perm_VicEPA) %>%
  select(sub_burrow:sub_attached_perm)


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
  mutate(ph_acidic = ifelse(ph < 7, 1, 0),
         ph_neut_alk = ifelse(ph >= 7, 1, 0)) %>%
  select(ph_acidic, ph_neut_alk)


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

temperature <- fin_AUS[grepl("^ther", names(fin_AUS), ignore.case = TRUE)]

# Transform "Thermophily_Chessman2017" values to numeric 
temperature <- transform(temperature, temperature = as.numeric(Thermophily_Chessman2017)) %>%
  select(Ther1_botwe:Ther3_botwe, temperature)

temperature <- temperature %>%
  mutate_all(as.integer) %>%
  mutate(temp_very_cold = ifelse(temperature < 6, 1, NA),
         temp_cold = ifelse(temperature >= 6 & temperature < 10, 1, NA),
         temp_mod = ifelse(temperature >= 10 & temperature < 18, 1, NA),
         temp_warm = ifelse(temperature > 18, 1, NA),
         temp_eurytherm = coalesce(Ther2_botwe, Ther3_botwe)) %>%
  select(temp_very_cold:temp_eurytherm)


# ---- Life Duration ----
# Only Botwe and VicEPA with data
# Explanation:
# 1. life_1: < 1 month
# 3. life_2: > 1 month

life <- fin_AUS[grepl("life", names(fin_AUS), ignore.case = TRUE)]

life <- life %>%
  select(-Ref_Total_lifespan_VicEPA) %>%
  mutate_all(as.integer) %>%
  mutate(life_1 = coalesce(Life1_botwe, Life2_botwe, Total_lifespan_less_than_1m_VicEPA),
         life_2 = coalesce(Life3_botwe, Total_lifespan_1_to_3m_VicEPA, Total_lifespan_3_to_12m_VicEPA,
                           Total_lifespan_more_than_1y_VicEPA)) %>%
  select(life_1:life_2)


# ---- Size ----
# Size in categories seems best option but problems with categories of VicEPA and Botwe
# Shafer and GBR with specific size in mm
# Explanation:
# size_s: small (< 9 mm)
# size_m: medium (9 - 16 mm)
# size_l: large (> 16 mm)

# NOTE: The categories for VicEPA where transformed differently. Up to 10 mm -> small, 10 - 20 mm -> medium, > 20 mm -> large

# Transform Shafer data to categories
fin_AUS <- fin_AUS %>%
  mutate(size1_shafer = ifelse(Max_body_size_.mm._._number_Shafer > 0 & Max_body_size_.mm._._number_Shafer < 9, 1, NA),
         size2_shafer = ifelse(Max_body_size_.mm._._number_Shafer >= 9 & Max_body_size_.mm._._number_Shafer <= 16, 1, NA),
         size3_shafer = ifelse(Max_body_size_.mm._._number_Shafer < 16, 1, NA))

# Transform Shafer data to categories
fin_AUS <- fin_AUS %>%
  mutate(size1_gbr = ifelse(Max_body_size_.mm._._number_bugs_gbr > 0 & Max_body_size_.mm._._number_bugs_gbr < 9, 1, NA),
         size2_gbr = ifelse(Max_body_size_.mm._._number_bugs_gbr >= 9 & Max_body_size_.mm._._number_bugs_gbr <= 16, 1, NA),
         size3_gbr = ifelse(Max_body_size_.mm._._number_bugs_gbr < 16, 1, NA))

size <- fin_AUS[grepl("size", names(fin_AUS), ignore.case = TRUE)]
size <- select(size, -(Max_body_size_.mm._._number_Shafer:Max_body_size_.mm._._number_bugs_gbr))

size <- size %>%
  mutate_all(as.integer) %>%
  mutate(size_s = coalesce(Size1_botwe, Max_size_less_than_5_VicEPA, Max_size_less_than_5_VicEPA,
                           size1_shafer, size1_gbr),
         
         size_m = coalesce(Size2_botwe, Max_size_10_to_20_VicEPA, size2_shafer, size2_gbr),
         
         size_l = coalesce(Size3_botwe, Max_size_20_to_40_VicEPA, size3_shafer,
                           Max_size_more_than_40_VicEPA, size3_gbr)) %>%
  select(size_s:size_l)


# ---- Aquatic Stages ----
# Only VicEPA with data on aquatic stages of taxa

aquatic <- fin_AUS[grepl("Aquatic", names(fin_AUS))] 

aquatic <- aquatic %>%
  rename(aquatic_egg = Aquatic_eggs_VicEPA,
         aquatic_nymph = Aquatic_nymph_VicEPA,
         aquatic_larva = Aquatic_larva_VicEPA,
         aquatic_pupa = Aquatic_pupa_VicEPA,
         aquatic_adult = Aquatic_imago_adult_VicEPA) %>%
  mutate_all(funs(replace(.,. == 3, 1)))


# NOTE: Information about emergence flight, resistance form, saprobity and dissemination strategy
# are not included in any database!


# ------------------------------------------------------------------------------------------------------------------------- #
#### Combine all traits with names_AUS ####

# --- Combine trait information and add join ID
trait_AUS <- cbind(voltinism, reproduction, feeding, respiration, drift, substrate, ph, temperature, life, size, aquatic)
trait_AUS$id_join <- 1:nrow(trait_AUS)

# --- Taxon information from df_AUS
names_AUS <- df_AUS %>% select(Order:Species, id_join)

# --- Merge names_AUS with trait_AUS via id_join
df_AUS_compl <- merge(x = names_AUS, y = trait_AUS, by = "id_join", all.x = TRUE)
df_AUS_compl <- select(df_AUS_compl, -id_join)

# --- Remove rows with all NAs in trait columns
df_AUS_compl <- df_AUS_compl[rowSums(is.na(df_AUS_compl[5:ncol(df_AUS_compl)])) < ncol(df_AUS_compl[5:ncol(df_AUS_compl)]), ] 

# --- Save the database as .csv
write.table(df_AUS_compl, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_trait.csv", sep = ",")
