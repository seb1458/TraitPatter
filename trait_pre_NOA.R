##############################################
#### Preparation: North American database ####
##############################################
# ---- Trait Information Preprocessing ----

# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_NOA <- read.csv(file.path(path, "North America", "macroinvertebrate_NOA_tax.csv"), stringsAsFactors = FALSE)
df_NOA <- df_NOA %>% na_if(0) %>% na_if("")

# --------------------------------------------------------------------------------------------------------------- #
#### Query traits to keep ####

# ---- Size ----
# Trait 'Measured_length' is converted to the following size classes:
# size1: small (< 9 mm) 
# size2: medium (9-16 mm)
# size3: large (> 16 mm)

# Blank cells in Measured_length are replaced with zeroes
# Commas are replaced by points 
# df_NOA$Measured_length is transformed to numeric
df_NOA$Measured_length <- sub(",",".", df_NOA$Measured_length)
df_NOA[, "Measured_length"] <- as.numeric(df_NOA[, "Measured_length"])

size <- df_NOA %>%
  mutate(size1 = ifelse(Measured_length > 0 & Measured_length < 9, 1, NA),
         size2 = ifelse(Measured_length >= 9 & Measured_length <= 16, 1, NA),
         size3 = ifelse(Measured_length > 16, 1, NA)) %>%
  select(size1:size3)


# ---- Voltinism ----
levels(as.factor(df_NOA$Voltinism))

voltinism <- df_NOA %>%
  mutate(volt_1 = ifelse(grepl("< 1 Generation per year", Voltinism, ignore.case = TRUE), 1, NA),
         volt_2 = ifelse(grepl("1 Generation per year", Voltinism, ignore.case = TRUE), 1, NA),
         volt_3 = ifelse(grepl("> 1 Generation per year", Voltinism, ignore.case = TRUE), 1, NA)) %>%
  select(volt_1:volt_3)


# ---- Aquatic Stages ----
levels(as.factor(df_NOA$No.Aquatic_stages))

stages <- df_NOA %>%
  mutate(stage_ln = ifelse(grepl("1", No.Aquatic_stages), 1, NA),
         stage_ln = ifelse(grepl("2", No.Aquatic_stages), 1, stage_ln),
         stage_ln = ifelse(grepl("3", No.Aquatic_stages), 1, stage_ln),
         stage_ln = ifelse(grepl("4", No.Aquatic_stages), 1, stage_ln)) %>%
  mutate(stage_e = ifelse(grepl("2", No.Aquatic_stages), 1, NA),
         stage_e = ifelse(grepl("3", No.Aquatic_stages), 1, stage_ln),
         stage_e = ifelse(grepl("4", No.Aquatic_stages), 1, stage_ln)) %>%
  mutate(stage_p = ifelse(grepl("3", No.Aquatic_stages), 1, NA),
         stage_p = ifelse(grepl("4", No.Aquatic_stages), 1, stage_p)) %>%
  mutate(stage_a = ifelse(grepl("4", No.Aquatic_stages), 1, NA)) %>%
  select(stage_ln:stage_a)

# ---- Feed Mode ----
# Feed mode "Absorber" is deleted from EUR

levels(as.factor(df_NOA$Feed_mode_prim))

feed <- df_NOA %>%
  mutate(feed_abs = ifelse(df_NOA$Feed_mode_prim == "Absorber", 1, NA),
         feed_gath = ifelse(df_NOA$Feed_mode_prim == "Collector-gatherer", 1, NA),
         feed_shred = ifelse(df_NOA$Feed_mode_prim == "Shredder", 1, NA),
         feed_scrap = ifelse(df_NOA$Feed_mode_prim == "Scraper/grazer", 1, NA),
         feed_filt = ifelse(df_NOA$Feed_mode_prim == "Collector-filterer", 1, NA),
         feed_piercer = ifelse(df_NOA$Feed_mode_prim == "Piercer herbivore", 1, NA),
         feed_pred = ifelse(df_NOA$Feed_mode_prim == "Predator", 1, NA),
         feed_para = ifelse(df_NOA$Feed_mode_prim == "Parasite", 1, NA)) %>%
  select(feed_abs:feed_para)


# ---- Respiration ----
levels(as.factor(df_NOA$Resp_late))

# 
# 
# # European levels
# # ???
# 
# # Atmospheric breathers
# unique(df_NOA[df_NOA$Resp_late == "Atmospheric breathers", "Family"])
# # EUR
# # "acroloxidae": tegument (snail)
# # "chaoboridae": tegument (Resh and Cardé: kidney shaped air sacs, function as hyrostatic organ)
# # "culicidae": spiracle (Resh and Cardé: ... water-surface inhabitants, and maintain cintact with the atmosphere with their spiracles (p. 291))
# # "dixidae": spiracle
# # "dolichpodidae": spiracle
# # "dytiscidae": spiracle
# # "ephydridae": spiracle (Resh and Cardé: Figure 16-27: Larva with spiracles p. 286)
# # "hydrophilidae": mainly spiracle, 2nd plastron, one tegument/gill/spiracle (Resh and Cardé: Figure 4: Larva on water surface, p. 82; Adults have plastron p. 903)
# # "lymnaeidae": tegument (pond snail)
# # "nepidae": spiracle (Resh and Cardé: ... spiracles are locted at the end of a long tube ... p. 892)
# # "physidae": tegument (bladder snail)
# # "planorbidae": tegument (ramshorn snail)
# # "psychodidae": spiracle (Resh and Cardé: All species of psychodidae possess spiracles p. 286)
# # "ptychopteridae": spiracle
# # "scirtidae": gill
# # "stratiomyidae": spiracle
# # "syrphidae": spiracle (Resh and Cardé: Figure 16-27: Larvae possess spiracles, p. 286)
# # "tabanidae": spiracle
# # "tipulidae": spiracle
# 
# # Cutaneous
# taxa <- unique(df_NOA[df_NOA$Resp_late == "Cutaneous", "Family"])
# 
# keep <- which(df_EUR$family %in% taxa)
# df_cut <- df_EUR[keep, c(2, resp_col)]
# levels(as.factor(df_cut$family))
# # EUR
# # "apataniidae": tegument
# # "bithyniidae": gill
# # "blephariceridae": gill
# # "brachycentridae": tegument
# # "ceratopogonidae": gill
# # "chaoboridae": tegument
# # "chironomidae": tegument
# # "dipseudopsidae": tegument
# # "dytiscidae": spiracle
# # "enchytraeidae": tegument
# # "erpobdellidae": tegument
# # "glossiphoniidae": tegument
# # "glossosomatidae": tegument
# # "goeridae": gill
# # "haplotaxidae": tegument
# # "helicopsychidae": tegument
# # "hirudinidae": tegument
# # "hydridae": tegument
# # "hydrobiidae": gill
# # "hydroptilidae": tegument
# # "lepidostomatidae": tegument / gill
# # "leptoceridae": tegument / gill
# # "limnephilidae": tegument / gill
# # "lumbricidae": tegument
# # "lumbriculidae": tegument
# # "molannidae": tegument / gill
# # "naididae": tegument
# # "naucoridae": plastron / spiracle
# # "neritidae": spiracle 
# # "odontoceridae": gill
# # "philopotamidae": tegument
# # "phryganeidae": tegument / gill 
# # "piscicolidae": tegument
# # "polycentropodidae": tegument 
# # "psychomyiidae": tegument
# # "rhyacophilidae": tegument / gill
# # "sericostomatidae": tegument / gill
# # "sialidae": gill
# # "simuliidae": gill
# # "sisyridae": gill
# # "tubificidae": tegument
# # "uenoidae": tegument
# # "valvatidae": gill
# # "viviparidae": gill
# 
# # Hemoglobin
# unique(df_NOA[df_NOA$Resp_late == "Hemoglobin", "Family"])
# # EUR
# # "chironomidae": tegument, gill
# # "notonectidae": mainly spiracle
# # "planorbidae": tegument (ramshorn snail)
# # "tubificidae": tegument (worm)
# 
# # Plant breathers #
# unique(df_NOA[df_NOA$Resp_late == "Plant breathers", "Family"])
# # EUR
# # "chyrsomelidae": spiracle
# # "culicidae": spiracle
# # "curculionidae": x
# # "ephydridae": spiracle (Resh and Cardé: Figure 16-27: Larva with spiracles p. 286)
# # "noteridae": tegument/spiracle
# 
# # Temporary air store
# unique(df_NOA[df_NOA$Resp_late == "Temporary air store", "Family"])
# # EUR
# # "belostomatidae": x
# # "corixidae": spiracle
# # "naucoridae": spiracle, spiracle/plastron
# # "nepidae": spiracle (Resh and Cardé: ... spiracles are located at the end of a long tube ... p. 892)
# # "notonectidae": spiracle
# # "pleidae": spiracle
# 
# resp <- mutate(df_NOA,
#                  resp_spiracle = ifelse(Family %in% c("corixidae" , "naucoridae" , "nepidae" , "notonectidae" , "pleidae" ,
#                                                       "chrysomelidae" , "culicidae" , "ephydridae" , "noteridae", "dixidae" ,
#                                                       "dolichpodidae" , "dytiscidae" , "hydrophilidae" , "psychodidae" , "ptychopteridae" , 
#                                                       "stratiomyidae" , "syrphidae" , "tabanidae" , "tipulidae"), 1, 0),
#                  
#                  resp_gill = ifelse(Family %in% c("chironomidae" , "scirtidae" , "bithyniidae" , "blephariceridae" , "ceratopogonidae" ,
#                                                   "goeridae" , "hydrobiidae" , "lepidostomatidae" , "leptoceridae" , "limnephilidae" , "molannidae" ,
#                                                   "neritidae" , "odontoceridae" , "phryganeidae" , "rhyacophilidae" , "sericostomatidae" , 
#                                                   "sialidae" , "simuliidae" , "sisyridae" , "valvatidae" , "viviparidae"), 1, 0),
#                  
#                  resp_tegument = ifelse(Family %in% c("noteridae", "chironomidae" , "planorbidae" , "tubificidae" , "acroloxidae" ,
#                                                       "chaoboridae" , "lymnaeidae" , "physidae" , "uenoidae" , "sericostomatidae" , 
#                                                       "rhyacophilidae" , "psychomyiidae" , "polycentropodidae" , "piscicolidae" , 
#                                                       "phryganeidae" , "philopotamidae" , "naididae" , "molannidae" , "lumbriculidae" ,
#                                                       "lumbricidae" , "limnephilidae" , "leptoceridae" , "lepidostomatidae" , "hydridae" ,
#                                                       "hirudinidae" , "helicopsychidae" , "apataniidae" , "brachycentridae" , "dipseudopsidae" ,
#                                                       "enchytraeidae" , "erpobdellidae" , "glossiphoniidae" , "glossosomatidae" , "haplotaxidae"), 1, 0),
#                  
#                  resp_plastron = ifelse(Family %in% c("naucoridae"), 1, 0))
# 
# df_NOA$resp_hydro_vesicle <- 0
# 
# respiration <- df_NOA %>%
#   mutate(resp_spiracle = ifelse(Resp_late == "Spiracular gills" , 1, 0),
#          resp_gill = ifelse(Resp_late == "Tracheal gills", 1, 0)) %>%
#   select(resp_spiracle:resp_plastron)

resp <- df_NOA %>%
  mutate(resp_atm = ifelse(grepl("atmospheric", Resp_late, ignore.case = TRUE), 1, NA),
         resp_teg = ifelse(grepl("cutaneous", Resp_late, ignore.case = TRUE), 1, NA),
         resp_pls = ifelse(grepl("plastron", Resp_late, ignore.case = TRUE), 1, NA),
         resp_spi = ifelse(grepl("spiracular", Resp_late, ignore.case = TRUE), 1, NA),
         resp_gil = ifelse(grepl("tracheal", Resp_late, ignore.case = TRUE), 1, NA)) %>%
  select(resp_atm:resp_gil)


# ---- Drift ----
# Drift_early was chosen because it inherits most of the aquatic taxa (juveniles in water)
# Levels as in Australian database (drift1 = weak, drift2 = medium, drift3 = strong)
levels(as.factor(df_NOA$Drift_early))

drift <- df_NOA %>%
  mutate(drift1 = ifelse(grepl("weak", Drift_early, ignore.case = TRUE), 1, NA),
         drift2 = ifelse(grepl("medium", Drift_early, ignore.case = TRUE), 1, NA),
         drift3 = ifelse(grepl("strong", Drift_early, ignore.case = TRUE), 1, NA)) %>%
  select(drift1:drift3)


# ---- Locomotion ----
table(as.factor(df_NOA$Habit_prim))

# Evenutally dismiss planktonic
# Check comment column?

locomotion <- df_NOA %>%
  mutate(locom_sessil = ifelse(grepl("attached", Habit_prim, ignore.case = TRUE), 1, NA),
         locom_burrow = ifelse(grepl("burrower", Habit_prim, ignore.case = TRUE), 1, NA),
         locom_climber = ifelse(grepl("climber", Habit_prim, ignore.case = TRUE), 1, NA),
         locom_clinger = ifelse(grepl("clinger", Habit_prim, ignore.case = TRUE), 1, NA),
         locom_planktonic = ifelse(grepl("planktonic", Habit_prim, ignore.case = TRUE), 1, NA),
         locom_skater = ifelse(grepl("skater", Habit_prim, ignore.case = TRUE), 1, NA),
         locom_sprawler = ifelse(grepl("sprawler", Habit_prim, ignore.case = TRUE), 1, NA),
         locom_swimmer = ifelse(grepl("swimmer", Habit_prim, ignore.case = TRUE), 1, NA)) %>%
  select(locom_sessil:locom_swimmer)


# ---- Life Duration ----
levels(as.factor(df_NOA$Adult_lifespan))

life <- df_NOA %>%
  mutate(life_hours = ifelse(grepl("Hours", Adult_lifespan), 1, NA),
         life_days = ifelse(grepl("Days", Adult_lifespan), 1, NA),
         life_weeks = ifelse(grepl("Weeks", Adult_lifespan), 1, NA),
         life_months = ifelse(grepl("Months", Adult_lifespan), 1, NA)) %>%
  select(life_hours:life_months)

# ---- Oxygen/Saprobity ----
saprobity <- select(df_NOA, O2_normal, O2_low)

# --- pH Preference ----
ph <- df_NOA %>%
  mutate(ph_acid = pH_acidic,
         ph_norm = coalesce(pH_normal, pH_alkaline)) %>%
  select(ph_acid:ph_norm)

# ---- Reproduction ----
levels(as.factor(df_NOA$Ovipos))

# ---- Temperature preference ----
levels(as.factor(df_NOA$Thermal_pref))


# --------------------------------------------------------------------------------------------------------------- #
#### Final table ####
NOA_tax <- df_NOA %>% select(Taxa:Taxon)
NOA_fin <- cbind(NOA_tax, size, voltinism, stages, feed, resp, drift, saprobity, ph, locomotion, life)
# MISSING: Respiration, temperature preference, salinity preference, locomotion/substrate relation, aquatic stages, reproduction

# Write .csv
write.table(NOA_fin, file = "~/Schreibtisch/Thesis/data/North America/macroinvertebrate_NOA_trait.csv", sep = ",")
