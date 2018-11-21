#### Preparation: North American database ####
#---- Trait Information Preprocessing ----

# ------------------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# ------------------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(readxl)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_NOA <- read.csv(file.path(path, "North America", "macroinvertebrate_NOA_tax.csv"), stringsAsFactors = FALSE)
names(df_NOA)

# ------------------------------------------------------------------------------------------------------------------------- #
#### Query traits to keep ####

# ---- Size ----
# Trait 'Measured_length' is converted to the size classes of the European database.
# Classes in EUR database:
# size1: ≤ .25 cm 
# size2: > .25-.5 cm
# size3: > .5-1 cm
# size4: > 1-2 cm
# size5: > 2-4 cm
# size6: > 4-8 cm
# size7: > 8 cm

# Blank cells in Measured_length are replaced with zeroes
# Commas are replaced by points 
# df_NOA$Measured_length is transformed to numeric
df_NOA$Measured_length[df_NOA$Measured_length == ""] <- 0
df_NOA$Measured_length <- sub(",",".", df_NOA$Measured_length)
df_NOA[, "Measured_length"] <- as.numeric(df_NOA[, "Measured_length"])

size <- mutate(df_NOA,
                 size_1 = ifelse(df_NOA$Measured_length > 0 & df_NOA$Measured_length <= 2.5, 1, 0),
                 size_2 = ifelse(df_NOA$Measured_length > 2.5 & df_NOA$Measured_length <= 5.0, 1, 0),
                 size_3 = ifelse(df_NOA$Measured_length > 5.0 & df_NOA$Measured_length <= 10.0, 1, 0),
                 size_4 = ifelse(df_NOA$Measured_length > 10.0 & df_NOA$Measured_length <= 20.0, 1, 0),
                 size_5 = ifelse(df_NOA$Measured_length > 20.0 & df_NOA$Measured_length <= 40.0, 1, 0),
                 size_6 = ifelse(df_NOA$Measured_length > 40.0 & df_NOA$Measured_length <= 80.0, 1, 0),
                 size_7 = ifelse(df_NOA$Measured_length > 80, 1, 0))


# ---- Voltinism ----
levels(as.factor(df_NOA$Voltinism))

voltinism <- mutate(df_NOA,
                 volt_1 = ifelse(df_NOA$Voltinism == "< 1 Generation per year", 1, 0),
                 volt_2 = ifelse(df_NOA$Voltinism == "1 Generation per year", 1, 0),
                 volt_3 = ifelse(df_NOA$Voltinism == "> 1 Generation per year", 1, 0))


# ---- Aquatic Stages ----
# ???


# ---- Feed Mode ----
# Feed mode "Absorber" is deleted from EUR
# Feed mode "Collector gatherer" (NOA) is converted to "Deposit-feeder" (EUR) (Resh and Cardé 2009)

levels(as.factor(df_NOA$Feed_mode_prim))

feed <- mutate(df_NOA,
                 feed_abs = ifelse(df_NOA$Feed_mode_prim == "Absorber", 1, 0),
                 feed_deposit = ifelse(df_NOA$Feed_mode_prim == "Collector-gatherer", 1, 0),
                 feed_shredder = ifelse(df_NOA$Feed_mode_prim == "Shredder", 1, 0),
                 feed_scraper = ifelse(df_NOA$Feed_mode_prim == "Scraper/grazer", 1, 0),
                 feed_filter = ifelse(df_NOA$Feed_mode_prim == "Collector-filterer", 1, 0),
                 feed_piercer = ifelse(df_NOA$Feed_mode_prim == "Piercer herbivore", 1, 0),
                 feed_predator = ifelse(df_NOA$Feed_mode_prim == "Predator", 1, 0),
                 feed_parasite = ifelse(df_NOA$Feed_mode_prim == "Parasite", 1, 0))


# ---- Respiration ----
levels(as.factor(df_NOA$Resp_late))

# European levels
# ???

# Atmospheric breathers
unique(df_NOA[df_NOA$Resp_late == "Atmospheric breathers", "Family"])
# EUR
# "acroloxidae": tegument (snail)
# "chaoboridae": tegument (Resh and Cardé: kidney shaped air sacs, function as hyrostatic organ)
# "culicidae": spiracle (Resh and Cardé: ... water-surface inhabitants, and maintain cintact with the atmosphere with their spiracles (p. 291))
# "dixidae": spiracle
# "dolichpodidae": spiracle
# "dytiscidae": spiracle
# "ephydridae": spiracle (Resh and Cardé: Figure 16-27: Larva with spiracles p. 286)
# "hydrophilidae": mainly spiracle, 2nd plastron, one tegument/gill/spiracle (Resh and Cardé: Figure 4: Larva on water surface, p. 82; Adults have plastron p. 903)
# "lymnaeidae": tegument (pond snail)
# "nepidae": spiracle (Resh and Cardé: ... spiracles are locted at the end of a long tube ... p. 892)
# "physidae": tegument (bladder snail)
# "planorbidae": tegument (ramshorn snail)
# "psychodidae": spiracle (Resh and Cardé: All species of psychodidae possess spiracles p. 286)
# "ptychopteridae": spiracle
# "scirtidae": gill
# "stratiomyidae": spiracle
# "syrphidae": spiracle (Resh and Cardé: Figure 16-27: Larvae possess spiracles, p. 286)
# "tabanidae": spiracle
# "tipulidae": spiracle

# Cutaneous
taxa <- unique(df_NOA[df_NOA$Resp_late == "Cutaneous", "Family"])

keep <- which(df_EUR$family %in% taxa)
df_cut <- df_EUR[keep, c(2, resp_col)]
levels(as.factor(df_cut$family))
# EUR
# "apataniidae": tegument
# "bithyniidae": gill
# "blephariceridae": gill
# "brachycentridae": tegument
# "ceratopogonidae": gill
# "chaoboridae": tegument
# "chironomidae": tegument
# "dipseudopsidae": tegument
# "dytiscidae": spiracle
# "enchytraeidae": tegument
# "erpobdellidae": tegument
# "glossiphoniidae": tegument
# "glossosomatidae": tegument
# "goeridae": gill
# "haplotaxidae": tegument
# "helicopsychidae": tegument
# "hirudinidae": tegument
# "hydridae": tegument
# "hydrobiidae": gill
# "hydroptilidae": tegument
# "lepidostomatidae": tegument / gill
# "leptoceridae": tegument / gill
# "limnephilidae": tegument / gill
# "lumbricidae": tegument
# "lumbriculidae": tegument
# "molannidae": tegument / gill
# "naididae": tegument
# "naucoridae": plastron / spiracle
# "neritidae": spiracle 
# "odontoceridae": gill
# "philopotamidae": tegument
# "phryganeidae": tegument / gill 
# "piscicolidae": tegument
# "polycentropodidae": tegument 
# "psychomyiidae": tegument
# "rhyacophilidae": tegument / gill
# "sericostomatidae": tegument / gill
# "sialidae": gill
# "simuliidae": gill
# "sisyridae": gill
# "tubificidae": tegument
# "uenoidae": tegument
# "valvatidae": gill
# "viviparidae": gill

# Hemoglobin
unique(df_NOA[df_NOA$Resp_late == "Hemoglobin", "Family"])
# EUR
# "chironomidae": tegument, gill
# "notonectidae": mainly spiracle
# "planorbidae": tegument (ramshorn snail)
# "tubificidae": tegument (worm)

# Plant breathers #
unique(df_NOA[df_NOA$Resp_late == "Plant breathers", "Family"])
# EUR
# "chyrsomelidae": spiracle
# "culicidae": spiracle
# "curculionidae": x
# "ephydridae": spiracle (Resh and Cardé: Figure 16-27: Larva with spiracles p. 286)
# "noteridae": tegument/spiracle

# Temporary air store
unique(df_NOA[df_NOA$Resp_late == "Temporary air store", "Family"])
# EUR
# "belostomatidae": x
# "corixidae": spiracle
# "naucoridae": spiracle, spiracle/plastron
# "nepidae": spiracle (Resh and Cardé: ... spiracles are located at the end of a long tube ... p. 892)
# "notonectidae": spiracle
# "pleidae": spiracle

resp <- mutate(df_NOA,
                 resp_spiracle = ifelse(Family %in% c("corixidae" , "naucoridae" , "nepidae" , "notonectidae" , "pleidae" ,
                                                      "chrysomelidae" , "culicidae" , "ephydridae" , "noteridae", "dixidae" ,
                                                      "dolichpodidae" , "dytiscidae" , "hydrophilidae" , "psychodidae" , "ptychopteridae" , 
                                                      "stratiomyidae" , "syrphidae" , "tabanidae" , "tipulidae"), 1, 0),
                 
                 resp_gill = ifelse(Family %in% c("chironomidae" , "scirtidae" , "bithyniidae" , "blephariceridae" , "ceratopogonidae" ,
                                                  "goeridae" , "hydrobiidae" , "lepidostomatidae" , "leptoceridae" , "limnephilidae" , "molannidae" ,
                                                  "neritidae" , "odontoceridae" , "phryganeidae" , "rhyacophilidae" , "sericostomatidae" , 
                                                  "sialidae" , "simuliidae" , "sisyridae" , "valvatidae" , "viviparidae"), 1, 0),
                 
                 resp_tegument = ifelse(Family %in% c("noteridae" , "chironomidae" , "planorbidae" , "tubificidae" , "acroloxidae" ,
                                                      "chaoboridae" , "lymnaeidae" , "physidae" , "uenoidae" , "sericostomatidae" , 
                                                      "rhyacophilidae" , "psychomyiidae" , "polycentropodidae" , "piscicolidae" , 
                                                      "phryganeidae" , "philopotamidae" , "naididae" , "molannidae" , "lumbriculidae" ,
                                                      "lumbricidae" , "limnephilidae" , "leptoceridae" , "lepidostomatidae" , "hydridae" ,
                                                      "hirudinidae" , "helicopsychidae" , "apataniidae" , "brachycentridae" , "dipseudopsidae" ,
                                                      "enchytraeidae" , "erpobdellidae" , "glossiphoniidae" , "glossosomatidae" , "haplotaxidae"), 1, 0),
                 
                 resp_plastron = ifelse(Family %in% c("naucoridae"), 1, 0)
)
df_NOA$resp_hydro_vesicle <- 0

df_NOA <- df_NOA %>%
  mutate(resp_spiracle = ifelse(Resp_late == "Spiracular gills" , 1, 0),
         resp_gill = ifelse(Resp_late == "Tracheal gills", 1, 0))


# ---- Drift ----
# Drift_early was chosen because it inherits most of the aquatic taxa (juveniles in water)
# Levels as in Australian database (drift1 = weak, drift2 = medium, drift3 = strong)
levels(as.factor(df_NOA$Drift_early))

drift <- df_NOA %>%
  mutate(drift1 = ifelse(grepl("weak", Drift_early, ignore.case = TRUE), 1, 0),
         drift2 = ifelse(grepl("medium", Drift_early, ignore.case = TRUE), 1, 0),
         drift3 = ifelse(grepl("sstrong", Drift_early, ignore.case = TRUE), 1, 0))


# ---- Locomotion ----
table(as.factor(df_NAM$Habit_prim))

# USA == EUR
# "Skater" == "loc_surf_swimmer"
# "Swimmer" + "Planktonic" == "loc_full_swimmer"
# "Burrower" == "loc_burrower"
# "Attached/fixed" == "loc_perm_attached"
# "Climber" == "loc_crawler"
# "Clinger" == "loc_temp_attached"
# "Sprawler" == "loc_interstitial"
# ??? == "loc_flier"

# 1. substrate1: Burrow
# 2. substrate2: Climb
# 3. substrate3: Sprawl
# 4. substrate4: Cling
# 5. substrate5: Swim
# 6. substrate6: Skate
# 7. substrate7: Attached (temporary)
# 8. substrate8: Attached (permanent)


# ---- Oxygen/Saprobity ----
saprobity <- df_NOA %>%
  select(O2_normal, O2_low)


# --- pH Preference ----
ph <- df_NOA %>%
  mutate(ph1 = pH_acidic,
         ph2 = pH_normal) %>%
  mutate(ph2 = ifelse(is.na(ph2), pH_alkaline, ph2))


# ---- Temperature preference ----
levels(as.factor(df_NOA$Thermal_pref))


# ---- Salinity Preference
# Assigning levels via df_NOA$Salin_fresh, df_NOA$Salin_brakish, df_NOA$Salin_salt?
