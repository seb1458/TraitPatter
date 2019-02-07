#####################
### Clean Up Data ###
#####################


# --------------------------------------------------------------------------------------------------------------- #
#### Working Directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####

# Data for MCA
ALL <- read.table(file.path(path, "final", "macroinvertebrate_ALL_mod.csv"), sep = ",", row.names = 1, header = TRUE)
names(ALL)

# Data for NMDS
NMDS <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin.csv"), sep = ",", row.names = 1, header = TRUE)

# --------------------------------------------------------------------------------------------------------------- #
#### Clean Up Data ####

# --- Change trait modality levels

# Region
lev <- with(ALL, levels(region))
lev[lev == "EUR"] <- "Europe"; lev[lev == "NAM"] <- "North America"; lev[lev == "AUS"] <- "Australia"
ALL <- within(ALL, levels(region) <- lev)

# pH Preference
lev <- with(ALL, levels(ph))
lev[lev == "ph_acidic"] <- "Acidic"; lev[lev == "ph_normal"] <- "Neutral/Alkaline"
ALL <- within(ALL, levels(ph) <- lev)

# Temperature Preference
lev <- with(ALL, levels(temperature))
lev[lev == "temp_very_cold"] <- "Very Cold"; lev[lev == "temp_cold"] <- "Cold"
lev[lev == "temp_moderate"] <- "Moderate"; lev[lev == "temp_warm"] <- "Warm"
lev[lev == "temp_ind"] <- "Indifferent"
ALL <- within(ALL, levels(temperature) <- lev)

# Feeding Mode
lev <- with(ALL, levels(feed_mode))
lev[lev == "feed_filter"] <- "Filterer"; lev[lev == "feed_gatherer"] <- "Gatherer"
lev[lev == "feed_parasite"] <- "Parasite"; lev[lev == "feed_predator"] <- "Predator"
lev[lev == "feed_scraper"] <- "Scraper"; lev[lev == "feed_shredder"] <- "Shredder"
ALL <- within(ALL, levels(feed_mode) <- lev)

# # Locomotion
lev <- with(ALL, levels(locomotion))
lev[lev == "loc_burrow"] <- "Burrower"; lev[lev == "loc_sessil"] <- "Sessil"
lev[lev == "loc_skate"] <- "Skater"; lev[lev == "loc_sprawl"] <- "Sprawler"
lev[lev == "loc_swim"] <- "Swimmer"
ALL <- within(ALL, levels(locomotion) <- lev)

# Respiration
lev <- with(ALL, levels(respiration))
lev[lev == "resp_atmospheric"] <- "Atmospheric"; lev[lev == "resp_gills"] <- "Gills"
lev[lev == "resp_plastron"] <- "Plastron"; lev[lev == "resp_Spiracle"] <- "Spiracle"
lev[lev == "resp_tegument"] <- "Tegument"
ALL <- within(ALL, levels(respiration) <- lev)
 
# Drift
lev <- with(ALL, levels(drift))
lev[lev == "drift_high"] <- "High"; lev[lev == "drif_low"] <- "Low"
ALL <- within(ALL, levels(drift) <- lev)

# Life Duration
lev <- with(ALL, levels(life))
lev[lev == "life1"] <- "< 1 Month"; lev[lev == "life2"] <- "1+ Month";
ALL <- within(ALL, levels(life) <- lev)

# Size
lev <- with(ALL, levels(size))
lev[lev == "size_large"] <- "Large"; lev[lev == "size_medium"] <- "Medium"; lev[lev == "size_small"] <- "Small"
ALL <- within(ALL, levels(size) <- lev)

# Reproduction
lev <- with(ALL, levels(reproduction))
lev[lev == "rep_aqu"] <- "Aquatic eggs"; lev[lev == "rep_ter"] <- "Terrestric eggs"
lev[lev == "rep_ovo"] <- "Ovoviparity"
ALL <- within(ALL, levels(reproduction) <- lev)

# Aquatic Stages
lev <- with(ALL, levels(aquatic_stages))
lev[lev == "stage1"] <- "Only eggs"; lev[lev == "stage2"] <- "Up to larva"; 
lev[lev == "stage3"] <- "Up to pupa"; lev[lev == "stage4"] <- "Up to adult"
ALL <- within(ALL, levels(aquatic_stages) <- lev)

# Voltinism
lev <- with(ALL, levels(voltinism))
lev[lev == "volt1"] <- "Semivoltine"; lev[lev == "volt2"] <- "Univoltine"
lev[lev == "volt3"] <- "Multivoltine"
ALL <- within(ALL, levels(voltinism) <- lev)


# ------------------------------------------------------ #
# --- Rename columns
new.names <- c("Order", "Family", "Region", "pH Preference", "Temperature Preference", "Feeding Mode", "Locomotion",
              "Respiration", "Drift", "Life Duration", "Size", "Reproduction", "Aquatic Stages", "Voltinism") 

names(ALL) <- new.names


# --------------------------------------------------------------------------------------------------------------- #
#### Save Fancy Data ####
head(NMDS)

NMDS <- rename(NMDS, Region = region)

lev <- with(NMDS, levels(Region))
lev[lev == "EUR"] <- "Europe"; lev[lev == "NAM"] <- "North America"; lev[lev == "AUS"] <- "Australia"
NMDS <- within(NMDS, levels(Region) <- lev)


# --------------------------------------------------------------------------------------------------------------- #
#### Save Fancy Data ####

# Write as .csv
write.table(ALL, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_mod_fancy.csv", sep = ",")

write.table(NMDS, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_bin_fancy.csv", sep = ",")
