#########################################
#### Preparation: Australia database ####
#########################################

#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"



#### Packages ####
library(tidyverse)
library(data.table)
library(readxl)
library(reshape2)

library(taxize)
library(stringr)
library(beepr)



#### Load data ####
df_AUS <- read_excel(file.path(path, "Australia", "Australian macroinv trait database.xlsx"), sheet = 1)

# Add ID for later join
df_AUS$id_join <- 1:nrow(df_AUS)

# Change NULL, N/A, "NA" to NA
df_AUS[df_AUS == "NULL"] <- NA
df_AUS[df_AUS == "N/A"] <- NA
df_AUS[df_AUS == "NA"] <- NA



#### Query taxon information ####
# --- Order + Order_bugs_gbr + Order_fam_Chessman2017
df_AUS$Order <- coalesce(df_AUS$Order, df_AUS$Order_bugs_gbr, df_AUS$Order_fam_Chessman2017)

# --- Genus_and_species + SAName_botwe
df_AUS$Genus_and_species <- coalesce(df_AUS$Genus_and_species, df_AUS$SAName_botwe)

# --- Find and remove duplicated entries in names_AUS$long_code
df_AUS %>% 
  group_by(long_code) %>% 
  filter(n() > 1) %>%
  arrange(desc(long_code)) %>%
  select(long_code:Genus_and_species)

# Seperate entries with NAs in long_code from rest
AUS_na <- df_AUS[is.na(df_AUS$long_code), ]

# Select only entries without duplicate entries in long_code
df_AUS <- df_AUS[!duplicated(df_AUS$long_code) & !is.na(df_AUS$long_code), ]

# Put non-duplicates and NAs back together
df_AUS <- rbind(df_AUS, AUS_na)
rm(AUS_na)



#### Table Join with the ID list ####
# ID list from Ben Kefford with long_code data and taxon information
# NOTE: The file containing the long_codes has some format errors for some cells. Remove manually in spreadsheet.
id_sheet1 <- read_excel(file.path(path, "Australia", "VicEPA_Codes.xlsx"), sheet = 1)
id_sheet1 <- id_sheet1 %>%
  select(-PHYLUM, -CLASS, -X__2, -X__3) %>%
  rename(ID = X__1, Order = ORDER, Family = FAMILY, Species = SPECIES)

id_sheet2 <- read_excel(file.path(path, "Australia", "VicEPA_Codes.xlsx"), sheet = 2)
id_sheet2 <- id_sheet2 %>%
  select(-CLASS, -Comments) %>%
  rename(ID = Code, Order = ORDER, Family = FAMILY, Species = SPECIES)

id_sheet3 <- read_excel(file.path(path, "Australia", "VicEPA_Codes.xlsx"), sheet = 3)
id_sheet3 <- id_sheet3 %>%
  select(-CLASS, -Comments) %>%
  rename(ID = Code, Order = ORDER, Family = FAMILY, Species = SPECIES)


id_list <- rbind(id_sheet1, id_sheet2, id_sheet3) %>%
  rename(long_code = ID, Order.vic = Order, Family.vic = Family, Species.vic = Species)

rm(id_sheet1, id_sheet2, id_sheet3)

# Remove duplicated entries from id_list
id_list <- id_list[!duplicated(id_list$long_code), ]

# NOTE: Some codes are in names_AUS but not in id_list as well as some missing entries for names_AUS$long_code
setdiff(df_AUS$long_code, id_list$long_code)
sum(is.na(df_AUS$long_code))

# --- Use left table join to extend the df_AUS data
df_AUS <- merge(x = df_AUS, y = id_list, by = "long_code", all.x = TRUE)

# Combine information from both tables in new columns. Delete old columns
# 1. Order information
df_AUS$Order <- coalesce(df_AUS$Order, df_AUS$Order.vic)

# 2. Family information
df_AUS$Family <- coalesce(df_AUS$Family, df_AUS$Family.vic)

# 3. Species information
df_AUS$Genus_and_species <- coalesce(df_AUS$Genus_and_species, df_AUS$Species.vic)

df_AUS <- df_AUS %>% select(-Order.vic, -Family.vic, -Species.vic)



#### Final Preparation of Taxa Information ####
# --- Delete incomplete information
# When NA in Family and long_code: no identification possible
df_AUS <- df_AUS %>%
  filter(!(is.na(Family) & is.na(long_code)))

# --- Strange code data: MITEXXXX dismissed for now
# names_AUS <- names_AUS[!grepl("mite", names_AUS$long_code, ignore.case = TRUE), ]

# --- Remove rows with all NAs in the taxon and code columns
df_AUS <- df_AUS[rowSums(is.na(df_AUS[1:5])) < 5, ] 



#### Preprocessing Script with data.table ####
# Trying to resolve crude taxa names
# differentiate into species column and unresolved taxa
df_AUS <- as.data.table(df_AUS)
df_AUS[, `:=`(Species = lapply(Genus_and_species, function(y) {
  val <- grep("Genus|.*sp.*|group.*|unknown|.*unidentif.*|undifferentiated|.*SO[1-9]|.*L10.*|Ecnomina|Kingolus / Simsonia.*", 
              y, value = TRUE, invert = TRUE, ignore.case = TRUE)
  val <- grep("[A-z] [A-z]", val, value = TRUE)
  ifelse(identical(val, character(0)), NA, val)
}
), 
Unresolved_taxa = lapply(Genus_and_species, function(y) {
  val <- grep("Genus|.*sp.*|group.*|unknown|.*unidentif.*|undifferentiated|.*SO[1-9]|.*L10.*|Ecnomina|Kingolus / Simsonia.*", 
              y, value = TRUE, ignore.case = TRUE)
  ifelse(identical(val, character(0)), NA, val)
}
)
)]


# compare Unresolved_taxa column with Genus column with regard to sp. and spp. 
# AST_trait_DB[Unresolved_taxa %like% "sp.$|spp.$", (lapply(.SD, function(y) ifelse(is.na(y), Unresolved_taxa,
#                                                                                        as.character(y)))), 
#              .SDcols = "Genus"]


# Delete Genus_and_species column
df_AUS[, Genus_and_species := NULL]


# set order of columns
setcolorder(df_AUS, c("Order", "Family", "Genus", "Species", "long_code", "short_code", "Unresolved_taxa"))


# Some colnames contian "." or ".." -> needs to be changed
setnames(x = df_AUS, old = names(df_AUS), 
         new = gsub(pattern = "\\.", replacement = "_", x = names(df_AUS)))


# still some colnames have two "_" 
setnames(x = df_AUS, old = names(df_AUS), 
         new = gsub(pattern = "\\__", replacement = "_", x = names(df_AUS)))


# Some columns have list as type -> change to write table
grep("list", sapply(df_AUS, typeof), value = TRUE)
names(df_AUS)
df_AUS[, `:=`(Species = unlist(Species), Unresolved_taxa = unlist(Unresolved_taxa) )]
 


#### Complement Information ####
# --- Some entries with missing Order but with information in Family column. 
# 1. Get all order and family names where Order is not NA
order_compl <- df_AUS[!is.na(df_AUS$Order), 1:2] %>%
  unique()

# 2. Dismiss all rows with NA as family entry and all duplicates
order_compl <- order_compl[!is.na(order_compl$Family), ]
order_compl <- order_compl[!duplicated(order_compl$Family), ]

# 3. Table join to complete entries
df_AUS <- merge(x = df_AUS, y = order_compl, by = "Family", all.x = TRUE) 

# Order.x = old Order names, Order.y = complete Order names
df_AUS <- df_AUS %>%
  rename(Order = Order.y) %>%
  select(Order, Family, Genus, Species, long_code, Unresolved_taxa, everything())
rm(order_compl)


# --- Correct entries for Family column
levels(as.factor(df_AUS$Family))

df_AUS <- df_AUS %>%
  mutate(Family = ifelse(Family == "Anisoptera (dragonflies)", NA, Family),
         Family = ifelse(Family == "Arrenuridae (water mite)", "Arrenuridae", Family),
         Family = ifelse(Family == "Aturidae (water mites)", "Aturidae", Family),
         Family = ifelse(Family == "Chironomidae: Aphroteniinae" |
                           Family == "Chironomidae: Chironominae" |
                           Family == "Chironomidae: Orthocladiinae" |
                           Family == "Chironomidae: Podonominae" |
                           Family == "Chironomidae: Tanypodinae",
                         "Chironomidae", Family),
         Family = ifelse(Family == "Calocid/Helicophidae", "Helicophidae", Family), 
         Family = ifelse(Family == "Coenagrionidae (odonata)", "Coenagrionidae", Family),
         Family = ifelse(Family == "Conoesucidae (Tricoptera)", "Conoesucidae", Family),
         Family = ifelse(Family == "Gripopterygidae (Plecoptera)", "Gripopterygidae", Family))


# --- Correct entries which are not a family name
df_AUS[!grepl("idae", df_AUS$Family), 1:6]

levels(as.factor(df_AUS[!grepl("idae", df_AUS$Family), 2]))

# Acarina is a subclass name. Acariformes is the super order name
# Acariformes has following orders: Sarcoptiformes, Trombidiformes, Oribatida, Mesostigmata
df_AUS[grep("Acariformes", df_AUS$Order, ignore.case = TRUE), 1:6]

# 1. Halacaroidea: super family name
# Order: Trombidiformes

# 2. Hydracarina: super family name
# Order: Trombidiformes

# 3. Mesostigmata: order name
df_AUS[grep("Mesostigmata", df_AUS$Unresolved_taxa, ignore.case = TRUE), "Order"] <- "Mesostigmata"

# 4. Trombidioidea: super family name
# Order: Trombidiformes

# 5. Astigmata: cohort name
# Order: Sarcoptiformes

# 6. Oribatida: order name
df_AUS[grep("Oribatida", df_AUS$Unresolved_taxa, ignore.case = TRUE), "Order"] <- "Oribatida"

# Species belonging to Sarcoptiformes
df_AUS[grep("Astigmata", df_AUS$Unresolved_taxa, ignore.case = TRUE), "Order"] <- "Sarcoptiformes"

# Species belonging to Trombidiformes
df_AUS[grep("Halacoroidea|Hydracarina|Trombidioidea", df_AUS$Unresolved_taxa, ignore.case = TRUE), "Order"] <- "Trombidiformes"


# Amphipoda is the Order
df_AUS[grep("Amphipoda", df_AUS$Family, ignore.case = TRUE), 1:6]
df_AUS[grep("Amphipoda", df_AUS$Family, ignore.case = TRUE), "Family"] <- NA

df_AUS[grep("Amphipoda", df_AUS$Order, ignore.case = TRUE), 1:6]
# Entry for order already existing


# Brachyura is an Infraorder. Actual Order is Decapoda
df_AUS[grep("Brachyura", df_AUS$Family, ignore.case = TRUE), "Order"] <- "Decapoda"
df_AUS[grep("Brachyura", df_AUS$Family, ignore.case = TRUE), "Family"] <- NA


# Caridea is an Infraorder. Actual Order is Decapoda
# Entry for order already existing


# Coleoptera is an Order. 
# Entry for order already existing


# Conchostraca is a class (?)
# Delete?


# Decapoda is an Order
df_AUS[grep("Decapoda", df_AUS$Family, ignore.case = TRUE), "Order"] <- "Decapoda"
df_AUS[grep("Decapoda", df_AUS$Family, ignore.case = TRUE), "Family"] <- NA


# Diptera is an Order
# Entry for order already existing


# Gastropoda is a Class. Identification via species names if existing
df_AUS[grep("Gastropoda", df_AUS$Family, ignore.case = TRUE), 1:6]

# 1. Whelk is used for many different species. No clear identification

# 2. Conuber belongs to the genera Turbinidae (turban snails) (?). No order found
df_AUS[grep("Conuber", df_AUS$Unresolved_taxa, ignore.case = TRUE), "Genus"] <- "Conuber"

# 3. Gastropoda sp. not clearly identifiable


# Hemiptera is an Order
# Entry for Order already existing


# Isopoda is an Order. 
# Entry for Order already existing


# Lepidoptera is an Order
# Entry for Order already existing


# Nematoda is a Phylum. Not clearly identifiable.


# Nemertea is a Phylum. Not clearly identifiable.


# Odonata is an Order
# Entry for Order already existing


# Oligochaeta is a subclass. Not clearly identifiable.


# Oribatida is an Order.
df_AUS[grep("Oribatida", df_AUS$Family, ignore.case = TRUE), "Order"] <- "Oribatida"
df_AUS[grep("Oribatida", df_AUS$Family, ignore.case = TRUE), "Family"] <- NA


# Pelecypoda is a Class. Identification via species names if existing
# Corbiculoidea sp. is a Family
df_AUS[grep("Pelecypoda", df_AUS$Family, ignore.case = TRUE)[2], "Family"] <- "Corbiculidae"

# Mussel not clearly identifiable

# Bivalve Unid not clearly identifiable

# Polychaeta spp., Polychaeta SE species, Polychaeta Sp 2, Polychaeta Sp 3 not clearly identifiable


# Syncarida is a Super Order no identification possible


# Trichoptera is an Order.
# Entry for Order already existing

# Lepidoptera is an Order.
df_AUS[grep("Lepidoptera", df_AUS$Family, ignore.case = TRUE), 1:6]
df_AUS[grep("Lepidoptera", df_AUS$Family, ignore.case = TRUE), "Family"] <- NA

df_AUS[grep("Lepidoptera", df_AUS$Unresolved_taxa, ignore.case = TRUE), "Order"] <- "Lepidoptera"

# Some entries left, which are not identifiable
df_AUS <- df_AUS %>%
  select(Order, Family, Genus, Species, Unresolved_taxa, long_code, everything())

df_AUS <- df_AUS[rowSums(is.na(df_AUS[1:5])) < 5, ] 

df_AUS[!grepl("idae", df_AUS$Family), 1:6]



# --- Correct entries for Order column
levels(as.factor(df_AUS$Order))

# Super Order Syncarida contains two families: Koonungidae (Order: Anaspidacea) and Parabathynellidae (Order: Bathynellacea)
df_AUS[grep("Super", df_AUS$Order, ignore.case = TRUE), 1:6]

df_AUS[grep("Super", df_AUS$Order, ignore.case = TRUE)[1:2], "Order"] <- "Anaspidacea"
df_AUS[grep("Super", df_AUS$Order, ignore.case = TRUE)[1], "Order"] <- "Bathynellacea"
# Family Syncarida is non-existent



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

# Final columns to keep
fin_AUS <- df_AUS %>%
  select(id_join,
         keep_shafer,
         keep_gbr,
         keep_vicepa,
         keep_chessman,
         keep_bowte,
         keep_maxwell)



#### Format trait information ####

# ---- Voltinism ---- #
# Explanation
# 1. volt1: < 1 reproductive cycle per year
# 2. volt2: = 1 reproductive cycle per year
# 3. volt3: > 1 reproductive cycle per year

# Transforming Shafer and gbr data to described modalities 
# For both transforming processes the maximal amount of generations per year was anticipated.
# E.g. number of generations per year = 0.5-1 translates to 1 generation per year (modality: volt2)
fin_AUS <- fin_AUS %>%
  mutate(volt2_shafer = ifelse(grepl("1$", df_AUS$Number_of_generations_per_year_Shafer), 1, 0),
         volt3_shafer = ifelse(!grepl("1$|NA", df_AUS$Number_of_generations_per_year_Shafer), 1, 0))
  # No species with voltinism < 1 
  # "1$|NA" anything else besides 0.5-1, 1 and NA

fin_AUS <- fin_AUS %>%
  mutate(volt1_gbr = ifelse(grepl("5$", df_AUS$Number_of_generations_per_year_bugs_gbr), 1, 0),
         volt2_gbr = ifelse(grepl("0.5-1|<=|^1$", df_AUS$Number_of_generations_per_year_bugs_gbr), 1, 0),
         volt3_gbr = ifelse(!grepl("5$|0.5-1|<=|^1$|NA", df_AUS$Number_of_generations_per_year_Shafer),1 ,0))
  

# Starting with the data from Botwe, and adding more information from the other databases
# Chessman has no voltinism data 
# Maxwell has no data for voltinism = 1
# Shafer has no data for voltinism < 1

(volt_names <- grep("volt", names(fin_AUS), value = TRUE, ignore.case = TRUE))

voltinism <- fin_AUS %>% select(volt_names)

voltinism <- voltinism %>%
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
  select(volt1:volt3)


# ---- Reproduction ---- #
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
  
reproduction <- fin_AUS %>% select(rep_names)

reproduction <- reproduction %>%
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
  select(rep1:rep3)  


# ---- Feeding ---- #
# Explanation
# 1. feed1: Collector-gatherer
# 2. feed2: Collector-filterer
# 3. feed3: Herbivor (scraper, shredder, piercer)
# 4. feed4: Predator (piercer, engulfer)
# 5. feed5: Shredder (detritivore)
# 6. feed6: Parasite

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

fin_AUS$feed1_maxwell <- ifelse(grepl("0|NA", fin_AUS$feed1_maxwell), fin_AUS$`C,SH_Maxwell`, fin_AUS$feed1_maxwell)
fin_AUS$feed1_maxwell <- ifelse(grepl("0|NA", fin_AUS$feed1_maxwell), fin_AUS$`C,SC_Maxwell`, fin_AUS$feed1_maxwell)

(feed_names <- grep("feed|trop", names(fin_AUS), value = TRUE, ignore.case = TRUE))

feeding <- fin_AUS %>% select(feed_names)

feeding <- feeding %>% select(-c(grep("ref", names(feeding), value = TRUE, ignore.case = TRUE)))

feeding <- feeding %>%
  mutate(feed1 = Trop1_botwe, feed2 = Trop2_botwe, feed3 = Trop3_botwe, feed4 = Trop4_botwe,
         feed5 = Trop1_botwe, feed5 = Trop5_botwe, feed6 = feed6_maxwell) %>%
  mutate(feed1 = ifelse(is.na(feed1), feed1_maxwell, feed1),
         feed2 = ifelse(is.na(feed2), feed2_maxwell, feed2),
         feed3 = ifelse(is.na(feed3), feed3_maxwell, feed3),
         feed4 = ifelse(is.na(feed4), feed4_maxwell, feed4),
         feed5 = ifelse(is.na(feed5), feed5_maxwell, feed5)) %>%
  mutate(feed1 = ifelse(is.na(feed1), Feeding_filterers_VicEPA, feed1),
         feed2 = ifelse(is.na(feed2), Feeding_deposit_grazer_VicEPA, feed2),
         feed3 = ifelse(is.na(feed3), Feeding_scrapers_VicEPA, feed3),
         feed4 = ifelse(is.na(feed4), Feeding_predators_VicEPA, feed4),
         feed5 = ifelse(is.na(feed5), Feeding_shredders_VicEPA, feed5),
         feed6 = ifelse(is.na(feed6), Feeding_parasite_VicEPA, feed6)) %>%
  mutate(feed1 = ifelse(is.na(feed1), `Filterer_(proportion_of_feeding)_fam_Chessman2017`, feed1),
         feed2 = ifelse(is.na(feed2), `Gatherer_(proportion_of_feeding)_fam_Chessman2017`, feed2),
         feed3 = ifelse(is.na(feed3), `Scraper_(proportion_of_feeding)_fam_Chessman2017`, feed3),
         feed4 = ifelse(is.na(feed4), `Predator_(proportion_of_feeding)_fam_Chessman2017`, feed4),
         feed5 = ifelse(is.na(feed5), `Shredder_(proportion_of_feeding)_fam_Chessman2017`, feed5)) %>%
  mutate(feed1 = ifelse(is.na(feed1), `Filterer (proportion of feeding)_genus_Chessman2017`, feed1),
         feed2 = ifelse(is.na(feed2), `Gatherer (proportion of feeding)_genus_Chessman2017`, feed2),
         feed3 = ifelse(is.na(feed3), `Scraper (proportion of feeding)_genus_Chessman2017`, feed3),
         feed4 = ifelse(is.na(feed4), `Predator (proportion of feeding)_genus_Chessman2017`, feed4),
         feed5 = ifelse(is.na(feed5), `Shredder (proportion of feeding)_genus_Chessman2017`, feed5)) %>%
  mutate(feed1 = ifelse(is.na(feed3), feed3_shafer, feed3),
         feed2 = ifelse(is.na(feed4), feed4_shafer, feed4),
         feed3 = ifelse(is.na(feed5), feed5_shafer, feed5),
         feed4 = ifelse(is.na(feed6), feed6_shafer, feed6)) %>%
  mutate(feed1 = ifelse(is.na(feed3), feed3_gbr, feed3),
         feed2 = ifelse(is.na(feed4), feed4_gbr, feed4),
         feed3 = ifelse(is.na(feed5), feed5_gbr, feed5),
         feed4 = ifelse(is.na(feed6), feed6_gbr, feed6))  


# Last changes
# 1. Assign missing piercer-group of VicEPA to feed3
feeding$feed3 <- ifelse(grepl("0", feeding$feed3), feeding$Feeding_piercers_VicEPA, feeding$feed3)

# 2. Change Chessman values (0.25, 0.3333, 0.5) to 1
replace <- c("0.2", "0.25", "0.333333333333333", "0.5")
for (i in seq_along(feeding)) {
  feeding[[i]][feeding[[i]] %in% replace] <- 1
}
feeding %>% mutate_all(as.factor) %>% sapply(levels)

feeding <- feeding %>% select(feed1:feed6)

# Note: All shredder modalities were assigned to feed3 (hebivor)
# Missing: "Feeding_absorber_VicEPA", 


# ---- Respiration ---- #
# Explanation
# 1. resp1: Tegument
# 2. resp2: Gills
# 3. resp3: Plastron
# 4. resp4: Air/Atmospheric
# 5. resp5: Spiracle

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
  mutate(resp2_chessman.new = `Gills_(aquatic_stages)_fam_Chessman2017`,
         resp4_chessman.new = `Air_respiration_(aquatic_stages)_fam_Chessman2017`,
         resp5_chessman.new = `Functional_spiracles_(aquatic_stages)_fam_Chessman2017`)


(resp_names <- grep("new|Resp._botwe", names(fin_AUS), value = TRUE, ignore.case = TRUE))

respiration <- fin_AUS %>% select(resp_names)

respiration <- respiration %>%
  mutate(resp1 = Resp1_botwe, resp2 = Resp2_botwe, resp3 = Resp3_botwe,
         resp4 = resp4_maxwell.new, resp5 = resp5_maxwell.new) %>%
  mutate(resp1 = ifelse(is.na(resp1), resp1_maxwell.new, resp1),
         resp2 = ifelse(is.na(resp2), resp2_maxwell.new, resp2),
         resp3 = ifelse(is.na(resp3), resp3_maxwell.new, resp3)) %>%
  mutate(resp1 = ifelse(is.na(resp1), resp1_vicepa.new, resp1),
         resp2 = ifelse(is.na(resp2), resp2_vicepa.new, resp2),
         resp3 = ifelse(is.na(resp3), resp3_vicepa.new, resp3),
         resp5 = ifelse(is.na(resp5), resp5_vicepa.new, resp5)) %>%
  mutate(resp2 = ifelse(is.na(resp2), resp2_chessman.new, resp2),
         resp4 = ifelse(is.na(resp4), resp4_chessman.new, resp4),
         resp5 = ifelse(is.na(resp5), resp5_chessman.new, resp5)) %>%
  mutate(resp1 = ifelse(is.na(resp1), resp1_shafer.new, resp1),
         resp2 = ifelse(is.na(resp2), resp2_shafer.new, resp2),
         resp3 = ifelse(is.na(resp3), resp3_shafer.new, resp3),
         resp4 = ifelse(is.na(resp4), resp4_shafer.new, resp4)) %>%
  mutate(resp1 = ifelse(is.na(resp1), resp1_gbr.new, resp1),
         resp2 = ifelse(is.na(resp2), resp2_gbr.new, resp2),
         resp3 = ifelse(is.na(resp3), resp3_gbr.new, resp3),
         resp4 = ifelse(is.na(resp4), resp4_gbr.new, resp4)) %>%
  select(resp1:resp5) 


# ---- Dispersal ---- #
# Only (aquatic) drift data was summarised
# 1. drift1: low
# 2. drift2: medium
# 3. drift3: high
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
  mutate(drift1 = drift1_botwe.new, drift2 = drift2_botwe.new, drift3 = drift3_botwe.new) %>%
  mutate(drift1 = ifelse(is.na(drift1), drift1_vicepa.new, drift1),
         drift2 = ifelse(is.na(drift2), drift2_vicepa.new, drift2),
         drift3 = ifelse(is.na(drift3), drift3_vicepa.new, drift3)) %>%
  mutate(drift1 = ifelse(is.na(drift1), drift1_shafer.new, drift1),
         drift3 = ifelse(is.na(drift3), drift3_shafer.new, drift3)) %>%
  mutate(drift2 = ifelse(is.na(drift2), drift2_gbr.new, drift2),
         drift3 = ifelse(is.na(drift3), drift3_gbr.new, drift3)) %>%
  mutate(drift1 = ifelse(is.na(drift1), drift1_maxwell.new, drift1),
         drift3 = ifelse(is.na(drift3), drift3_maxwell.new, drift3)) %>%
  select(drift1:drift3)

# ---- Substrate Relation ---- #
# Definition: Substrate relation OR attachment OR (micro)habitat
# Only Botwe and VicEPA with data
# Sprawler = Crawler (?) -> "Attach_crawl_VicEPA" becomes substrate3
# Explanation:
# 1. substrate1: Burrow
# 2. substrate2: Climb
# 3. substrate3: Sprawl
# 4. substrate4: Cling
# 5. substrate5: Swim
# 6. substrate6: Skate
# 7. substrate7: Attached (temporary)
# 8. substrate8: Attached (permanent)

sub_names <- grepl("habi", names(fin_AUS), ignore.case = TRUE) | grepl("attach", names(fin_AUS), ignore.case = TRUE)

substrate <- fin_AUS[sub_names]

substrate <- substrate %>%
  mutate(substrate1 = Habi1_botwe,
         substrate2 = Habi2_botwe,
         substrate3 = Habi3_botwe,
         substrate4 = Habi4_botwe,
         substrate5 = Habi5_botwe,
         substrate6 = Habi6_botwe,
         substrate7 = Attach_temp_VicEPA,
         substrate8 = Attach_perm_VicEPA) %>%
  mutate(substrate5 = ifelse(is.na(substrate5), Attach_swim_VicEPA, substrate5),
         substrate1 = ifelse(is.na(substrate1), Attach_burrow_VicEPA, substrate1),
         substrate3 = ifelse(is.na(substrate3), Attach_crawl_VicEPA, substrate3)) %>%
  select(substrate1:substrate8)


# ---- Salinity Preference ---- #
# Only Botwe and VicEPA with data
# Modality "EC4_Maxwell" is missing in database

sal_names <- grepl("^sal", names(fin_AUS), ignore.case = TRUE) | grepl("^EC", names(fin_AUS), ignore.case = TRUE)

salinity <- fin_AUS[sal_names]

salinity[salinity == "NA"] <- 0

salinity <- salinity %>%
  mutate(salinity1 = Sal1_botwe,
         salinity2 = Sal2_botwe,
         salinity3 = Sal3_botwe,
         salinity4 = Sal4_botwe) %>%
  mutate(salinity1 = ifelse(is.na(salinity1), EC1_Maxwell, salinity1),
         salinity2 = ifelse(is.na(salinity2), EC2_Maxwell, salinity2),
         salinity3 = ifelse(is.na(salinity3), EC3_Maxwell, salinity3)) %>%
  select(salinity1:salinity4)


# ---- pH Preference ---- #
# Only Chessman with data
# Explanation (same levels as for Europe):
# 1. ph1: Acidic (ph < 7)
# 2. ph2: Neutral to alkaline (ph >= 7)

ph_names <- grepl("^ph", names(fin_AUS), ignore.case = TRUE)
(ph <- fin_AUS[ph_names])

# Transform pH values to numeric 
ph <- transform(ph, ph = as.numeric(pH_minimum_fam_Chessman2017)) %>%
  select(ph)

ph <- ph %>%
  mutate(ph1 = ifelse(ph < 7, 1, 0),
         ph2 = ifelse(ph >= 7, 1, 0)) %>%
  select(ph1, ph2)

# Remove NAs? Or replace with 0?


# ---- Temperature Preference ---- #
# Only Chessman and Botwe with data
# Chessman with numeric data can be transformed to ranges. Botwe data is mostly eurytherm.
# Ther1_botwe is excluded because of stenothermal AND eurythermal character

# Explanation (same levels as for Europe):  
# temp1: very cold (< 6 °C)
# temp2: cold (>=6 °C & < 10 °C)
# temp3: moderate (>=10 °C & < 18 °C)
# temp4: warm (> 18 °C)
# temp5: eurytherm (no specifice preference)


temp_names <- grepl("^ther", names(fin_AUS), ignore.case = TRUE)
(temperature <- fin_AUS[temp_names])

# Transform "Thermophily_Chessman2017" values to numeric 
temperature <- transform(temperature, temperature = as.numeric(Thermophily_Chessman2017)) %>%
  select(Ther1_botwe:Ther3_botwe, temperature)

temperature <- temperature %>%
  mutate(temp1 = ifelse(temperature < 6, 1, 0),
         temp2 = ifelse(temperature >= 6 & temperature < 10, 1, 0),
         temp3 = ifelse(temperature >= 10 & temperature < 18, 1, 0),
         temp4 = ifelse(temperature > 18, 1, 0),
         temp5 = NA) %>%
  mutate(temp5 = ifelse(is.na(temp5), Ther2_botwe, temp5), 
         temp5 = ifelse(is.na(temp5), Ther3_botwe, temp5)) %>%
  select(temp1:temp5)

# Remove NAs? Or replace with 0?


# ---- Life Duration ---- #
# Only Botwe and VicEPA with data
# Explanation:
# 1. life1: < 1 month
# 3. life2: > 1 month

life_names <- grepl("life", names(fin_AUS), ignore.case = TRUE)
(life <- fin_AUS[life_names])

life <- life %>%
  mutate(life1 = Life1_botwe, life2 = Life3_botwe) %>%
  mutate(life1 = ifelse(is.na(life1) | life1 == "0", Life2_botwe, life1),
         life1 = ifelse(is.na(life1) | life1 == "0", Total_lifespan_less_than_1m_VicEPA, life1)) %>%
  mutate(life2 = ifelse(is.na(life2) | life2 == "0", Total_lifespan_1_to_3m_VicEPA, life1),
         life2 = ifelse(is.na(life2) | life2 == "0", Total_lifespan_3_to_12m_VicEPA, life1),
         life2 = ifelse(is.na(life2) | life2 == "0", Total_lifespan_more_than_1y_VicEPA, life1)) %>%
  select(life1:life2)

# ---- Size ---- #
# Size in categories seems best option but problems with categories of VicEPA and Botwe
# Shafer and GBR with specific size in mm


# ---- Aquatic Stages ---- #
# Only VicEPA with data on aquatic stages of taxa


# NOTE: Information about emergence flight, resistance form, saprobity and dissemination strategy
# are not included in any database!



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