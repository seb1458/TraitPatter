#########################################
#### Preparation: Australia database ####
#########################################
#---- Taxa Information Preprocessing ----

# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(data.table)
library(stringr)
library(readxl)
library(taxize)


# --------------------------------------------------------------------------------------------------------------- #
#### Load data ####
df_AUS <- read_excel(file.path(path, "Australia", "Australian macroinv trait database.xlsx"), sheet = 1)

# Add ID for later join
df_AUS$id_join <- 1:nrow(df_AUS)

# Change NULL, N/A, "NA" to NA
df_AUS[df_AUS == "NULL"] <- NA
df_AUS[df_AUS == "N/A"] <- NA
df_AUS[df_AUS == "NA"] <- NA


# --------------------------------------------------------------------------------------------------------------- #
#### Query taxon information ####
# --- Order + Order_bugs_gbr + Order_fam_Chessman2017
df_AUS$Order <- coalesce(df_AUS$Order, df_AUS$Order_bugs_gbr, df_AUS$Order_fam_Chessman2017)

# --- Family + name_in_Schafer + TrueFamily
df_AUS$Family <- coalesce(df_AUS$Family, df_AUS$name_in_Schafer, df_AUS$TrueFamily)

# --- Genus column: just one column

# --- Genus_and_species + SAName_botwe
df_AUS$Genus_and_species <- coalesce(df_AUS$Genus_and_species, df_AUS$SAName_botwe)

# Order hierarchically
df_AUS <- df_AUS %>%
  select(Order, Family, Genus, Genus_and_species, long_code, everything())

# --- Remove rows with all NAs in the taxon and code columns
df_AUS <- df_AUS[rowSums(is.na(df_AUS[1:5])) < 5, ]


# --- Find and remove duplicated entries in names_AUS$long_code
df_AUS %>% 
  group_by(long_code) %>% 
  filter(n() > 1) %>%
  arrange(desc(long_code)) %>%
  select(long_code:Genus_and_species)

# Select only entries without duplicate entries in long_code. NAs are not accounted for as duplicates.
df_AUS <- df_AUS[!duplicated(df_AUS$long_code, incomparables = NA), ]


# --------------------------------------------------------------------------------------------------------------- #
#### Table Join with the ID list ####
# ID list from Ben Kefford with long_code data and taxon information
# NOTE: The file containing the long_codes has some format errors for some cells. Removed manually in spreadsheet.
id_sheet1 <- read_excel(file.path(path, "Australia", "NEW_VicEPA_Codes.xlsx"), sheet = 1)
id_sheet1 <- id_sheet1 %>%
  select(-PHYLUM, -CLASS, -X__2, -X__3) %>%
  rename(long_code = X__1, Order = ORDER, Family = FAMILY, Species = SPECIES)

id_sheet2 <- read_excel(file.path(path, "Australia", "NEW_VicEPA_Codes.xlsx"), sheet = 2)
id_sheet2 <- id_sheet2 %>%
  select(-CLASS, -Comments) %>%
  rename(long_code = Code, Order = ORDER, Family = FAMILY, Species = SPECIES)

id_sheet3 <- read_excel(file.path(path, "Australia", "NEW_VicEPA_Codes.xlsx"), sheet = 3)
id_sheet3 <- id_sheet3 %>%
  select(-CLASS, -Comments) %>%
  rename(long_code = Code, Order = ORDER, Family = FAMILY, Species = SPECIES)

id_sheet4 <- read_excel(file.path(path, "Australia", "TaxaDb.xls"), sheet = 1)
id_sheet4 <- id_sheet4 %>%
  select(X__1, Order, Family, Species) %>%
  rename(long_code = X__1)

# Put into one object
id_list <- rbind(id_sheet1, id_sheet2, id_sheet3, id_sheet4) %>%
  mutate(Genus = word(Species, 1)) %>%
  select(long_code, Order, Family, Genus, Species) %>%
  rename(Order.vic = Order, Family.vic = Family, Genus.vic = Genus, Species.vic = Species)


# Remove duplicated entries and NAs from id_list
id_list <- id_list[!duplicated(id_list$long_code, incomparables = NA), ]
id_list <- id_list[!is.na(id_list$long_code), ]

# NOTE: Some codes are in df_AUS but not in id_list as well as some missing entries for names_AUS$long_code
setdiff(df_AUS$long_code, id_list$long_code)
sum(is.na(df_AUS$long_code))

# --- Use left table join to extend the df_AUS data
df_AUS <- merge(x = df_AUS, y = id_list, by = "long_code", all.x = TRUE)

# Combine information from both tables in new columns. Delete old columns
df_AUS <- df_AUS %>%
  mutate(Order = coalesce(Order.vic, Order),
         Family = coalesce(Family.vic, Family),
         Genus = coalesce(Genus.vic, Genus),
         Genus_and_species = coalesce(df_AUS$Genus_and_species, df_AUS$Species.vic)) %>%
  select(-Order.vic, -Family.vic, -Species.vic, -Genus.vic)


# --------------------------------------------------------------------------------------------------------------- #
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


# --------------------------------------------------------------------------------------------------------------- #
#### Dealing with Missing/Wrong Data ####

# Correct wrong genera names (Extend by first part of species names)
df_AUS <- df_AUS %>%
  mutate(genus_ext = word(Species, 1)) %>%
  mutate(genus_real = coalesce(genus_ext, Genus)) %>%
  select(-Genus, -genus_ext) %>%
  rename(Genus = genus_real) %>%
  select(Order, Family, Genus, everything())

# Now if genus is NA, no identification possible
df_AUS <- df_AUS[!is.na(df_AUS$Genus), ]

# Just five cases where entry for genus exists but family name does not
# Additionally these genus entries are all order names
df_AUS[is.na(df_AUS$Family) & !is.na(df_AUS$Genus), 1:5]
# Delete, because no identification possible
df_AUS[is.na(df_AUS$Family) & !is.na(df_AUS$Genus), ] <- NA

# Now if familiy is NA, no identification possible
df_AUS <- df_AUS[!is.na(df_AUS$Family), ]

# Lot of order names missing, where taxonomic information is available
df_AUS[is.na(df_AUS$Order) & !is.na(df_AUS$Family) , 1:5]
# Check again later

# Also wrong information in genus column for some rows:
# 1. Class, family, subfamily and tribe names
df_AUS <- df_AUS[!grepl("dae$|inae$|ini$|inea$", df_AUS$Genus), ]

# 2. Nonsense
df_AUS <- df_AUS[!grepl("Bivalve|crab|^sp|Group|Order|Family|Genus|genus|Mites|Infra|[0-9]|Hel$|grape|dark|light|with",
                        df_AUS$Genus), ]

# 3. Correct wrong entries
df_AUS <- df_AUS %>%
  mutate(Genus = ifelse(grepl("Notriolus", Genus), "Notriolus", Genus))

# 4. Wrong entries
df_AUS <- df_AUS[!grepl("Telmatopelopia|Conchostraca", df_AUS$Genus), ]

# 5. Dismiss larval stages from database
df_AUS <- df_AUS[!grepl("I$", df_AUS$long_code), ]


# --------------------------------------------------------------------------------------------------------------- #
#### Get Taxonomic Information via taxize ####

# Get taxonomic information  for genera names
genera_gbif <- unique(df_AUS$Genus)

# Wrong spelling
df_AUS[grepl("Helicopysche", df_AUS$Genus), "Genus"] <- "Helicopsyche" 

# Correct names
genera_gbif <- unique(df_AUS$Genus)
genera_gbif <- genera_gbif[!grepl("Austrosimulium|Diptera|Compterosmittia", genera_gbif)]

genera_ids <- get_ids(genera_gbif, db = "gbif")

genera_class <- cbind(classification(genera_ids, db = "gbif"))

genera_class <- genera_class %>%
  select(order, family, genus) %>%
  rename(order.gbif = order, family.gbif = family)

# Merge with df_AUS
df_AUS <- merge(df_AUS, genera_class, by.x = "Genus", by.y = "genus", all.x = TRUE)
df_AUS <- select(df_AUS, Order, order.gbif, Family, family.gbif, Genus, everything())

# Write .csv 
# write.table(df_AUS, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_tax2.csv", sep = ",")

# --------------------------------------------------------------------------------------------------------------- #
#### Correct wrong genus entries: Genera names ####
df_AUS <- read.csv(file.path(path, "Australia", "macroinvertebrate_AUS_tax2.csv"), stringsAsFactors = FALSE)

# Get rows were gbif did not find taxonomic information
df_AUS[is.na(df_AUS$order.gbif) | is.na(df_AUS$family.gbif), 1:6]

df_AUS <- df_AUS %>% 
  # Adversaeschna is a subgenus belonging to Aeshna
  mutate(order.gbif = ifelse(grepl("Adversaeschna", Genus), "Odonata", order.gbif),
         family.gbif = ifelse(grepl("Adversaeschna", Genus), "Aeshnidae", family.gbif),
         Genus = ifelse(grepl("Adversaeschna", Genus), "Aeshna", Genus)) %>%
  
  # Alathiria is spelled wrong -> Alathyria
  # family.gbif: Hyriidae, order: Unionoida
  mutate(order.gbif = ifelse(grepl("Alathiria", Genus), "Unionoida", order.gbif),
         family.gbif = ifelse(grepl("Alathiria", Genus), "Hyriidae", family.gbif),
         Genus = ifelse(grepl("Alathiria", Genus), "Alathyria", Genus)) %>%
  
  # Amerianna, Bayardella, Ferrissia, Glyptophysa, Gyraulus, Helicorbis, Isidorella, Planorbarius
  # belong to Planorbidae family, and to Pulmonata order
  mutate(order.gbif = ifelse(grepl("Amerianna|Bayardella|Ferrissia|Glyptophysa|Gyraulus|Helicorbis|Isidorella|Planorbarius", Genus), "Pulmonata", order.gbif),
         family.gbif = ifelse(grepl("Amerianna|Bayardella|Ferrissia|Glyptophysa|Gyraulus|Helicorbis|Isidorella|Planorbarius", Genus), "Planorbidae", family.gbif)) %>%
  
  # Austropeplea, Lymnaea belong to Lymnaeidae family and to Pulmonata order
  mutate(order.gbif = ifelse(grepl("Austropeplea|Lymnaea", Genus), "Pulmonata", order.gbif),
         family.gbif = ifelse(grepl("Austropeplea|Lymnaea", Genus), "Lymnaeidae", family.gbif)) %>%
  
  # Austroaeshna belongs to Aeshnidae family and Odonata order
  mutate(order.gbif = ifelse(grepl("Austroaeshna", Genus), "Odonata", order.gbif),
         family.gbif = ifelse(grepl("Austroaeshna", Genus), "Aeshnidae", family.gbif)) %>%
  
  # Austrosimulium belongts to Simuliidae family and Diptera order
  mutate(order.gbif = ifelse(grepl("Austrosimulium", Genus), "Diptera", order.gbif),
         family.gbif = ifelse(grepl("Austrosimulium", Genus), "Simuliidae", family.gbif)) %>%
  
  # Calopsectra belongts to Chironomidae family and Diptera order
  mutate(order.gbif = ifelse(grepl("Calopsectra", Genus), "Diptera", order.gbif),
         family.gbif = ifelse(grepl("Calopsectra", Genus), "Chironomidae", family.gbif)) %>%
  
  # Diaprecoris belongts to Corixidae family and Hemiptera order
  mutate(order.gbif = ifelse(grepl("Diaprecoris", Genus), "Hemiptera", order.gbif),
         family.gbif = ifelse(grepl("Diaprecoris", Genus), "Corixidae", family.gbif)) %>%
  
  # Dineutis wrong spelling -> Dineutus
  mutate(order.gbif = ifelse(grepl("Dineutis", Genus), "Coleoptera", order.gbif),
         family.gbif = ifelse(grepl("Dineutis", Genus), "Gyrinidae", family.gbif),
         Genus = ifelse(grepl("Dineutis", Genus), "Dineutus", Genus)) %>%
  
  # Flabellifrontipoba is subgenus name, genus is Oxus
  mutate(order.gbif = ifelse(grepl("Flabellifrontipoba", Genus), "Trombidiformes", order.gbif),
         family.gbif = ifelse(grepl("Flabellifrontipoba", Genus), "Oxidae", family.gbif),
         Genus = ifelse(grepl("Flabellifrontipoba", Genus), "Oxus", Genus)) %>%
  
  # Glacidorbis belongs to Glacidorbidae family and Triganglionata order
  mutate(order.gbif = ifelse(grepl("Glacidorbis", Genus), "Triganglionata", order.gbif),
         family.gbif = ifelse(grepl("Glacidorbis", Genus), "Glacidorbidae", family.gbif)) %>%
  
  # Haitia belongs to Lythraceae family and Myrtales order
  mutate(order.gbif = ifelse(grepl("Haitia", Genus), "Myrtales", order.gbif),
         family.gbif = ifelse(grepl("Haitia", Genus), "Lythraceae", family.gbif)) %>%
  
  # Helicopysche wrong name -> Helicopsyche
  mutate(Genus = ifelse(grepl("Helicopysche", Genus), "Helicopsyche", Genus)) %>%
  
  # Heterolimnesia belongs to Limnesiidae family and Trombidiformes order
  mutate(order.gbif = ifelse(grepl("Heterolimnesia", Genus), "Trombidiformes", order.gbif),
         family.gbif = ifelse(grepl("Heterolimnesia", Genus), "Limnesiidae", family.gbif)) %>%
  
  # Leptocerid belongs to Leptoceridae family and Trichoptera order
  mutate(order.gbif = ifelse(grepl("Leptocerid", Genus), "Trichoptera", order.gbif),
         family.gbif = ifelse(grepl("Leptocerid", Genus), "Leptoceridae", family.gbif)) %>%
  
  # Nebiossophlebia wrong name, correct Neboissophlebia
  mutate(Genus = ifelse(grepl("Nebiossophlebia", Genus), "Neboissophlebia", Genus)) %>%
  
  # Paradixa old name, new name is Dixella
  mutate(order.gbif = ifelse(grepl("Paradixa", Genus), "Diptera", order.gbif),
         family.gbif = ifelse(grepl("Paradixa", Genus), "Dixidae", family.gbif),
         Genus = ifelse(grepl("Paradixa", Genus), "Dixella", Genus)) %>%
  
  # Physa is wrong genus name -> Physella
  mutate(order.gbif = ifelse(grepl("Physa", Genus), "Planorboidea", order.gbif),
         family.gbif = ifelse(grepl("Physa", Genus), "Physidae", family.gbif),
         Genus = ifelse(grepl("Physa", Genus), "Physella", Genus)) %>%
  
  # Plotiopsis belongs to Thiaridae family and Sorbeoconcha order
  mutate(order.gbif = ifelse(grepl("Plotiopsis", Genus), "Sorbeoconcha", order.gbif),
         family.gbif = ifelse(grepl("Plotiopsis", Genus), "Thiaridae", family.gbif)) %>%
  
  # Wrong spelling for Rhyncaustrobates, correct: Rhynchaustrobates, belonging to Trombidiformes order
  mutate(order.gbif = ifelse(grepl("Rhyncaustrobates", Genus), "Trombidiformes", order.gbif),
         Genus = ifelse(grepl("Rhyncaustrobates", Genus), "Rhynchaustrobates", Genus)) %>%
  
  # Stylaria belongs to Naididae family and Haplotaxida
  mutate(order.gbif = ifelse(grepl("Stylaria", Genus), "Haplotaxida", order.gbif),
         family.gbif = ifelse(grepl("Stylaria", Genus), "Naididae", family.gbif)) %>%
  
  # Thiara belongs to Thiaridae family and Sorbeoconcha order
  mutate(order.gbif = ifelse(grepl("Thiara", Genus), "Sorbeoconcha", order.gbif),
         family.gbif = ifelse(grepl("Thiara", Genus), "Thiaridae", family.gbif)) %>%
  
  # Tubifex belongs to Tubificidae family and Haplotaxida order
  mutate(order.gbif = ifelse(grepl("Tubifex", Genus), "Haplotaxida", order.gbif),
         family.gbif = ifelse(grepl("Tubifex", Genus), "Tubificidae", family.gbif))

# Dineutus and all other species with two entries for adult and larva, delete larva
df_AUS <- df_AUS[!grepl("larva", df_AUS$Species, ignore.case = TRUE), ]

# Higher taxonomic names in genus column: 
# Amphipoda, Astigmata, Austropelopia, Coleoptera, Crustacea, Diptera, Ephmeroptera, 
# Gastropoda, Halacaroidea, Hemiptera, Isopoda, Lepidoptera, Mesostigmata, Nemertea,
# Odonata, Oribatida, Plecoptera, Polychaeta, Polychaete, Syncarida, Trichoptera,
# Trombidioidea, Turbellaria, Zygoptera, Pygmanisus -> No identification possible
high_tax <- c("Amphipoda", "Astigmata", "Austropelopia", "Coleoptera", "Crustacea", "Diptera", "Ephmeroptera", 
              "Gastropoda", "Halacaroidea", "Hemiptera", "Isopoda", "Lepidoptera", "Mesostigmata", "Nemertea",
              "Odonata", "Oribatida", "Plecoptera", "Polychaeta", "Polychaete", "Syncarida", "Trichoptera",
              "Trombidioidea", "Turbellaria", "Zygoptera", "Pygmanisus", "Nematoda", "Nematomorpha")

df_AUS <- df_AUS[!(df_AUS$Genus %in% high_tax), ]


# --------------------------------------------------------------------------------------------------------------- #
#### Correct wrong genus entries: Order names ####
sort(levels(as.factor(df_AUS$Order)))

df_AUS[grepl("Diplo", df_AUS$Order), 1] <- "Diplostraca"


# --------------------------------------------------------------------------------------------------------------- #
#### Final Merge ####
names(df_AUS)[1:6]

df_AUS <- df_AUS %>%
  mutate(order.new = coalesce(order.gbif, Order),
         family.new = coalesce(family.gbif, Family)) %>%
  select(-c(Order, order.gbif, Family, family.gbif)) %>%
  rename(Order = order.new, Family = family.new) %>%
  select(Order, Family, everything())

# Everything complete
df_AUS[is.na(df_AUS$Genus) | is.na(df_AUS$Family) | is.na(df_AUS$Order), 1:5]


# --------------------------------------------------------------------------------------------------------------- #
#### Final Table ####

# Write .csv 
write.table(df_AUS, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_tax.csv", sep = ",")
