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

# --- Genus column
df_AUS <- df_AUS %>%
  select(Order, Family, Genus, Genus_and_species, long_code, everything())

# --- Remove rows with all NAs in the taxon and code columns
df_AUS <- df_AUS[rowSums(is.na(df_AUS[1:5])) < 5, ] 

# --- Genus_and_species + SAName_botwe
df_AUS$Genus_and_species <- coalesce(df_AUS$Genus_and_species, df_AUS$SAName_botwe)

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
#### Final Preparation of Taxa Information ####
# --- Delete incomplete information
# When NA in Family and long_code: no identification possible
# df_AUS <- df_AUS %>%
#   filter(!(is.na(Family) & is.na(long_code)))
# 
# --- Strange code data: MITEXXXX dismissed for now
# names_AUS <- names_AUS[!grepl("mite", names_AUS$long_code, ignore.case = TRUE), ]



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
#### Correct wrong entries ####

# --- Order
levels(as.factor(df_AUS$Order))

df_AUS[grepl("Diplostraca", df_AUS$Order), "Order"] <- "Diplostraca"

# Super Order Syncarida contains two families: Koonungidae (Order: Anaspidacea) and Parabathynellidae (Order: Bathynellacea)
df_AUS[grepl("Koonungidae", df_AUS$Family), "Order"] <- "Anaspidacea"
df_AUS[grepl("Parabath", df_AUS$Family), "Order"] <- "Bathynellacea"

# --- Family
levels(as.factor(df_AUS$Family))

# Correct entries and delete all entries not ending with "-dae"
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
         Family = ifelse(Family == "Gripopterygidae (Plecoptera)", "Gripopterygidae", Family)) %>%
  mutate(Family = ifelse(!grepl("dae$", df_AUS$Family), NA, df_AUS$Family))


# --- Genus
levels(as.factor(df_AUS$Genus))

# Correct entries and delete all entries ending with "-dae" and all nonsense
df_AUS <- df_AUS %>%
  mutate(Genus = ifelse(grepl("Notriolus", Genus), "Notriolus", Genus),
         Genus = ifelse(grepl("shuckardi", Genus), "Shuckardi", Genus),
         Genus = ifelse(grepl("tripunctatus", Genus), "Tripunctatus", Genus),
         Genus = ifelse(grepl("zealandicus", Genus), "Zealandicus", Genus)) %>%
  mutate(Genus = ifelse(grepl("^sp|dae$|dea$|[0-9]|Family|Order|with|juveniles|genus|Genus|grape", Genus),
                        NA, Genus),
         Genus = ifelse(grepl("Trichoptera|Telmatopelopia|Mites|Plecoptera|Diptera|Isopoda|Coleoptera|Conchostraca|Turbellaria|Neuroptera|Polychaet|Oligochaeta|Pelecypoda|Infra|Odonata", Genus),
                        NA, Genus))

# Lot of subfamily for Chironomidae
df_AUS <- df_AUS %>%
  mutate(Family = ifelse(grepl("Chironomini|Tanytarsini|Chironominae|Orthocladiinae|Tanypodinae|Pentaneurini|Podonominae|Diamesinae|Aphroteniinae", Genus), 
                         "Chironomidae", Family)) %>%
  mutate(Genus = ifelse(grepl("Chironomini|Tanytarsini|Chironominae|Orthocladiinae|Tanypodinae|Pentaneurini|Podonominae|Diamesinae|Aphroteniinae", Genus), 
                        NA, Genus))

# --- Species
df_AUS <- df_AUS %>%
  mutate(Species = ifelse(grepl("imm|Peza|Unid|juveniles", Species), NA, Species))


# --------------------------------------------------------------------------------------------------------------- #
#### Get Taxonomic Information via taxize ####

# --- Get information for missing order, family and genus
tax_miss <- df_AUS[which((is.na(df_AUS$Genus)) | (is.na(df_AUS$Family)) | (is.na(df_AUS$Order))),
                    c("Family", "Genus", "Species", "id_join")]

# --- First: Species information
# Only non-NA cases
spec_miss <- tax_miss[!is.na(tax_miss$Species), ]

# Some taxa with multiple names, or label for "Adult"/"Larva"
spec_miss$Species <- word(spec_miss$Species, start = 1, end = 2)
spec_miss <- spec_miss[!duplicated(spec_miss$Species), ]

# Remove nonsense like "crab imm", "Peza ops", and taxa containing "juveniles"
# Also Diaprecoris barycephala and Hydrophilus latipalpus can not be found
spec_miss <- spec_miss[!grepl("Diaprecoris|latipalpus", spec_miss$Species), ]

# Get ids from gbif, and classify
spec_gbif <- spec_miss$Species
ids_gbif <- get_ids(spec_gbif, db = "gbif")
class_spec <- cbind(classification(ids_gbif, db= "gbif", return_id = FALSE))

tax_spec <- select(class_spec, order, family, genus, species)
tax_spec$id_join <- spec_miss$id_join

# Merge with df_AUS
df_AUS <- merge(x = df_AUS, y = tax_spec, by = "id_join", all.x = TRUE)

df_AUS <- df_AUS %>%
  mutate(Order = coalesce(order, Order),
         Family = coalesce(family, Family),
         Genus = coalesce(genus, Genus)) %>%
  select(-c(order:species))


# --- Second: Genus information
gen_miss <- tax_miss[!is.na(tax_miss$Genus), c("Family", "Genus", "id_join")]
gen_miss$Genus

# Get ids from gbif, and classify
gen_gbif <- gen_miss$Genus
ids_gbif <- get_ids(gen_gbif, db = "gbif")
class_gen <- cbind(classification(ids_gbif, db= "gbif", return_id = FALSE))

tax_gen <- select(class_gen, order, family, genus)
tax_gen$id_join <- gen_miss$id_join

# Merge with df_AUS
df_AUS <- merge(x = df_AUS, y = tax_gen, by = "id_join", all.x = TRUE)

df_AUS <- df_AUS %>%
  mutate(Order = coalesce(order, Order),
         Family = coalesce(family, Family),
         Genus = coalesce(genus, Genus)) %>%
  select(-c(order:genus))


# --- Third: Family information
fam_miss <- tax_miss[!is.na(tax_miss$Family), c("Family", "id_join")]

# Remove family names, nonsense (sp., Order, ...) or wrong categories (Neuroptera, Oligochaeta, ...)
fam_miss[grepl("Aturidae", fam_miss$Family), 1] <- "Aturidae"
fam_miss[grepl("(Unident.)", fam_miss$Family), 1] <- "Hymenostomatidae"
fam_miss[grepl("Calocid/Helicophidae", fam_miss$Family), 1] <- "Helicophidae"

fam_miss <- fam_miss[grepl("dae$", fam_miss$Family), ]

# Get ids from gbif, and classify
fam_gbif <- fam_miss$Family
ids_gbif <- get_ids(fam_gbif, db = "gbif")
class_fam <- cbind(classification(ids_gbif, db= "gbif", return_id = FALSE))

tax_fam <- select(class_fam, order, family)
tax_fam$id_join <- fam_miss$id_join

# Merge with df_AUS
df_AUS <- merge(x = df_AUS, y = tax_fam, by = "id_join", all.x = TRUE)

df_AUS <- df_AUS %>%
  mutate(Order = coalesce(order, Order),
         Family = coalesce(family, Family)) %>%
  select(-c(order:family), -id_join)

# ---------------------------------------------------------------------------------------------------------------- #
# Write .csv 
# write.table(df_AUS, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_tax2.csv", sep = ",")

df_AUS <- read.csv(file.path(path, "Australia", "macroinvertebrate_AUS_tax2.csv"), stringsAsFactors = FALSE)

# Still missing data
View(df_AUS[which((is.na(df_AUS$Genus)) | (is.na(df_AUS$Family)) | (is.na(df_AUS$Order))), 1:7])

# ------------------------------------------------------- #
# --- Still missing data

# --- Genus still with wrong information
levels(as.factor(df_AUS[which((is.na(df_AUS$Genus)) | (is.na(df_AUS$Family)) | (is.na(df_AUS$Order))), 3]))

# Some Genus names are order names or something else
# Amphipoda is an order already existing in the order column
# Astigmata is an order already existing in the order column
# Lepidoptera is an order name, already existing in the order column
# Mesostigmata is an order name, already existing in the order column
# Ephemeroptera is an order name, alread exissting in the order column
df_AUS <- df_AUS %>%
  mutate(Genus = ifelse(grepl("Amphipoda|Astigmata|Hemiptera|Lepidoptera|Mesostigmata|Ephemeroptera", Genus),
       NA, Genus))


# Gastropoda is a class name
# Crustacea is a phylum
# Ectoprocta is a phylum
# Nematoda is a phylum name
# Nematomorpha is a phylum name
# Nemertea is a phylum name
df_AUS <- df_AUS %>%
  mutate(Genus = ifelse(grepl("Gastropoda|Crustacea|Ectoprocta|Nematoda|Nematomorpha|Nemertea", Genus), NA, Genus)) %>%
  mutate(Family = ifelse(grepl("Crustacea", Family), NA, Family)) 


# Austropeplea, Bayardella, Ferrissia, Glyptophysa, Gyraulus, Helicorbis, Isidorella, Physa, Pseudosuccinea
# are genus names, belonging to Pulmonata order
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Austropeplea|Bayardella|Ferrissia|Glyptophysa|Gyraulus|Helicorbis|Isidorella|Physa|Pseudosuccinea", Genus),
                        "Pulmonata", Order))


# Plotiopsis is genus name, belonging to Sorbeoconcha order
df_AUS[grepl("Plotiopsis", df_AUS$Genus), "Order"] <- "Sorbeoconcha"

# Syncardia does no exist, order Super Order Syncarida is also not sufficient for identification -> Delete
df_AUS <- df_AUS[!grepl("Syncardia", df_AUS$Genus), ]

# Zygoptera is suborder name
df_AUS <- df_AUS %>%
  mutate(Genus = ifelse(grepl("Zygoptera", Genus), NA, Genus))

# Physa belongs to Physidae family
df_AUS <- df_AUS %>%
  mutate(Family = ifelse(grepl("Physa", Genus), "Physidae", Family))


# --- Family still with wrong information
levels(as.factor(df_AUS[which((is.na(df_AUS$Family)) | (is.na(df_AUS$Order))), 2]))

# Aeolosomatidae, order unknown
df_AUS[grepl("Aeolosomatidae", df_AUS$Family), 1:6]

# Capitellidae, order unknown
df_AUS[grepl("Capitellidae", df_AUS$Family), 1:6]

# Glacidorbidae: order is Triganglionata
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Glacidorbidae", Family), "Triganglionata", Order))


# --- Correct order names
levels(as.factor(df_AUS$Order))

# Passeriformes wrong order
df_AUS[grepl("Passeriformes", df_AUS$Order), ] <- NA

# Anthoathecatae wrong spelling
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Anthoathecatae", Order), "Anthoathecata", Order))

# Basommatophora is suborder, order is Pulmonata
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Basommatophora", Order), "Pulmonata", Order))

# Gordioidea wrong name, order is Gordea
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Gordioidea", Order), "Gordea", Order))

# Hoplonemertea is suborder, 
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Hoplonemertea", Order), "Monostilifera", Order))

# Hygrophila wrong order, Families Lymnaeidae, Physidae and Planorbidae belong to Pulmonata
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Hygrophila", Order), "Pulmonata", Order))

# Hypsogastropoda, Littorinimorpha wrong name, order is Sorbeoconcha
# But also genera with wrong families
df_AUS <- df_AUS %>%
  mutate(Order = ifelse(grepl("Hypsogastropoda|Littorinimorpha|Neogastropoda", Order), "Sorbeoconcha", Order)) %>%
  mutate(Family = ifelse(grepl("Austropyrgus|Posticobia|Potamopyrgus|Angrobia", Genus), "Hydrobiidae", Family),
         Family = ifelse(grepl("Glacidorbis", Genus), "Glacidorbidae", Family))


# --- Remove rows with:
# 1. All NAs in taxonomic columns
df_AUS <- df_AUS[!rowSums(is.na(df_AUS[1:4])) == 4, ]

# 2. All rows with only order name
df_AUS <- df_AUS[!rowSums(is.na(df_AUS[2:4])) == 3, ]

# 3. All rows with only order and family name
df_AUS <- df_AUS[!rowSums(is.na(df_AUS[3:4])) == 2, ]

# Taxonomic information is complete

# --------------------------------------------------------------------------------------------------------------- #
#### Final Table ####

# Write .csv 
write.table(df_AUS, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_tax.csv", sep = ",")
