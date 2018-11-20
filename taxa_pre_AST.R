#### Preparation: Australia database ####
#---- Taxa Information Preprocessing ----


#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"



#### Packages ####
library(tidyverse)
library(data.table)
library(readxl)



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



#### Write .csv ####
write.table(df_AUS, file = "~/Schreibtisch/Thesis/data/Australia/macroinvertebrate_AUS_tax.csv", sep = ",")
