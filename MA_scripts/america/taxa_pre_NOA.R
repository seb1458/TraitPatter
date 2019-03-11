##############################################
#### Preparation: North America database  ####
##############################################
# ---- Taxa Information Preprocessing ----

# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)
library(data.table)
library(taxize)
library(stringr)


# --------------------------------------------------------------------------------------------------------------- #
#### Load data ####
US_trait_DB <- read.csv(file.path(path, "North America", "6138 inverttraitstable_csv.csv"), stringsAsFactors = FALSE)

US_trait_DB <- US_trait_DB %>% 
  filter(row_number() %% 2 != 1)

US_trait_DB <- as.data.table(US_trait_DB)


# --------------------------------------------------------------------------------------------------------------- #
#### Check Ovipos_behav_comment column ####
grep("ovo", US_trait_DB$Ovipos_behav_comment, ignore.case = TRUE) 


# --------------------------------------------------------------------------------------------------------------- #
#### Used scripts ####
# Load functions script
data_scripts <- "~/Schreibtisch/Thesis/scripts/america"
source(file = file.path(data_scripts, "Used_functions.R"))

# Functions: .fetch_dupl and .agg_dupl


# --------------------------------------------------------------------------------------------------------------- #
#### Prepare Taxa Information
# spp. in Taxon to NA
# all spp. taxa in Taxon column have an entry in Genus -> transform to NA
all(US_trait_DB[Taxon %like% "spp.", .(Genus = !is.na(Genus))]$Genus)
US_trait_DB[, Taxa := ifelse(Taxon %like% "spp.", NA, Taxon)]


# Same entry in Taxon & Family? -> Not always
all(US_trait_DB[Taxa %like% ".+dae", .(Taxon = ifelse(Taxa == Family, TRUE, FALSE))]$Taxa)


# Also few have an entry for Genus
# View(US_trait_DB[grep(".+dae", Taxon), .(Taxon, Genus)])
# -> That are those who are actually not Familynames


# fetch taxa in taxon that have 'dae' in name but are not in families column (data.table way)
US_trait_DB[Taxa %like% ".+dae" & Taxa == Family, Taxa := NA]
# Familienames in Taxon occur also in column Family, hence this is actually sufficient:
# US_trait_DB[Taxon == Family, Taxon := NA]
# check: US_trait_DB[Taxon %like% ".+dae",]


# Change col order
setcolorder(x = US_trait_DB, neworder = c("TraitRecord_ID", "Taxa", "Family", "Genus", "Taxon"))

# Remove unneccessary information
# US_trait_DB <- rename(US_trait_DB, Ovipos_com = Ovipos_behav_comments)
search_col <- grep("Study|Data|TraitRecord|Comment", names(US_trait_DB), ignore.case = TRUE, value = TRUE)
US_trait_DB[, c(search_col) := NULL]


# --------------------------------------------------------------------------------------------------------------- #
#### How to handle duplicates? ####

# fetch duplicated taxa
US_dupl <- .fetch_dupl(dat = US_trait_DB, x = US_trait_DB[, .(Taxa)])


# Get "conflict" data (multiple different entries for the same species)
# NoA_conflict <- as.data.table(US_dupl)
# 
# NoA_conflict <- NoA_conflict[, lapply(.SD, function(y) {
#   y <- y[!is.na(y)]
#   y <- y[!duplicated(y)]
#   y[length(y) > 1]
# }), by = Taxa, .SDcols = names(NoA_conflict)]


# Split up in taxa & trait data
US_dupl_taxa <- US_dupl[, c("Taxa", "Genus", "Family", "Taxon")]
US_dupl_trait <- US_dupl[, -grep("Taxa|Genus|Family|Taxon", names(US_dupl),
                                 ignore.case = TRUE
)]


# ! handle columns different with measured traits  
US_dupl_measured <- US_dupl_trait[, grep("Measured", names(US_dupl_trait),
                                         ignore.case = TRUE
)]
US_dupl_trait <- US_dupl_trait[, -grep("Measured", names(US_dupl_trait),
                                       ignore.case = TRUE
)]


# Aggregate traits
US_dupl_agg <- .agg_dupl(x = US_dupl_trait, index = US_dupl_taxa[, "Taxa"])


# Aggregate measured traits -> use mean
US_dupl_measured <- aggregate(US_dupl_measured,
                              by = list(US_dupl_taxa[, "Taxa"]),
                              mean, na.rm = TRUE
)

# Get unique values from taxa information
US_dupl_taxa <- US_dupl_taxa[!duplicated(US_dupl_taxa$Taxa), ]


# Bind all together
US_dupl <- cbind(US_dupl_taxa, US_dupl_agg[, -1], US_dupl_measured[, -1])


# Bring together with US_trait_DB
US_trait_DB <- rbind(US_trait_DB[!Taxa %in% US_dupl_taxa$Taxa], US_dupl)
# ? Actually better doing all this with data.table, preserving all the information
# Check outcome -> looks good!
# .fetch_dupl(dat = US_trait_DB, x = US_trait_DB[,.(Taxa)])


# Order according to Taxa column 
setorder(US_trait_DB, Taxa)


# --------------------------------------------------------------------------------------------------------------- #
#### Taxa Information ####
df_NOA <- US_trait_DB

df_NOA[df_NOA == ""] <- NA

# Merge taxa columns
df_NOA <- df_NOA %>%
  mutate(Taxa = coalesce(Taxa, Taxon)) %>%
  select(-Taxon) %>%
  select(Family, Genus, Taxa, everything())

head(df_NOA[, 1:5])

# If genus is NA insert first word of taxa column
df_NOA <- mutate(df_NOA, Genus = ifelse(is.na(Genus), word(Taxa, 1), Genus))

# Remove rows were genus information is family names
df_NOA <- df_NOA[!grepl("dae$", df_NOA$Genus), ]

# Are family names all availabe?
df_NOA[is.na(df_NOA$Family), 1:4]
# Missing family names, but genus names are subfamilies. Dimissed.

df_NOA <- df_NOA[!is.na(df_NOA$Family), ]


# Get order levels from "Global Biodiversity Information Facility" (gbif)
NOA_fam <- unique(df_NOA$Family)

tax_gbif <- get_ids(names = NOA_fam, db = "gbif")
cl_gbif <- cbind(classification(tax_gbif$gbif, return_id = FALSE)); beep(4)
cl_gbif <- cl_gbif %>% select(order, family)

df_NOA <- merge(cl_gbif, df_NOA, by.x = "family", by.y = "Family")
df_NOA <- df_NOA %>%
  select(order, family, Genus, Taxa, everything()) %>%
  rename(Order = order, Family = family)

# Some order names are missing
df_NOA <- df_NOA %>%
  
  # Nerillidae belongs to Aciculata
  mutate(Order = ifelse(grepl("Nerillidae", Family), "Aciculata", Order)) %>%
  
  # Acroloxidae, Lymnaeidae, Ancylidae, Planorbidae, Physidae belong to Pulmonata
  mutate(Order = ifelse(grepl("Acroloxidae|Lymnaeidae|Ancylidae|Planorbidae|Physidae", Family), "Pulmonata", Order)) %>%
  
  # Pleuroceridae belong to Sorbeoconcha
  mutate(Order = ifelse(grepl("Pleuroceridae", Family), "Sorbeoconcha", Order)) %>%
  
  # Valvatidae belongs to Triganglionata
  mutate(Order = ifelse(grepl("Valvatidae", Family), "Triganglionata", Order)) %>%
  
  # Pilidae belongs to Architaenioglossa
  mutate(Order = ifelse(grepl("Pilidae", Family), "Architaenioglossa", Order)) %>%
  
  # Uchidastygacaridae belongs to Trombidiformes
  mutate(Order = ifelse(grepl("Uchidastygacaridae", Family), "Trombidiformes", Order)) %>%
  
  # Aeolosomatidae no order known (incertae sedis)
  mutate(Family = ifelse(grepl("Aeolosomatidae", Family), NA, Family)) 

# Select only rows with non-NA in order column
df_NOA <- df_NOA[!is.na(df_NOA$Order), ]

# Taxonomic information complete

# --------------------------------------------------------------------------------------------------------------- #
#### Write Table ####
write.table(df_NOA, file = "~/Schreibtisch/Thesis/data/North America/macroinvertebrate_NAM_tax.csv", sep = ",")
