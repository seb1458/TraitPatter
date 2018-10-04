# Data processing US_DB

#---------------------------------------#
#### Packages ####
library(data.table)
library(readxl)

#---------------------------------------#
#### Data directory ####
data_in <- "/home/kunz/Dokumente/Trait DB"

#---------------------------------------#

# Load US DB
US_trait_DB <- read_excel(file.path(data_in, "North America", "6138 inverttraitstable.xls"), sheet = 1)
US_trait_DB <- as.data.table(US_trait_DB)

# spp. in Taxon to NA
# all spp. taxa in Taxon column have an entry in Genus -> transform to NA
all(US_trait_DB[Taxon %like% "spp.", .(Genus = !is.na(Genus))]$Genus)
US_trait_DB[, Taxon := ifelse(Taxon %like% "spp.", NA, Taxon)]

# Same entry in Taxon & Family? -> Not always
all(US_trait_DB[Taxon %like% ".+dae", .(Taxon = ifelse(Taxon == Family, TRUE, FALSE))]$Taxon)

# Also few have an entry for Genus
# View(US_trait_DB[grep(".+dae", Taxon), .(Taxon, Genus)])
# -> That are those who are actually not Familynames

# fetch taxa in taxon that have 'dae' in name but are not in families column (data.table way)
US_trait_DB[Taxon %like% ".+dae" & Taxon == Family, Taxon := NA]
# Familienames in Taxon occur also in column Family, hence this is actually sufficient:
# US_trait_DB[Taxon == Family, Taxon := NA]
# check: US_trait_DB[Taxon %like% ".+dae",]