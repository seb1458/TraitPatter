# Data processing US_DB

#---------------------------------------#
#### Packages ####
library(data.table)
library(readxl)

#---------------------------------------#
#### Data directory ####
data_in <- "/home/kunz/Dokumente/Trait DB"

# Location of loaded scripts
data_scripts <- "/home/kunz/Dokumente/Scripts/TP_stage1"

#---------------------------------------#

#### Used scripts ####
# Load functions script
source(file = file.path(data_scripts, "Used_functions_NoGH.R"))

#---------------------------------------#

# Load US DB
US_trait_DB <- read_excel(file.path(data_in, "North America", "6138 inverttraitstable.xls"), sheet = 1)
US_trait_DB <- as.data.table(US_trait_DB)


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
search_col <- grep("Study|Data|Adult|TraitRecord|Comment", names(US_trait_DB), ignore.case = TRUE, value = TRUE)
US_trait_DB[, c(search_col) := NULL]

#---------------------------------------#

#### How to handle duplicates? ####

# fetch duplicated taxa
US_dupl <- .fetch_dupl(dat = US_trait_DB, x = US_trait_DB[, .(Taxa)])

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