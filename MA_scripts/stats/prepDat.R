######################
#### Prepare Data ####
######################


# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
NMDS <- read.table(file.path(path, "final", "macroinvertebrate_ALL_bin_fancy.csv"), sep = ",", row.names = 1, header = TRUE)


# --------------------------------------------------------------------------------------------------------------- #
#### Preparation ####
names(NMDS)

# Check for Order
# Low number of families in all databases
order_EUR <- unique(NMDS[grepl("Europe", NMDS$Region), "Order"])
order_NAM <- unique(NMDS[grepl("North America", NMDS$Region), "Order"])
order_AUS <- unique(NMDS[grepl("Australia", NMDS$Region), "Order"])

order_names <- Reduce(intersect, list(order_EUR, order_NAM, order_AUS))
NMDS <- NMDS[NMDS$Order %in% order_names, ]

# Save as .csv
write.table(NMDS, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_bin_final.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
MCA <- read.table(file.path(path, "final", "macroinvertebrate_ALL_mod_fancy.csv"), sep = ",", row.names = 1, header = TRUE)


# --------------------------------------------------------------------------------------------------------------- #
#### Preparation ####
names(MCA)

# Low number of families in all databases
# Check for Order
order_EUR <- unique(MCA[grepl("Europe", MCA$Region), "Order"])
order_NAM <- unique(MCA[grepl("North America", MCA$Region), "Order"])
order_AUS <- unique(MCA[grepl("Australia", MCA$Region), "Order"])

order_names <- Reduce(intersect, list(order_EUR, order_NAM, order_AUS))
MCA <- MCA[MCA$Order %in% order_names, ]

# Select those names from dataframes
# MCA <- MCA[MCA$Family %in% names, ]

# Save as .csv
write.table(MCA, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_mod_final.csv", sep = ",")


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
SOM <- read.table(file.path(path, "final", "macroinvertebrate_ALL_int.csv"), sep = ",", row.names = 1, header = TRUE)


# --------------------------------------------------------------------------------------------------------------- #
#### Preparation ####
names(SOM)

# Low number of families in all databases
# Check for Order
order_EUR <- unique(SOM[grepl("EUR", SOM$region), "order"])
order_NAM <- unique(SOM[grepl("NAM", SOM$region), "order"])
order_AUS <- unique(SOM[grepl("AUS", SOM$region), "order"])

order_names <- Reduce(intersect, list(order_EUR, order_NAM, order_AUS))
SOM <- SOM[SOM$order %in% order_names, ]

# Save as .csv
write.table(SOM, file = "~/Schreibtisch/Thesis/data/final/macroinvertebrate_ALL_int_final.csv", sep = ",")
