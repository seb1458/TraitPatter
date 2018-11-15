#########################
#### NA Error Script ####
#########################

#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


#### Packages ####
library(readxl)


#### Load data ####
df_AUS <- read_excel(file.path(path, "Australia", "Australian macroinv trait database.xlsx"), sheet = 1)

# Replace NULL, N/A, "NA" with real NAs
df_AUS[df_AUS == "NULL"] <- NA
df_AUS[df_AUS == "N/A"] <- NA
df_AUS[df_AUS == "NA"] <- NA


#### Query Data ####
# E.g. long_code
df_AUS[df_AUS$long_code == "IB010199", ]

# A lot of rows with all NAs in taxa information column
# NAs could also be the queried code


#### Remove all-NA rows ####
# No identification of species possible if no taxon information and no code
noNA_df <- df_AUS[!(rowSums(is.na(df_AUS[1:4])) == ncol(df_AUS[1:4])), ] 


#### Query Data w/o NAs ####
# Same code as before
noNA_df[noNA_df$long_code == "IB010199", ]

# Problem: NAs in noNA_df$long_code does not mean, that no identification is possible
# Only if noNA_df$long_code AND noNA_df$Family are NA there is no identification possible

final <- noNA_df[!(is.na(noNA_df$long_code) & is.na(noNA_df$Family)), ]
final[final$long_code == "IB010199", ]
# Still NAs left