#### Preparation: European Database  ####
# ---- Trait Information Preprocessing ----

# ------------------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# ------------------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR_complete.csv"), stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Query Traits ####
names(df_EUR)

# Remove unnecessary columns
df_EUR <- select(df_EUR, -(si.1:si.5))


# ------------------------------------------------------------------------------------------------------------------------- #
#### Final Table ####
# Write .csv
write.table(df_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR.csv", sep = ",")
