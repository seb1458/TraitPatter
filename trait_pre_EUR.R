#------------------------------------#
#--- Preparation: Europe database ---#
#------------------------------------#

# ------------------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# ------------------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR.csv"), stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Query Traits ####
names(df_EUR)

# ---- Voltinism ----
# Levels = What???



df_EUR <- mutate(df_EUR,
                 oxy_normal = ifelse(df_EUR$xenosaprobic + df_EUR$oligosaprobic >= df_EUR$b.mesosaprobic + df_EUR$a.mesosaprobic + df_EUR$polysaprobic, 1, 0),
                 oxy_low = ifelse(df_EUR$xenosaprobic + df_EUR$oligosaprobic <= df_EUR$b.mesosaprobic + df_EUR$a.mesosaprobic + df_EUR$polysaprobic, 1, 0))