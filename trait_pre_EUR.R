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


# ---- Voltinism ----
grep("volt", names(df_EUR), ignore.case = TRUE, value = TRUE)

# volt_1: volt_semi
# volt_2: volt_uni
# volt_3: volt_bi + volt_tri + volt_multi
# QUESTION: What happens to volt_flex

df_EUR <- df_EUR %>%
  mutate(volt_3 = ifelse(is.na(volt_bi), volt_tri, volt_bi),
         volt_3 = ifelse(is.na(volt_bi), volt_multi, volt_bi)) %>%
  mutate(volt_1 = ifelse(!is.na(volt_semi), 1, NA),
         volt_2 = ifelse(!is.na(volt_uni), 1, NA),
         volt_3 = ifelse(!is.na(volt_bi), 1, NA)) %>%
  select(-(volt_semi:volt_flex))

# ------------------------------------------------------------------------------------------------------------------------- #
#### Final Table ####
# Write .csv
write.table(df_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR.csv", sep = ",")
