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
df_EUR <- na_if(df_EUR, 0)


# ------------------------------------------------------------------------------------------------------------------------- #
#### Query Traits ####
names(df_EUR)

# Remove unnecessary columns
df_EUR <- select(df_EUR, -(si.1:si.5))

# ---- Size ----
# Information of Tachet is transfered to EU DB
tachet <- read.csv(file.path(path, "Europe", "Tachet_mod_csv.csv"), stringsAsFactors = FALSE)[, c(1:14)]

# Merge df_EUR with size information from Tachet
EU_size <- df_EUR[, 3:5]

# 1. Merge on species level
EU_size <- merge(EU_size, tachet, by.x = "Taxon", by.y = "taxa", all.x =  TRUE)

# 2. Merge on genus level
EU_size <- merge(EU_size, tachet, by.x = "genus.x", by.y = "taxa", all.x =  TRUE)

# 3. Combine size columns and add to df_EU
EU_size <- EU_size %>%
  transmute(size_1 = coalesce(size_1.x, size_1.y),
            size_2 = coalesce(size_2.x, size_2.y),
            size_3 = coalesce(size_3.x, size_3.y),
            size_4 = coalesce(size_4.x, size_4.y),
            size_5 = coalesce(size_5.x, size_5.y),
            size_6 = coalesce(size_6.x, size_6.y),
            size_7 = coalesce(size_7.x, size_7.y))

df_EUR <- select(df_EUR, -(size_1:size_7))
df_EUR <- cbind(df_EUR, EU_size)


# ---- Voltinism ----
grep("volt", names(df_EUR), ignore.case = TRUE, value = TRUE)

# volt_1: volt_semi
# volt_2: volt_uni
# volt_3: volt_bi + volt_tri + volt_multi
# QUESTION: What happens to volt_flex

df_EUR <- df_EUR %>%
  mutate(volt_1 = volt_semi,
         volt_2 = volt_uni,
         volt_3 = coalesce(volt_bi, volt_tri, volt_multi)) %>%
  mutate(volt_1 = ifelse(!is.na(volt_1), 1, NA),
         volt_2 = ifelse(!is.na(volt_2), 1, NA),
         volt_3 = ifelse(!is.na(volt_3), 1, NA)) %>%
  select(-(volt_semi:volt_flex))


# ------------------------------------------------------------------------------------------------------------------------- #
#### Final Table ####

# Write .csv
write.table(df_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR_trait.csv", sep = ",")
