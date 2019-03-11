#########################################
#### Preparation: European Database  ####
#########################################
# ---- Trait Information Preprocessing ----

# --------------------------------------------------------------------------------------------------------------- #
#### Working directory ####
path <- "~/Schreibtisch/Thesis/data"


# --------------------------------------------------------------------------------------------------------------- #
#### Packages ####
library(tidyverse)


# --------------------------------------------------------------------------------------------------------------- #
#### Load Data ####
df_EUR <- read.csv(file.path(path, "Europe", "macroinvertebrate_EUR_complete.csv"), stringsAsFactors = FALSE)
df_EUR <- na_if(df_EUR, 0)

names(df_EUR)

# Remove unnecessary traity
df_EUR <- select(df_EUR, -c(si.1:si.5))

# --------------------------------------------------------------------------------------------------------------- #
#### Add Information from Tachet ####
# Load Tachet DB
tachet <- read.csv(file.path(path, "Europe", "Tachet_mod.csv"), stringsAsFactors = FALSE)
tachet <- na_if(tachet, 0)

names(tachet)

# Modify pH preference
tachet <- tachet %>%
  mutate(ph_acidic_t = coalesce(Ph_4_t, Ph_445_t, Ph_455_t, Ph_555_t, Ph_556_t, Ph_6_t),
         ph_neutral_t = NA) %>%
  select(-c(Ph_4_t:Ph_6_t))

# Merge EU DB and Tachet DB
# ---- Species level ----
df_EUR <- merge(df_EUR, tachet, by.x = "species", by.y = "Species", all.x = TRUE)

# Extend information with entries from Tachet
df_EUR <- df_EUR %>%
  mutate(volt_semi = ifelse(is.na(volt_semi), volt1_t, volt_semi),
         volt_uni = ifelse(is.na(volt_uni), volt2_t, volt_uni),
         volt_bi = ifelse(is.na(volt_bi), volt3_t, volt_bi)) %>%
  
  mutate(stage_egg = ifelse(is.na(stage_egg), stage_egg_t, stage_egg),
         stage_larva = ifelse(is.na(stage_larva), stage_larva_t, stage_larva),
         stage_adult = ifelse(is.na(stage_adult), stage_adult_t, stage_adult)) %>%
  
  mutate(rep_ovovipar = ifelse(is.na(rep_ovovipar), rep_ovo_t, rep_ovovipar),
         rep_egg_free_iso = ifelse(is.na(rep_egg_free_iso), rep_eggs_freeiso_t, rep_egg_free_iso),
         rep_egg_cem_iso = ifelse(is.na(rep_egg_cem_iso), rep_egg_isocem_t, rep_egg_cem_iso),
         rep_clutch_fixed = ifelse(is.na(rep_clutch_fixed), rep_clutch_fix_t, rep_clutch_fixed),
         rep_clutch_free = ifelse(is.na(rep_clutch_free), rep_clutch_free_t, rep_clutch_free),
         rep_clutch_veg = ifelse(is.na(rep_clutch_veg), rep_clutch_veg_t, rep_clutch_veg),
         rep_clutch_ter = ifelse(is.na(rep_clutch_ter), rep_clutch_terr_t, rep_clutch_ter),
         rep_asexual = ifelse(is.na(rep_asexual), rep_asexual_t, rep_asexual)) %>%
  
  mutate(dissem_aq_passive = ifelse(is.na(dissem_aq_passive), disp_aq_passive_t, dissem_aq_passive),
         dissem_aq_active = ifelse(is.na(dissem_aq_active), disp_aq_active_t, dissem_aq_active),
         dissem_air_passive = ifelse(is.na(dissem_air_passive), disp_air_passive_t, dissem_air_passive),
         dissem_air_active = ifelse(is.na(dissem_air_active), disp_air_active_t, dissem_air_active)) %>%
  
  mutate(resp_teg = ifelse(is.na(resp_teg), resp_tegument_t, resp_teg),
         resp_gil = ifelse(is.na(resp_gil), resp_gill_t, resp_gil),
         resp_pls = ifelse(is.na(resp_pls), resp_plastron_t, resp_pls),
         resp_spi = ifelse(is.na(resp_spi), resp_spiracle_t, resp_spi),
         resp_ves = ifelse(is.na(resp_ves), resp_vesicle_t, resp_ves)) %>%
  
  mutate(locom_swim_dive = ifelse(is.na(locom_swim_dive), loc_swimmer_surf_t, locom_swim_dive),
         locom_swim_dive = ifelse(is.na(locom_swim_dive), loc_swimmer_full_t, locom_swim_dive),
         locom_burrow = ifelse(is.na(locom_burrow), loc_burrower_t, locom_burrow),
         locom_sessil = ifelse(is.na(locom_sessil), loc_att_temp_t, locom_sessil),
         locom_sessil = ifelse(is.na(locom_sessil), loc_att_perm_t, locom_sessil)) %>%
  
  mutate(feed_grazer = ifelse(is.na(feed_grazer), feed_scraper_t, feed_grazer),
         feed_shred = ifelse(is.na(feed_shred), feed_shredder_t, feed_shred),
         feed_gath = ifelse(is.na(feed_gath), feed_deposit_t, feed_gath),
         feed_active_filter = ifelse(is.na(feed_active_filter), feed_filter_t, feed_active_filter),
         feed_active_filter = ifelse(is.na(feed_active_filter), feed_abs_t, feed_active_filter),
         feed_predator = ifelse(is.na(feed_predator), feed_predator_t, feed_predator),
         feed_parasite = ifelse(is.na(feed_parasite), feed_parasite_t, feed_parasite)) %>%
  
  mutate(temp_cold = ifelse(is.na(temp_cold), temp_cold_t, temp_cold),
         temp_warm = ifelse(is.na(temp_warm), temp_warm_t, temp_warm),
         temp_eurytherm = ifelse(is.na(temp_eurytherm), temp_eury_t, temp_eurytherm)) %>%
  
  mutate(ph_acidic = ifelse(is.na(ph_acidic), ph_acidic_t, ph_acidic),
         ph_neutral_alk = ifelse(is.na(ph_neutral_alk), ph_neutral_t, ph_neutral_alk)) %>%
  
  select(-Group, -Family, -Subfamily, -Genus, -Taxa , -grep("_t$", names(df_EUR))
  )


# ---- Genus level ----
tachet_sub <- tachet[grepl("sp\\.$", tachet$Species), ]

df_EUR <- merge(df_EUR, tachet_sub, by.x = "genus", by.y = "Genus", all.x = TRUE)

df_EUR <- df_EUR %>%
  mutate(volt_semi = ifelse(is.na(volt_semi), volt1_t, volt_semi),
         volt_uni = ifelse(is.na(volt_uni), volt2_t, volt_uni),
         volt_bi = ifelse(is.na(volt_bi), volt3_t, volt_bi)) %>%
  
  mutate(stage_egg = ifelse(is.na(stage_egg), stage_egg_t, stage_egg),
         stage_larva = ifelse(is.na(stage_larva), stage_larva_t, stage_larva),
         stage_adult = ifelse(is.na(stage_adult), stage_adult_t, stage_adult)) %>%
  
  mutate(rep_ovovipar = ifelse(is.na(rep_ovovipar), rep_ovo_t, rep_ovovipar),
         rep_egg_free_iso = ifelse(is.na(rep_egg_free_iso), rep_eggs_freeiso_t, rep_egg_free_iso),
         rep_egg_cem_iso = ifelse(is.na(rep_egg_cem_iso), rep_egg_isocem_t, rep_egg_cem_iso),
         rep_clutch_fixed = ifelse(is.na(rep_clutch_fixed), rep_clutch_fix_t, rep_clutch_fixed),
         rep_clutch_free = ifelse(is.na(rep_clutch_free), rep_clutch_free_t, rep_clutch_free),
         rep_clutch_veg = ifelse(is.na(rep_clutch_veg), rep_clutch_veg_t, rep_clutch_veg),
         rep_clutch_ter = ifelse(is.na(rep_clutch_ter), rep_clutch_terr_t, rep_clutch_ter),
         rep_asexual = ifelse(is.na(rep_asexual), rep_asexual_t, rep_asexual)) %>%
  
  mutate(dissem_aq_passive = ifelse(is.na(dissem_aq_passive), disp_aq_passive_t, dissem_aq_passive),
         dissem_aq_active = ifelse(is.na(dissem_aq_active), disp_aq_active_t, dissem_aq_active),
         dissem_air_passive = ifelse(is.na(dissem_air_passive), disp_air_passive_t, dissem_air_passive),
         dissem_air_active = ifelse(is.na(dissem_air_active), disp_air_active_t, dissem_air_active)) %>%
  
  mutate(resp_teg = ifelse(is.na(resp_teg), resp_tegument_t, resp_teg),
         resp_gil = ifelse(is.na(resp_gil), resp_gill_t, resp_gil),
         resp_pls = ifelse(is.na(resp_pls), resp_plastron_t, resp_pls),
         resp_spi = ifelse(is.na(resp_spi), resp_spiracle_t, resp_spi),
         resp_ves = ifelse(is.na(resp_ves), resp_vesicle_t, resp_ves)) %>%
  
  mutate(locom_swim_dive = ifelse(is.na(locom_swim_dive), loc_swimmer_surf_t, locom_swim_dive),
         locom_swim_dive = ifelse(is.na(locom_swim_dive), loc_swimmer_full_t, locom_swim_dive),
         locom_burrow = ifelse(is.na(locom_burrow), loc_burrower_t, locom_burrow),
         locom_sessil = ifelse(is.na(locom_sessil), loc_att_temp_t, locom_sessil),
         locom_sessil = ifelse(is.na(locom_sessil), loc_att_perm_t, locom_sessil)) %>%
  
  mutate(feed_grazer = ifelse(is.na(feed_grazer), feed_scraper_t, feed_grazer),
         feed_shred = ifelse(is.na(feed_shred), feed_shredder_t, feed_shred),
         feed_gath = ifelse(is.na(feed_gath), feed_deposit_t, feed_gath),
         feed_active_filter = ifelse(is.na(feed_active_filter), feed_filter_t, feed_active_filter),
         feed_active_filter = ifelse(is.na(feed_active_filter), feed_abs_t, feed_active_filter),
         feed_predator = ifelse(is.na(feed_predator), feed_predator_t, feed_predator),
         feed_parasite = ifelse(is.na(feed_parasite), feed_parasite_t, feed_parasite)) %>%
  
  mutate(temp_cold = ifelse(is.na(temp_cold), temp_cold_t, temp_cold),
         temp_warm = ifelse(is.na(temp_warm), temp_warm_t, temp_warm),
         temp_eurytherm = ifelse(is.na(temp_eurytherm), temp_eury_t, temp_eurytherm)) %>%
  
  mutate(ph_acidic = ifelse(is.na(ph_acidic), ph_acidic_t, ph_acidic),
         ph_neutral_alk = ifelse(is.na(ph_neutral_alk), ph_neutral_t, ph_neutral_alk)) %>%
  
  select(-Group, -Family, -Subfamily, -Species, -Taxa , -grep("_t$", names(df_EUR))) %>%
  rename(size_1 = Size_1.y, size_2 = Size_2.y, size_3 = Size_3.y, size_4 = Size_4.y,
         size_5 = Size_5.y, size_6 = Size_6.y, size_7 = Size_7.y) %>%
  select(order, family, genus, species, everything(), -starts_with("Size", ignore.case = FALSE))

# Modify voltinism column
df_EUR <- df_EUR %>%
  mutate(volt_1 = volt_semi,
         volt_2 = volt_uni,
         volt_3 = coalesce(volt_bi, volt_tri, volt_multi, volt_flex)) %>%
  mutate(volt_1 = ifelse(is.na(volt_1), NA, 1),
         volt_2 = ifelse(is.na(volt_2), NA, 1),
         volt_3 = ifelse(is.na(volt_3), NA, 1)) %>%
  select(-grep("volt_", names(df_EUR)))

# Modify reproduction column
# At the moment: Which states appear? But has to be coaslesced into one column

df_EUR <- df_EUR %>%
  mutate(stage_larva = coalesce(stage_larva, stage_nymph)) %>%
  select(-stage_nymph) %>%
  mutate(stage_egg.new = ifelse(!is.na(stage_egg) & is.na(stage_larva) & is.na(stage_pupa) & is.na(stage_adult), 1, NA),
         stage_larva.new = ifelse(!is.na(stage_egg) & !is.na(stage_larva) & is.na(stage_pupa) & is.na(stage_adult), 1, NA),
         stage_pupa.new = ifelse(!is.na(stage_egg) & !is.na(stage_larva) & !is.na(stage_pupa) & is.na(stage_adult), 1, NA),
         stage_adult.new = ifelse(!is.na(stage_egg) & !is.na(stage_larva) & !is.na(stage_pupa) & !is.na(stage_adult), 1, NA)) %>%
  select(-c(stage_egg:stage_adult)) %>%
  rename(stage_egg = stage_egg.new, stage_larva = stage_larva.new, stage_pupa = stage_pupa.new, stage_adult = stage_adult.new)
  

# --------------------------------------------------------------------------------------------------------------- #
#### Final Table ####

# Write .csv
write.table(df_EUR, file = "~/Schreibtisch/Thesis/data/Europe/macroinvertebrate_EUR_trait.csv", sep = ",")
